;;; docs: http://wiki.call-cc.org/eggref/4/ncurses

; thoughts:
; * could use a sparse array for large files? sparse strings for long lines?
; * maybe pipe out to external command for syntax highlighting?
; * extend vim grammar to include multiple cursors? or maybe just use regex with search.

(use ncurses srfi-1 srfi-13 regex prometheus vector-lib extras ports)

;;; consts that aren't in ncurses

(define KEY_ESCAPE (integer->char 27))
(define KEY_ALT_BACKSPACE (integer->char 127))
(define KEY_LINEFEED #\newline) ; enter key

;;; general util - because this crap isn't implemented in the stdlib???

(define (string-insert s t i) (string-replace s t i i))
(define (string-remove s i)
  (if (<= i 0)
    #f
    (string-replace s "" (- i 1) i)))

(define (clamp low up val)
  (max low (min up val)))

(define (for-n start stop fn)
  (map fn (iota (- stop start) start)))

(define (vector-delete v i)
  (vector-append (vector-copy v 0 i)
                 (if (< i (- (vector-length v) 1))
                   (vector-copy v (+ i 1))
                   #())))

(define (vector-insert v n i) ; TODO: dedup this
  (vector-append (vector-copy v 0 i)
                 (vector n)
                 (if (<= i (- (vector-length v) 1))
                   (vector-copy v i)
                   #())))

(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

(define (read-to-vector filename)
  (with-input-from-file filename
                        (lambda ()
                          (port-fold
                            (lambda (l v)
                              (vector-append v (vector l)))
                            #()
                            read-line))))

(define (write-from-vector vector filename)
  (with-output-to-file filename
                       (lambda ()
                         (vector-for-each
                           (lambda (i e)
                             (display e)
                             (newline))
                           vector))))

;;; more specialized util

(define-syntax bind-textobj!
  (syntax-rules ()
    ((bind-textobj! <mode> <key> <body> ...)
     (<mode> 'bind-textobj! <key> (eval '(lambda (cursor ch) <body> ...))))))

(define-syntax bind! ; bind a key to a command
  (syntax-rules ()
    ((bind! <mode> <key> <body> ...)
     (<mode> 'bind! <key> (eval '(lambda (cursor ch) <body> ...)))))) ; yucky eval to break hygenic macros

(define-syntax bind-motion!
  (syntax-rules ()
    ((bind-motion! <mode> <key> <direct-expr> <chain-expr>)
     (begin (bind! <mode> <key> <direct-expr>)
            (bind-textobj! <mode> <key> <chain-expr>)))
    ((bind-motion! <mode> <key> <expr>)
     (bind-motion! <mode> <key> <expr> <expr>))))

(define-syntax with-do-motion
  (syntax-rules ()
    ((with-do-motion <char> <cursor> <range-binding> <expr>)
     (let ((<range-binding> (make-range <cursor> (command-mode 'get-bind <char> #t) <cursor> #f)))
       <expr>))))

(define (proper-line line) ; substitute weird characters out of a line
  (string-substitute "\t" tabstring line #t))

(define (make-range cursor move-thunk . args)
  (let ((start ((cursor 'p) 'really-clone)))
    (apply move-thunk args)
    (let ((end ((cursor 'p) 'really-clone)))
      (if (start 'eq? end)
        #f
        (begin
          (if (end '< start)
            (swap! start end))
          `(,start ,end ,cursor))))))

(define (draw-init) ; initialize ncurses and stuff
  (initscr)
  (cbreak)
  ;(raw) ; catch ctrl-c
  (noecho))

(define (draw-finalize) ; clean up drawing stuff
  (endwin))

(define (draw-cursor cursor #!optional altstr) ; TODO: actually draw a block so this works with multiple cursors
  (let ((str
          (if altstr
            altstr
            (cursor 'cur-line-s))))
    (wmove (stdscr)
           (- (cursor 'line) ((cursor 'buffer) 'scroll))
           (cursor 'proper-line-pos str))))

(define (draw-line str i #!optional dirty)
  (wmove (stdscr) i 0)
  (if dirty
    (wclrtoeol (stdscr)))
  (waddstr (stdscr)
           (if str
             (proper-line str)
             "~")))

(define (draw-buf buf)
  (if dirty-screen
    (wclear (stdscr))
    (set! dirty-screen '()))
  (for-n 0 (LINES)
         (lambda (i)
           (draw-line (buf 'get-line (+ (buf 'scroll) i)) i)))
  (draw-cursor (car (buf 'cursors)))
  (wrefresh (stdscr)))

(define (scroll-to cursor)
  (let ((buf (cursor 'buffer)))
    (cond
      ((>= (cursor 'line) (+ (buf 'scroll) (LINES)))
       (buf 'set-scroll! (+ (buf 'scroll) 1)))
      ((< (cursor 'line) (buf 'scroll))
       (buf 'set-scroll! (cursor 'line))))))

(define (get-char)
  (wgetch (stdscr)))

;;;

(define tabstop 3)
(define tabstring (list->string (make-list tabstop #\space)))
(define dirty-screen #f) ; do we need to redraw? this is an ugly to set! on purpose.

(define-object *pos* (*the-root-object*) ; xy pair
               (x setx! 0)
               (y sety! 0)

               ((inc! self resend axis #!optional amount)
                (let ((v (if amount amount 1)))
                  (self (symbol-append 'set axis '!) (+ (self axis) v))))

               ((dec! self resend axis #!optional amount)
                (let ((v (if amount amount 1)))
                  (self (symbol-append 'set axis '!) (- (self axis) v))))

               ((really-clone self resend) ; heh
                (let ((r (self 'clone)))
                  (r 'setx! (r 'x)) ; if we don't do this, 'x points at the parent slot and will update with it
                  (r 'sety! (r 'y)) ; same for y
                  r))

               ((eq? self resend other)
                (and (= (self 'y) (other 'y))
                     (= (self 'x) (other 'x))))

               ((< self resend other)
                (or (< (self 'y) (other 'y))
                    (and (= (self 'y) (other 'y))
                         (< (self 'x) (other 'x)))))

               ((make self resend x y)
                (let ((r (*pos* 'clone)))
                  (r 'setx! x)
                  (r 'sety! y)
                  r)))

(define-object *mode* (*the-root-object*)
               (bindings set-bindings! '()) ; key bindings
               (textobj-bindings set-textobj-bindings! '()) ; text object bindings (not accessible unless in op chain)
               (post-thunk set-post-thunk! (lambda (cursor) (void))) ; thunk called directly before drawing every loop (use for cleanup)

               ((bind! self resend k thunk) ; bind a thunk (thunk gets passed a cursor)
                (self 'set-bindings! (append (self 'bindings) `(,(cons k thunk)))))

               ((bind-textobj! self resend k thunk) ; bind a thunk (thunk gets passed a cursor)
                (self 'set-textobj-bindings! (append (self 'textobj-bindings) `(,(cons k thunk)))))

               ((get-bind self resend k #!optional text-obj)
                (let ((p (assoc k (self (if text-obj
                                          'textobj-bindings
                                          'bindings)))))
                  (cdr (if p p (assoc 'else (self 'bindings)))))))

(define-object *buf* (*the-root-object*)
               (cursors set-cursors! '())
               (lines set-lines! #(""))
               (scroll set-scroll! 0)

               ((load-file self resend filename)
                (self 'set-lines! (read-to-vector filename)))

               ((get-line self resend i)
                (if (<= i (self 'last-line))
                  (vector-ref (self 'lines) i)
                  #f))

               ((replace-line self resend new-line line-i)
                (vector-set! (self 'lines) line-i new-line))

               ((delete-line self resend line-i)
                (if (= (self 'last-line) 0) ; edge-case: only one line in buffer
                  (self 'replace-line "" 0)
                  (self 'set-lines! (vector-delete (self 'lines) line-i)))
                (set! dirty-screen #t))

               ((delete self resend range)
                (if range
                  (let* ((start (car range))
                         (end (cadr range)))
                    (if (= (start 'y) (end 'y))
                      (self 'replace-line (string-replace (self 'get-line (start 'y)) "" (start 'x) (end 'x)) (start 'y)))
                    ((caddr range) 'go start))
                  (set! dirty-screen #t)))

               ((insert-line self resend new-line line-i)
                (self 'set-lines! (vector-insert (self 'lines) new-line line-i))
                (set! dirty-screen #t))

               ((last-line self resend)
                (- (vector-length (self 'lines)) 1)))

(define-object *cursor* (*the-root-object*)
               (buffer set-buffer! '())
               (p setp! (*pos* 'clone))

               ((line self resend)
                ((self 'p) 'y))

               ((line-pos self resend)
                ((self 'p) 'x))

               ((insert-line self resend dir)
                (if (equal? 'next dir)
                  (begin
                    ((self 'buffer) 'insert-line "" (+ (self 'line) 1))
                    (self 'm-next-line))
                  ((self 'buffer) 'insert-line "" (self 'line))))

               ((replace-cur-line self resend new-str)
                ((self 'buffer) 'replace-line
                                new-str
                                (self 'line)))

               ((cur-line-s self resend)
                ((self 'buffer) 'get-line (self 'line)))

               ((cur-char self resend)
                (if (self 'p-eol?)
                  #\space ; WARNING: I didn't think this bit through. It might not work.
                  (string-ref (self 'cur-line-s) (self 'line-pos))))

               ((prev-char self resend)
                (if (> (self 'line-pos) 0)
                  (string-ref (self 'cur-line-s) (- (self 'line-pos) 1))))

               ((proper-line-pos self resend line)
                (string-length
                  (proper-line (string-take line (self 'line-pos)))))

               ((begin-word? self resend)
                (and
                  (or (self 'bol?)
                      (char-whitespace? (self 'prev-char)))
                  (not (char-whitespace? (self 'cur-char)))))

               ((eol? self resend)
                (>= (self 'line-pos) (- (string-length (self 'cur-line-s)) 1)))

               ((p-eol? self resend)
                (>= (self 'line-pos) (string-length (self 'cur-line-s))))

               ((bol? self resend)
                (<= (self 'line-pos) 0))

               ((m-prev-line self resend)
                (if (> (self 'line) 0)
                  ((self 'p) 'sety! (- (self 'line) 1))
                  #f))

               ((m-next-line self resend)
                (if (< (self 'line) ((self 'buffer) 'last-line))
                  ((self 'p) 'sety! (+ (self 'line) 1))
                  #f))

               ((%next-char self resend)
                ((self 'p) 'inc! 'x))

               ((%prev-char self resend)
                ((self 'p) 'dec! 'x))

               ((m-next-char self resend)
                (if (self 'p-eol?)
                  #f
                  (self '%next-char)))

               ((m-prev-char self resend)
                (if (self 'bol?)
                  #f
                  (self '%prev-char)))

               ((m-eol self resend)
                ((self 'p) 'setx! (string-length (self 'cur-line-s))))

               ((m-bol self resend)
                ((self 'p) 'setx! 0))

               ((m-word self resend dir)
                (call/cc
                  (lambda (break)
                    (if (self 'begin-word?) ; bump past beginning of word if we're currently on one
                      (if (not (self (symbol-append 'm- dir '-char)))
                        (break #f)))
                    (let loop ()
                      (if (not (self 'begin-word?))
                        (begin (if (not (self (symbol-append 'm- dir '-char)))
                                 (break #f))
                               (loop))
                        (break #t))))))

               ((scroll-to-top self resend)
                ((self 'p) 'sety! ((self 'buffer) 'scroll)))

               ((scroll-to-bottom self resend)
                ((self 'p) 'sety! (+ ((self 'buffer) 'scroll) (LINES) -1)))

               ((clamp-to-line self resend)
                (if (> (self 'line) ((self 'buffer) 'last-line)) ; catch when cursor goes over edge (i.e. due to deletion of last line)
                  ((self 'p) 'sety! ((self 'buffer) 'last-line)))
                ((self 'p) 'setx!
                           (clamp 0 (- (string-length (self 'cur-line-s)) 1)
                                  (self 'line-pos))))

               ((go self resend pos)
                (self 'setp! pos))

               ((new self resend buf)
                (let ((c (self 'clone)))
                  (c 'set-buffer! buf)
                  c))

               ((backspace self resend #!optional delete-line) ; helper function to backspace
                (let ((s (string-remove (self 'cur-line-s) (self 'line-pos))))
                  (if s
                    (begin (self 'replace-cur-line s)
                           (self '%prev-char))
                    (if delete-line
                      (begin ((self 'buffer) 'delete-line (self 'line))
                             (self 'm-prev-line)
                             (self 'm-eol)))))))

;;;

(*buf* 'set-cursors! (list (*cursor* 'new *buf*)))

(define command-mode (*mode* 'clone))
(define insert-mode (*mode* 'clone))
(define cur-mode command-mode)

;;; INSERT MODE

(bind! insert-mode KEY_ESCAPE (set! cur-mode command-mode))
(bind! insert-mode KEY_BACKSPACE (cursor 'backspace #t))
(bind! insert-mode KEY_ALT_BACKSPACE ((insert-mode 'get-bind KEY_BACKSPACE) cursor ch)) ; alias
(bind! insert-mode KEY_LINEFEED (cursor 'insert-line 'next) (cursor 'm-bol))
(bind! insert-mode 'else
       (cursor 'replace-cur-line
               (string-insert (cursor 'cur-line-s)
                              (string ch)
                              (cursor 'line-pos)))
       (cursor '%next-char))

;;; COMMAND MODE

(command-mode 'set-post-thunk! (lambda (cursor)
                                 (scroll-to cursor)
                                 (cursor 'clamp-to-line)))
(bind! command-mode #\i (set! cur-mode insert-mode))
(bind-motion! command-mode #\h (cursor 'm-prev-char))
(bind-motion! command-mode #\j (cursor 'm-next-line))
(bind-motion! command-mode #\k (cursor 'm-prev-line))
(bind-motion! command-mode #\l (cursor 'm-next-char))
(bind-motion! command-mode #\$ (cursor 'm-eol))
(bind-motion! command-mode #\0 (cursor 'm-bol))
(bind-motion! command-mode #\w
              (if (not (cursor 'm-word 'next))
                (if (cursor 'm-next-line)
                  (begin (cursor 'm-bol) (cursor 'm-word 'next))))
              (cursor 'm-word 'next))
(bind-motion! command-mode #\b
              (if (not (cursor 'm-word 'prev))
                (if (cursor 'm-prev-line)
                  (begin (cursor 'm-eol) (cursor 'm-word 'prev))))
              (cursor 'm-word 'prev))
(bind! command-mode #\X (cursor 'backspace))
(bind! command-mode #\x (cursor 'm-next-char) (cursor 'backspace))
(bind! command-mode #\O
       (cursor 'insert-line 'prev)
       (cursor 'm-bol)
       (set! cur-mode insert-mode))
(bind! command-mode #\o
       (cursor 'insert-line 'next)
       (cursor 'm-bol)
       (set! cur-mode insert-mode))
(bind! command-mode #\d
       (let ((cch (get-char)))
         (if (equal? ch cch)
           ((cursor 'buffer) 'delete-line (cursor 'line))
           (with-do-motion cch cursor range
                           ((cursor 'buffer) 'delete range)))))
(bind! command-mode #\c
       ((command-mode 'get-bind #\d) cursor #\c)
       (set! cur-mode insert-mode))
(bind! command-mode #\a
       (cursor 'm-next-char)
       (set! cur-mode insert-mode))
(bind! command-mode #\: (write-from-vector ((cursor 'buffer) 'lines) "pepout"))
(bind! command-mode #\H (cursor 'scroll-to-top))
(bind! command-mode #\L (cursor 'scroll-to-bottom))
(bind! command-mode 'else (void))

;;; MAIN LOOP

(let ((args (command-line-arguments)))
  (if (not (null? args))
    (*buf* 'load-file (car args))))

(draw-init)
(draw-buf *buf*) ; FIXME: UGLY: initial draw
(let loop ()
  (let ((c (get-char)) (main-cursor (car (*buf* 'cursors))))
    ((cur-mode 'get-bind c) main-cursor c)
    ((cur-mode 'post-thunk) main-cursor)
    (draw-buf *buf*))
  (loop))
