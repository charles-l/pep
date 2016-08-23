;;; docs: http://wiki.call-cc.org/eggref/4/ncurses

; thoughts:
; * could use a sparse array for large files? sparse strings for long lines?
; * maybe pipe out to external command for syntax highlighting?
; * extend vim grammar to include multiple cursors? or maybe just use regex with search.
; * make modes have their own draw functions

(use ncurses srfi-1 srfi-13 regex prometheus vector-lib)

;;; consts that aren't in ncurses

(define KEY_ESCAPE (integer->char 27))
(define KEY_ALT_BACKSPACE (integer->char 127))

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

(define-syntax swap
  (syntax-rules ()
                ((swap x y)
                 (let ((tmp x))
                   (set! x y)
                   (set! y tmp)))))

;;; more specialized util

(define-syntax bind! ; bind a key to a command
  (syntax-rules ()
                ((bind! <mode> <type> <key> <body> ...) ; WARNING: gonna break hygenic macros for a second...
                 (<mode> 'bind! <type> <key> (eval '(lambda (cursor ch) <body> ...)))))) ; ... EWWWWW! YUCKY EVAL!

(define (proper-line line) ; substitute weird characters out of a line
  (string-substitute "\t" tabstring line #t))

(define (make-range cursor move-thunk . args)
  (let ((start `(,((cursor 'p) 'clone))))
    (endwin)
    (print ((car start) 'x))
    (print ((car start) 'y))
    (apply move-thunk args)
    (print ((car start) 'x))
    (print ((car start) 'y))
    (append (start `(,((cursor 'p) 'clone))))))

(define (draw-init) ; initialize ncurses and stuff
  (initscr)
  (cbreak)
  (noecho))

(define (draw-finalize) ; clean up drawing stuff
  (endwin))

(define (draw-cursor cursor #!optional altstr) ; TODO: actually draw a block so this works with multiple cursors
  (let ((str
          (if altstr
            altstr
            (cursor 'cur-line-s))))
    (wmove (stdscr)
           (cursor 'line)
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

(define (get-char)
  (wgetch (stdscr)))

;;;

(define tabstop 3)
(define tabstring (list->string (make-list tabstop #\space)))
(define dirty-screen #f) ; do we need to redraw? this is an ugly to set! on purpose.

(define *pos* (*the-root-object* 'clone)) ; xy pair
(*pos* 'add-value-slot! 'x 'setx! 0)
(*pos* 'add-value-slot! 'y 'sety! 0)

(define-method (*pos* 'inc! self resend axis #!optional amount)
               (let ((v (if amount amount 1)))
                 (self (symbol-append 'set axis '!) (+ (self axis) v))))

(define-method (*pos* 'dec! self resend axis #!optional amount)
               (let ((v (if amount amount 1)))
                 (self (symbol-append 'set axis '!) (- (self axis) v))))

(define-method (*pos* 'make self resend x y)
               (let ((r (*pos* 'clone)))
                 (r 'setx! x)
                 (r 'sety! y)
                 r))

(define-object *mode* (*the-root-object*)
               (op-bindings set-op-bindings! '()) ; operator key bindings
               (motion-bindings set-motion-bindings! '()) ; motion key bindings
               (obj-bindings set-obj-bindings! '()) ; text object bindings (not included in 'all-bindings)
               (misc-bindings set-misc-bindings! '()) ; misc bindings
               (post-thunk set-post-thunk! (lambda (cursor) (void))) ; thunk called directly before drawing every loop (use for cleanup)

               ((bind! self resend type k thunk) ;; bind a thunk (thunk gets passed a cursor)
                (let* ((getter (symbol-append type '-bindings)) (setter (symbol-append 'set- getter '!)))
                  (self setter (append (self getter) `(,(cons k thunk))))))

               ((all-binds self resend)
                (apply append (map self (list 'op-bindings 'motion-bindings 'misc-bindings))))

               ((get-bind self resend k #!optional type)
                (let ((p (assoc k (self (if type
                                          (symbol-append type '-bindings)
                                          'all-binds)))))
                  (cdr (if p
                         p
                         (assoc 'else (self 'all-binds)))))))

(define-object *buf* (*the-root-object*)
               (cursors set-cursors! '())
               (lines set-lines! #("blah" "a" "b" "c" "some words on a line" "\t\ttabbed"))
               (scroll set-scroll! 0)

               ((get-line self resend i)
                (if (<= i (self 'last-line))
                  (vector-ref (self 'lines) i)
                  #f))

               ((replace-line self resend new-line line-i)
                (vector-set! (self 'lines) line-i new-line))

               ((delete-line self resend line-i)
                (if (= (self 'last-line) 1) ; edge-case: only one line in buffer
                  (self 'replace-line "" 0)
                  (self 'set-lines! (vector-delete (self 'lines) line-i)))
                (set! dirty-screen #t))

               ((delete self resend range)
                (let* ((start (car range))
                       (end (cadr range)))
                  (endwin)
                  (print (start 'x))
                  (print (start 'y))
                  (print (end 'x))
                  (print (end 'y))
                  (exit)
                  (cond
                    ((= (start 'y) (end 'y))
                     (self 'replace-line (string-replace (self 'get-line (start 'y)) "" (start 'x) (end 'x)) (start 'y))))
                  (caddr range) 'go start)
                (set! dirty-screen #t))

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
                (string-ref (self 'cur-line-s) (self 'line-pos)))

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
                               (loop)))))))

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

               ((backspace self resend) ; helper function to backspace
                (let ((s (string-remove (self 'cur-line-s) (self 'line-pos))))
                  (if s
                    (begin (self 'replace-cur-line s)
                           (self '%prev-char))))))

;;;

(*buf* 'set-cursors! (list (*cursor* 'new *buf*)))

(define command-mode (*mode* 'clone))
(define insert-mode (*mode* 'clone))
(define cur-mode command-mode)

;;; INSERT MODE

(bind! insert-mode 'misc KEY_ESCAPE (set! cur-mode command-mode))
(bind! insert-mode 'misc KEY_BACKSPACE (cursor 'backspace))
(bind! insert-mode 'misc KEY_ALT_BACKSPACE ((insert-mode 'get-bind KEY_BACKSPACE) cursor ch)) ; alias
(bind! insert-mode 'misc 'else
       (cursor 'replace-cur-line
               (string-insert (cursor 'cur-line-s)
                              (string ch)
                              (cursor 'line-pos)))
       (cursor '%next-char))

;;; COMMAND MODE

(command-mode 'set-post-thunk! (lambda (cursor)
                                 (cursor 'clamp-to-line)))
(bind! command-mode 'misc #\i (set! cur-mode insert-mode)) ; TODO: use list of pairs for bind! macro
(bind! command-mode 'motion #\h (cursor 'm-prev-char))
(bind! command-mode 'motion #\j (cursor 'm-next-line))
(bind! command-mode 'motion #\k (cursor 'm-prev-line))
(bind! command-mode 'motion #\l (cursor 'm-next-char))
(bind! command-mode 'motion #\$ (cursor 'm-eol))
(bind! command-mode 'motion #\0 (cursor 'm-bol))
(bind! command-mode 'misc #\X (cursor 'backspace))
(bind! command-mode 'misc #\x (cursor 'm-next-char) (cursor 'backspace))
(bind! command-mode 'misc #\O
       (cursor 'insert-line 'prev)
       (cursor 'm-bol)
       (set! cur-mode insert-mode))
(bind! command-mode 'misc #\o
       (cursor 'insert-line 'next)
       (cursor 'm-bol)
       (set! cur-mode insert-mode))
(bind! command-mode 'op #\d
       ((cursor 'buffer) 'delete
                         (make-range cursor (command-mode 'get-bind (get-char) 'motion) cursor #f)))
(bind! command-mode 'misc #\a
       (cursor 'm-next-char)
       (set! cur-mode insert-mode))
(bind! command-mode 'motion #\w
       (if (not (cursor 'm-word 'next))
         (if (cursor 'm-next-line)
           (begin (cursor 'm-bol) (cursor 'm-word 'next)))))
(bind! command-mode 'motion #\b
       (if (not (cursor 'm-word 'prev))
         (if (cursor 'm-prev-line)
           (begin (cursor 'm-eol) (cursor 'm-word 'prev)))))
(bind! command-mode 'misc 'else (void))

;;; MAIN LOOP

(draw-init)
(draw-buf *buf*) ; FIXME: UGLY: initial draw
(let loop ()
  (let ((c (get-char)) (main-cursor (car (*buf* 'cursors))))
    ((cur-mode 'get-bind c) main-cursor c)
    ((cur-mode 'post-thunk) main-cursor)
    (draw-buf *buf*))
  (loop))
