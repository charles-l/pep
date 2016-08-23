;; docs: http://wiki.call-cc.org/eggref/4/ncurses
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
                 (if (< i (- (vector-length v) 1))
                   (vector-copy v i)
                   #())))

;;; more specialized util


;; bind a key to a command
(define-syntax bind!
  (syntax-rules ()
                ((bind! <mode> <key> <body> ...) ; WARNING: gonna break hygenic macros for a second...
                 (<mode> 'bind! <key> (eval '(lambda (cursor ch) <body> ...)))))) ; ... EWWWWW! YUCKY EVAL!

(define (proper-line line) ; substitute weird characters out of a line
  (string-substitute "\t" tabstring line #t))

(define (make-pos cursor)
  `((line ,(cursor 'line)) (line-pos ,(cursor 'line-pos))))

(define (make-motion cursor movement)
  (let ((c `(,(make-pos cursor))))
    (cursor movement)
    (append c `(,(make-pos cursor)))))

(define (draw-init) ; initialize ncurses and stuff
  (initscr)
  (cbreak)
  (noecho))

(define (draw-finalize) ; clean up drawing stuff
  (endwin))

; TODO: actually draw a block so this works with multiple cursors
(define (draw-cursor cursor #!optional altstr)
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

;;;

(define tabstop 3)
(define tabstring (list->string (make-list tabstop #\space)))
(define dirty-screen #f) ; do we need to redraw? this is an ugly to set! on purpose.

(define mode (*the-root-object* 'clone))
(mode 'add-value-slot! 'binds 'set-binds! '())
(mode 'add-value-slot! 'post-thunk 'set-post-thunk! (lambda (cursor) (void)))
;; bind a thunk that accepts a cursor
(define-method (mode 'bind! self resend k thunk)
               (self 'set-binds!
                     (append (self 'binds) `(,(cons k thunk)))))

(define-method (mode 'get-bind self resend k)
               (let ((p (assoc k (self 'binds))))
                 (cdr (if p
                        p
                        (assoc 'else (self 'binds))))))

(define *buf* (*the-root-object* 'clone)) ; create main buffer
(*buf* 'add-value-slot! 'cursors 'set-cursors! '())
(*buf* 'add-value-slot! 'mode 'set-mode! 'command)
(*buf* 'add-value-slot! 'lines 'set-lines! #("blah" "a" "b" "c" "some words on a line" "\t\ttabbed"))
(*buf* 'add-value-slot! 'scroll 'set-scroll! 0)
(define-method (*buf* 'get-line self resend i)
               (if (<= i (self 'last-line))
                 (vector-ref (self 'lines) i)
                 #f))

(define-method (*buf* 'replace-line self resend new-line line-i)
               (vector-set! (self 'lines) line-i new-line))

(define-method (*buf* 'delete-line self resend line-i)
               (if (= (self 'last-line) 1) ; edge-case: only one line in buffer
                 (self 'replace-line "" 0)
                 (self 'set-lines! (vector-delete (self 'lines) line-i)))
               (set! dirty-screen #t))

(define-method (*buf* 'insert-line self resend new-line line-i)
               (self 'set-lines! (vector-insert (self 'lines) new-line line-i))
               (set! dirty-screen #t))

(define-method (*buf* 'last-line self resend)
               (- (vector-length (self 'lines)) 1))

(define cursor (*the-root-object* 'clone))
(cursor 'add-value-slot! 'buffer 'set-buffer! '())
(cursor 'add-value-slot! 'line-pos 'set-line-pos! 0)
(cursor 'add-value-slot! 'line 'set-line! 0)

(define-method (cursor 'cur-line-s self resend)
               ((self 'buffer) 'get-line (self 'line)))

(define-method (cursor 'cur-char self resend)
               (string-ref (self 'cur-line-s) (self 'line-pos)))

(define-method (cursor 'prev-char self resend)
               (if (> (self 'line-pos) 0)
                 (string-ref (self 'cur-line-s) (- (self 'line-pos) 1))))

(define-method (cursor 'proper-line-pos self resend line)
               (string-length
                 (proper-line (string-take line (self 'line-pos)))))

(define-method (cursor 'begin-word? self resend)
               (and
                 (or (self 'bol?)
                     (char-whitespace? (self 'prev-char)))
                 (not (char-whitespace? (self 'cur-char)))))

(define-method (cursor 'eol? self resend)
               (>= (self 'line-pos) (- (string-length (self 'cur-line-s)) 1)))

(define-method (cursor 'p-eol? self resend)
               (>= (self 'line-pos) (string-length (self 'cur-line-s))))

(define-method (cursor 'bol? self resend)
               (<= (self 'line-pos) 0))

(define-method (cursor 'm-prev-line self resend)
               (if (> (self 'line) 0)
                 (self 'set-line! (- (self 'line) 1))
                 #f))

(define-method (cursor 'm-next-line self resend)
               (if (< (self 'line) ((self 'buffer) 'last-line))
                 (self 'set-line! (+ (self 'line) 1))
                 #f))

(define-method (cursor '%next-char self resend)
               (self 'set-line-pos! (+ (self 'line-pos) 1)))

(define-method (cursor '%prev-char self resend)
               (self 'set-line-pos! (- (self 'line-pos) 1)))

(define-method (cursor 'm-next-char self resend)
               (if (self 'p-eol?)
                 #f
                 (self '%next-char)))

(define-method (cursor 'm-prev-char self resend)
               (if (self 'bol?)
                 #f
                 (self '%prev-char)))

(define-method (cursor 'm-eol self resend)
               (self 'set-line-pos! (string-length (self 'cur-line-s))))

(define-method (cursor 'm-bol self resend)
               (self 'set-line-pos! 0))

(define-method (cursor 'm-word self resend dir)
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

(define-method (cursor 'clamp-to-line self resend)
               (if (> (self 'line) ((self 'buffer) 'last-line)) ; catch when cursor goes over edge (i.e. due to deletion of last line)
                 (self 'set-line! ((self 'buffer) 'last-line)))
               (self 'set-line-pos!
                     (clamp 0 (- (string-length (self 'cur-line-s)) 1)
                            (self 'line-pos))))

(define-method (cursor 'insert-line self resend dir)
               (if (equal? 'next dir)
                (begin
                 ((self 'buffer) 'insert-line "" (+ (self 'line) 1))
                 (self 'm-next-line))
                ((self 'buffer) 'insert-line "" (self 'line))))

(define-method (cursor 'replace-cur-line self resend new-str)
               ((self 'buffer) 'replace-line
                               new-str
                               (self 'line)))

(define-method (cursor 'new self resend buf)
               (let ((c (self 'clone)))
                 (c 'set-buffer! buf)
                 c))

;;;

(*buf* 'set-cursors! (list (cursor 'new *buf*)))

(define command-mode (mode 'clone))
(define insert-mode (mode 'clone))
(define cur-mode command-mode) ;TODO: do this more elegantly

;;; INSERT MODE

(bind! insert-mode KEY_ESCAPE (set! cur-mode command-mode))
(bind! insert-mode KEY_ALT_BACKSPACE
       (let ((s (string-remove (cursor 'cur-line-s) (cursor 'line-pos))))
         (if s
           (begin (cursor 'replace-cur-line s)
                  (cursor '%prev-char)))))
(bind! insert-mode 'else
       (cursor 'replace-cur-line
               (string-insert (cursor 'cur-line-s)
                              (string ch)
                              (cursor 'line-pos)))
       (cursor '%next-char))

;;; COMMAND MODE

(command-mode 'set-post-thunk! (lambda (cursor)
                                 (cursor 'clamp-to-line)))
(bind! command-mode #\i (set! cur-mode insert-mode)) ; TODO: use list of pairs for bind! macro
(bind! command-mode #\h (cursor 'm-prev-char))
(bind! command-mode #\j (cursor 'm-next-line))
(bind! command-mode #\k (cursor 'm-prev-line))
(bind! command-mode #\l (cursor 'm-next-char))
(bind! command-mode #\O
       (cursor 'insert-line 'prev)
       (cursor 'm-bol)
       (set! cur-mode insert-mode))
(bind! command-mode #\o
       (cursor 'insert-line 'next)
       (cursor 'm-bol)
       (set! cur-mode insert-mode))
(bind! command-mode #\d
       ((cursor 'buffer) 'delete-line (cursor 'line)))
(bind! command-mode #\a
       (cursor 'm-next-char)
       (set! cur-mode insert-mode))
(bind! command-mode #\w
       (if (not (cursor 'm-word 'next))
           (if (cursor 'm-next-line)
             (begin (cursor 'm-bol) (cursor 'm-word 'next)))))
(bind! command-mode #\b
       (if (not (cursor 'm-word 'prev))
           (if (cursor 'm-prev-line)
             (begin (cursor 'm-eol) (cursor 'm-word 'prev)))))
(bind! command-mode #\$ (cursor 'm-eol))
(bind! command-mode #\0 (cursor 'm-bol))
(bind! command-mode 'else (void))

;;; MAIN LOOP

(draw-init)
(draw-buf *buf*) ; FIXME: UGLY: initial draw
(let loop ()
  (let ((c (wgetch (stdscr))) (main-cursor (car (*buf* 'cursors))))
    ((cur-mode 'get-bind c) main-cursor c)
    ((cur-mode 'post-thunk) main-cursor)
    (draw-buf *buf*))
  (loop))
