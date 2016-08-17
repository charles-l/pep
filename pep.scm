(use ncurses srfi-1 srfi-13 regex prometheus)

;;; util - because this crap isn't implemented by default???

(define (clamp low up val)
  (max low (min up val)))

(define (for-n start stop fn)
  (map (cut fn <>) (iota (- stop start) start)))

(define (proper-line line) ; substitute weird characters out of a line
  (string-substitute "\t" tabstring line))

;;;

(define tabstop 3)
(define tabstring (list->string (make-list tabstop #\space)))

(define *buf* (*the-root-object* 'clone)) ; create main buffer
(*buf* 'add-value-slot! 'cursors 'set-cursors! '())
(*buf* 'add-value-slot! 'mode 'set-mode! 'command)
(*buf* 'add-value-slot! 'dirty-screen 'set-dirty-screen! #f)
(*buf* 'add-value-slot! 'lines 'set-lines! #("blah" "a" "b" "c" "some words on a line" "\ttabbed"))
(*buf* 'add-value-slot! 'scroll 'set-scroll! 0)

(define cursor (*the-root-object* 'clone))
(cursor 'add-value-slot! 'buffer 'set-buffer! '())
(cursor 'add-value-slot! 'line-pos 'set-line-pos! 0)
(cursor 'add-value-slot! 'line 'set-line! 0)

(define-method (cursor 'buf-lines self resend)
               ((self 'buffer) 'lines))

(define-method (cursor 'cur-line-s self resend)
               (vector-ref (self 'buf-lines) (self 'line)))

(define-method (cursor 'cur-char self resend)
               (string-ref (self 'cur-line-s) (self 'line-pos)))

(define-method (cursor 'prev-char self resend)
               (if (> (self 'line-pos) 0)
                 (string-ref (self 'cur-line-s) (- (self 'line-pos) 1))))

(define-method (cursor 'proper-line-pos self resend line)
               (string-length
                 (proper-line (string-take (self 'cur-line-s) (self 'line-pos)))))

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
               (if (< (self 'line) (- (vector-length (self 'buf-lines)) 1))
                 (self 'set-line! (+ (self 'line) 1))
                 #f))

(define-method (cursor 'm-next-char self resend)
               (if (self 'p-eol?)
                 #f
                 (self 'set-line-pos! (+ (self 'line-pos) 1))))

(define-method (cursor 'm-prev-char self resend)
               (if (self 'bol?)
                 #f
                 (self 'set-line-pos! (- (self 'line-pos) 1))))

(define-method (cursor 'm-eol self resend)
               (self 'set-line-pos! (string-length (self 'cur-line-s))))

(define-method (cursor 'm-bol self resend)
               (self 'set-line-pos! 0))

(define-method (cursor 'm-word self resend dir)
               (call/cc
                 (lambda (break)
                   (if (self 'begin-word?) ;; bump past beginning of word if we're currently on one
                     (if (not (self (symbol-append 'm- dir '-char)))
                       (break #f)))
                   (let loop ()
                     (if (not (self 'begin-word?))
                       (begin (if (not (self (symbol-append 'm- dir '-char)))
                                (break #f))
                              (loop)))))))

(define-method (cursor 'clamp-to-line self resend)
               (self 'set-line-pos!
                     (clamp 0 (- (string-length (self 'cur-line-s)) 1) (self 'line-pos))))

(define-method (cursor 'new self resend buf)
               (let ((c (self 'clone)))
                 (c 'set-buffer! buf)
                 c))

(define (draw-line i)
  (mvwaddstr (stdscr) i 0
             (let ((offi (+ (*buf* 'scroll) i)))
               (if (< offi (vector-length (*buf* 'lines)))
                 (proper-line (vector-ref (*buf* 'lines) offi))
                 "~"))))

;;;

(initscr)
(cbreak)
(noecho)
(*buf* 'set-cursors! (list (cursor 'new *buf*)))
(let loop ()
  (let ((c (wgetch (stdscr))) (main-cursor (car (*buf* 'cursors))))
    (cond
      ((equal? c #\j)
       (main-cursor 'm-next-line))
      ((equal? c #\k)
       (main-cursor 'm-prev-line))
      ((equal? c #\l)
       (main-cursor 'm-next-char))
      ((equal? c #\h)
       (main-cursor 'm-prev-char))
      ((equal? c #\w)
       (if (not (main-cursor 'm-word 'next))
         (if (main-cursor 'm-next-line)
           (begin (main-cursor 'm-bol) (main-cursor 'm-word 'next)))))
      ((equal? c #\b)
       (if (not (main-cursor 'm-word 'prev))
         (if (main-cursor 'm-prev-line)
           (begin (main-cursor 'm-eol) (main-cursor 'm-word 'prev)))))
      ((equal? c #\$)
       (main-cursor 'm-eol))
      ((equal? c #\0)
       (main-cursor 'm-bol)))

    (main-cursor 'clamp-to-line)

    ;; draw
    (for-n 0 (LINES)
           draw-line)
    (wmove (stdscr)
           (main-cursor 'line)
           (main-cursor 'proper-line-pos (main-cursor 'cur-line-s)))
    (wrefresh (stdscr)))
  (loop))
(endwin)
