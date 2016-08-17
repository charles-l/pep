(use ncurses srfi-1 srfi-13 regex prometheus)

(define KEY_ESCAPE 27)
(define KEY_ALT_BACKSPACE 127)

;;; general util - because this crap isn't implemented in the stdlib???

(define (clamp low up val)
  (max low (min up val)))

(define (for-n start stop fn)
  (map (cut fn <>) (iota (- stop start) start)))

(define (proper-line line) ; substitute weird characters out of a line
  (string-substitute "\t" tabstring line))

;;; more specialized util

(define (draw-line str i #!optional dirty)
  (wmove (stdscr) i 0)
  (if dirty
    (wclrtoeol (stdscr)))
  (waddstr (stdscr)
           (if str
             (proper-line str)
             "~")))

;;;

(define tabstop 3)
(define tabstring (list->string (make-list tabstop #\space)))

(define *buf* (*the-root-object* 'clone)) ; create main buffer
(*buf* 'add-value-slot! 'cursors 'set-cursors! '())
(*buf* 'add-value-slot! 'mode 'set-mode! 'command)
(*buf* 'add-value-slot! 'dirty-screen 'set-dirty-screen! #f)
(*buf* 'add-value-slot! 'lines 'set-lines! #("blah" "a" "b" "c" "some words on a line" "\ttabbed"))
(*buf* 'add-value-slot! 'scroll 'set-scroll! 0)
(define-method (*buf* 'get-line self resend i)
               (if (< i (- (self 'lines-length) 1))
                 (vector-ref (self 'lines) i)
                 #f))

(define-method (*buf* 'lines-length self resend) ; size of lines array
               (vector-length (self 'lines)))

(define-method (*buf* 'draw self resend)
               (for-n 0 (LINES)
                      (lambda (i)
                        (draw-line (self 'get-line (+ (self 'scroll) i)) i)))
               ((car (self 'cursors)) 'draw)
               (wrefresh (stdscr)))

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
               (if (< (self 'line) (- ((self 'buffer) 'lines-length) 2))
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

; TODO: actually draw a block so this works with multiple cursors
(define-method (cursor 'draw self resend #!optional altstr)
               (let ((str (if altstr
                            altstr
                            (self 'cur-line-s))))
                 (wmove (stdscr)
                        (self 'line)
                        (self 'proper-line-pos str))))

;;;

(define (insert-mode buf) ; returns a vector of the new lines
  (let* ((main-cursor (car (buf 'cursors)))
         (left-str (string-take (main-cursor 'cur-line-s) (main-cursor 'line-pos)))
         (right-str (string-drop (main-cursor 'cur-line-s) (main-cursor 'line-pos))))
    (let loop ((c (wgetch (stdscr))) (str left-str))
      (if (= (char->integer c) KEY_ESCAPE)
        #(str) ; break out and return vector of new lines
        (if (or (= (char->integer c) KEY_BACKSPACE) (= (char->integer c) KEY_ALT_BACKSPACE))
          (begin
            (loop (wgetch (stdscr)) (string-take str (- (string-length str) 2))))
          (begin
            (cond
              ((equal? c KEY_STAB)
               (wclrtoeol (stdscr))
               (set! c "\t")))
            (let ((newstr (string-append str (string c)))) ; meh - wish i didn't need this second let
              ;; redraw
              (wmove (stdscr) (main-cursor 'line) 0)
              (draw-line (string-append newstr right-str) (main-cursor 'line) #t)

              (main-cursor 'set-line-pos! (+ (main-cursor 'line-pos) 1))
              (main-cursor 'draw newstr)

              (loop (wgetch (stdscr)) newstr))))))))

(initscr)
(cbreak)
(noecho)
(*buf* 'set-cursors! (list (cursor 'new *buf*)))

(define (command-mode buf)
  (let loop ()
    (let ((c (wgetch (stdscr))) (main-cursor (car (*buf* 'cursors))))
      (cond
        ((equal? c #\i)
         (insert-mode buf))
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

      (buf 'draw))
    (loop)))

(command-mode *buf*)
(endwin)
