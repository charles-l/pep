(use ncurses srfi-1 srfi-13 regex protobj)

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

(define *buf* (%)) ; create main buffer
(! *buf*
   (cursors '())
   (mode 'command)
   (dirty-screen #f) ; requires a redraw
   (lines #("blah" "a" "b" "c" "some words on a line" "\ttabbed"))
   (scroll 0))

(define (make-cursor buf)
  (let ((c (%)))
    (! c
       (buffer buf)
       (line-pos 0)
       (line 0)
       (buf-lines
         (lambda (self)
           (? (? self buffer) lines)))
       (cur-line-s
         (lambda (self)
           (vector-ref (@ self buf-lines) (? self line))))
       (cur-char
         (lambda (self)
           (string-ref (@ self cur-line-s) (? self line-pos))))
       (prev-char
         (lambda (self)
           (if (> (? self line-pos) 0)
             (string-ref (@ self cur-line-s) (- (? self line-pos) 1)))))
       (proper-line-pos
         (lambda (self line)
           (string-length
             (proper-line (string-take (@ self cur-line-s) (? self line-pos))))))
       (begin-word?
         (lambda (self)
           (and
             (or (@ self bol?)
                 (char-whitespace? (@ self prev-char)))
             (not (char-whitespace? (@ self cur-char))))))
       (eol?
         (lambda (self)
           (>= (? self line-pos) (- (string-length (@ self cur-line-s)) 1))))
       (p-eol?
         (lambda (self)
           (>= (? self line-pos) (string-length (@ self cur-line-s)))))
       (bol?
         (lambda (self)
           (<= (? self line-pos) 0)))
       (m-prev-line
         (lambda (self)
           (if (> (? self line) 0)
             (! self line (- (? self line) 1))
             #f)))
       (m-next-line
         (lambda (self)
           (if (< (? self line) (- (vector-length (@ self buf-lines)) 1))
             (! self line (+ (? self line) 1))
             #f)))
       (m-next-char
         (lambda (self)
           (if (@ self p-eol?)
             #f
             (! self line-pos (+ (? self line-pos) 1)))))
       (m-prev-char
         (lambda (self)
           (if (@ self bol?)
             #f
             (! self line-pos (- (? self line-pos) 1)))))
       (m-eol
         (lambda (self)
           (! self line-pos (string-length (@ self cur-line-s)))))
       (m-bol
         (lambda (self)
           (! self line-pos 0)))
       (m-word
         (lambda (self dir)
           (call/cc
             (let ((dir-func (if (eq? dir 'next) (? self m-next-char) (? self m-prev-char))))
               (lambda (break)
                 (if (@ self begin-word?) ;; bump past beginning of word if we're currently on one
                   (if (not (dir-func self))
                     (break #f)))
                 (let loop ()
                   (if (not (@ self begin-word?))
                     (begin (if (not (dir-func self))
                              (break #f))
                            (loop)))))))))
       (clamp-to-line
         (lambda (self)
           (! self line-pos (clamp 0 (- (string-length (@ self cur-line-s)) 1) (? self line-pos))))))
    c))

(define (draw-line i)
  (mvwaddstr (stdscr) i 0
             (let ((offi (+ (? *buf* scroll) i)))
               (if (< offi (vector-length (? *buf* lines)))
                 (proper-line (vector-ref (? *buf* lines) offi))
                 "~"))))

;;;;

(initscr)
(cbreak)
(noecho)
(! *buf* cursors (list (make-cursor *buf*)))
(let loop ()
  (let ((c (wgetch (stdscr))) (main-cursor (car (? *buf* cursors))))
    (cond
      ((equal? c #\j)
       (@ main-cursor m-next-line))
      ((equal? c #\k)
       (@ main-cursor m-prev-line))
      ((equal? c #\l)
       (@ main-cursor m-next-char))
      ((equal? c #\h)
       (@ main-cursor m-prev-char))
      ((equal? c #\w)
       (if (not (@ main-cursor m-word 'next))
         (if (@ main-cursor m-next-line)
           (begin (@ main-cursor m-bol) (@ main-cursor m-word 'next)))))
      ((equal? c #\b)
       (if (not (@ main-cursor m-word 'prev))
         (if (@ main-cursor m-prev-line)
           (begin (@ main-cursor m-eol) (@ main-cursor m-word 'prev)))))
      ((equal? c #\$)
       (@ main-cursor m-eol))
      ((equal? c #\0)
       (@ main-cursor m-bol)))

    (@ main-cursor clamp-to-line)

    ;; draw
    (for-n 0 (LINES)
           draw-line)
    (wmove (stdscr)
           (? main-cursor line)
           (@ main-cursor proper-line-pos (@ main-cursor cur-line-s)))
    (wrefresh (stdscr)))
  (loop))

(endwin)
