(use ncurses srfi-1 srfi-13 regex)

;;; util - because this crap isn't implemented by default???

(define-syntax iter ;; IM SO META IT'LL BLOW UR MIND
  (syntax-rules ()
    ((iter name op)
     (define-syntax name
       (syntax-rules ()
         ((name var)
          (set! var (op var 1)))
         ((name var val)
          (set! var (op var val))))))))

(define-syntax (assoc-ref)
  (syntax-rules ()
    ((assoc-ref alist key)
     (cadr (assoc key alist)))))

(define-syntax (assoc-set!)
  (syntax-rules ()
    ((assoc-set! alist key val)
     (set-cdr! (assoc key *buf*) (list value)))))

(iter inc! +)
(iter dec! -)

(define (clamp low up val)
  (max low (min up val)))

(define (for-n start stop fn)
  (map (cut fn <>) (iota (- stop start) start)))

(define (make-sym-eval . args) ; combines symbols, then evals the resulting function name
  ((eval (apply symbol-append args))))

;;;

(define (make-buffer lines)
  `((cursors '()) (mode 'command) (lines ,lines) (line-pos 0) (line 0) (scroll 0)))

(define (make-cursor line linepos scroll)
  `((line ,line) (linepos ,linepos)))

(define tabstop 3)
(define tabstring (list->string (make-list tabstop #\space)))
(define *buf* (make-buffer #("blah" "a" "b" "c" "some words on a line" "\ttabbed"))) ;; main buffer

(define (state-ref key)
  (cadr (assoc key *buf*)))

(define (state-set! key value)
  (set-cdr! (assoc key *buf*) (list value)))

(define state (getter-with-setter state-ref
                                  state-set!))

(define (cur-line-pos)
  (state 'line-pos))

(define (cur-line)
  (state 'line))

(define (cur-line-s)
  (vector-ref (state 'lines) (cur-line)))

(define (cur-char)
  (string-ref (cur-line-s) (cur-line-pos)))

(define (prev-cur-char)
  (if (> (cur-line-pos) 0)
    (string-ref (cur-line-s) (- (cur-line-pos) 1))
    #f))

(define (proper-line line) ;; line with tabs taken into account (use *only* for drawing)
  (string-substitute "\t" tabstring line))

(define (proper-line-pos) ;; calculates the linepos for the cursor
  (string-length (proper-line (string-take (cur-line-s) (cur-line-pos))))) ;; lineloc with tabs taken into account (use *only* for drawing)

(define (draw-line i)
  (mvwaddstr (stdscr) i 0
             (let ((offi (+ (state 'scroll) i)))
               (if (< offi (vector-length (state 'lines)))
                 (proper-line (vector-ref (state 'lines) offi))
                 "~"))))

(define (begin-word?)
  (and
    (or (bol?)
        (char-whitespace? (prev-cur-char)))
    (not (char-whitespace? (cur-char)))))

(define (eol?)
  (>= (cur-line-pos) (- (string-length (cur-line-s)) 1)))

(define (p-eol?) ;; one past eol
  (>= (cur-line-pos) (string-length (cur-line-s))))

(define (bol?)
  (<= (cur-line-pos) 0))

(define (m-prev-line)
  (if (> (cur-line) 0)
    (dec! (state 'line))
    #f))

(define (m-next-line)
  (if (< (cur-line) (- (vector-length (state 'lines)) 1))
    (inc! (state 'line))
    #f))

(define (m-next-char)
  (if (p-eol?)
    #f
    (inc! (state 'line-pos))))

(define (m-prev-char)
  (if (bol?)
    #f
    (dec! (state 'line-pos))))

(define (m-eol)
  (set! (state 'line-pos) (string-length (cur-line-s))))

(define (m-bol)
  (set! (state 'line-pos) 0))

(define (m-word dir)
  (call/cc
    (lambda (break)
      (if (begin-word?) ;; bump past beginning of word if we're currently on one
        (if (not (make-sym-eval 'm- dir '-char))
          (break #f)))
      (let loop ()
        (if (not (begin-word?))
          (begin (if (not (make-sym-eval 'm- dir '-char))
                   (break #f))
                 (loop)))))))

(define (clamp-line)
  (set! (state 'line-pos) (clamp 0 (- (string-length (cur-line-s)) 1) (state 'line-pos))))

;;;;

;; create initial cursor (not used yet)
(set! (state 'cursors) (cons (state 'cursors) (make-cursor 0 0 0)))

(initscr)
(cbreak)
(noecho)
(let loop ()
  (let ((c (wgetch (stdscr))))
    (cond
      ((equal? c #\j)
       (m-next-line))
      ((equal? c #\k)
       (m-prev-line))
      ((equal? c #\l)
       (m-next-char))
      ((equal? c #\h)
       (m-prev-char))
      ((equal? c #\w)
       (if (not (m-word 'next))
         (if (m-next-line)
           (begin (m-bol) (m-word 'next)))))
      ((equal? c #\b)
       (if (not (m-word 'prev))
         (if (m-prev-line)
           (begin (m-eol) (m-word 'prev)))))
      ((equal? c #\$)
       (m-eol))
      ((equal? c #\0)
       (m-bol))))

  (clamp-line)

  ;; draw
  (for-n 0 (LINES)
         draw-line)
  (wmove (stdscr) (cur-line) (proper-line-pos))
  (wrefresh (stdscr))
  (loop))

(endwin)
