; F'S AND LAMBDAS
; a Racket implementation of Tic-Tac-Toe

; A board is a list of nine characters, which are the contents of the board from
; left to right and then top to bottom. E.g., the list '(o n x x x o o n n)
; corresponds to the following board:
;
;  O |   | X
; ---|---|---
;  X | X | O
; ---|---|---
;  O |   |

#lang racket


; ; ; ; ;
; USERS ;
; ; ; ; ;

(define play-x
  (lambda (p c)
    (display-all (play-at-position 'x (code-to-board c) p position-names))))

(define play-o
  (lambda (p c)
    (display-all (play-at-position 'o (code-to-board c) p position-names))))


; ; ; ; ; ;
; PLAYING ;
; ; ; ; ; ;

; the board resulting from playing symbol s on board b at position p
; (position defined according to list pnames)
(define play-at-position
  (lambda (s b p pnames)
    (cond
      ((null? pnames) (error p "Invalid position name"))
      ((string=? (car pnames) p) (if (eq? (car b) 'n)
                                     (cons s (cdr b))
                                     (error p "Position not free")))
      (else (cons (car b) (play-at-position s (cdr b) p (cdr pnames)))))))

(define position-names '("a1" "a2" "a3" "b1" "b2" "b3" "c1" "c2" "c3"))

; ; ; ; ; ;
; WINNING ;
; ; ; ; ; ;

; whether the player using symbol s has won on board b
(define has-won?
  (lambda (s b)
    (some-arrangement-matches? s b winning-arrangements)))

; whether symbol s on board b matches some arrangement in list l
(define some-arrangement-matches?
  (lambda (s b l)
    (if (null? l)
        #f
        (or (arrangement-matches? s b (car l)) (some-arrangement-matches? s b (cdr l))))))

; whether symbol s on board b matches arrangement a
(define arrangement-matches?
  (lambda (s b a)
    (cond
      ; finished checking the arrangement:
      ((null? a) #t)
      ; the state of the current square is not relevant to the arrangement:
      ((not (car a)) (arrangement-matches? s (cdr b) (cdr a)))
      ; the square state is relevant and the symbol matches:
      ((eq? (car b) s) (arrangement-matches? s (cdr b) (cdr a)))
      ; the square state is relevant and the symbol does not match:
      (else #f))))

; check if the board is full (for draws)
(define board-full?
  (lambda (b)
    (cond
      ((null? b) #t)
      ((eq? (car b) 'n) #f)
      (else (board-full? (cdr b))))))

(define winning-arrangements '((#t #t #t #f #f #f #f #f #f)
                               (#f #f #f #t #t #t #f #f #f)
                               (#f #f #f #f #f #f #t #t #t)
                               (#t #f #f #t #f #f #t #f #f)
                               (#f #t #f #f #t #f #f #t #f)
                               (#f #f #t #f #f #t #f #f #t)
                               (#t #f #f #f #t #f #f #f #t)
                               (#f #f #t #f #t #f #t #f #f)))


; ; ; ; ; ;
; DISPLAY ;
; ; ; ; ; ;

(define display-all
  (lambda (b)
    (display "BOARD CODE: ") (display (board-to-code b)) (newline)
    (display-board b) (newline)
    (display-winner b)))

(define display-board
  (lambda (b)
    (if (null? b)
        (display "")
        ; if there is still board info to print:
        (begin
          (display " ") (display (symbol-to-string (car b)))
          (if (or (eq? (length b) 7) (eq? (length b) 4))
              ; if this is the last symbol of the first or second row, display
              ; the horizontal break in the board
              (begin (newline) (display "---|---|---") (newline)
                     (display-board (cdr b)))
              ; otherwise, display the horizontal break (as long as we are not
              ; on the 9th grid space)
              (if (eq? (length b) 1)
                  (display-board (cdr b))
                  (begin (display " |") (display-board (cdr b)))))))))

(define display-winner
  (lambda (b)
    (cond
      ((has-won? 'x b) (begin (display "*~*~*~*~*~*~*") (newline)
                              (display "~* ") (display (symbol-to-string 'x))
                              (display " WINS! *~") (newline)
                              (display "*~*~*~*~*~*~*")))
      ((has-won? 'o b) (begin (display "*~*~*~*~*~*~*") (newline)
                              (display "~* ") (display (symbol-to-string 'o))
                              (display " WINS! *~") (newline)
                              (display "*~*~*~*~*~*~*")))
      ((board-full? b) (begin (display "================") (newline)
                              (display "== CAT'S GAME ==") (newline)
                              (display "================"))))))

(define symbol-to-string
  (lambda (s)
    (cond
      ((eq? s 'o) "O")
      ((eq? s 'x) "X")
      ((eq? s 'n) " ")
      (else (error s "Invalid board symbol")))))


; ; ; ; ; ; ; ;
; BOARD CODES ;
; ; ; ; ; ; ; ;

(define board-to-code
  (lambda (b)
    (if (null? b)
        ""
        (string-append (list-ref alphabet (+ (* 9 (symbol-to-number (car b)))
                                             (* 3 (symbol-to-number (cadr b)))
                                             (symbol-to-number (caddr b))))
                       (board-to-code (cdddr b))))))

(define code-to-board
  (lambda (c)
    (if (string=? c "")
        '()
        (append (letter-to-board-row (substring c 0 1))
                (code-to-board (substring c 1))))))

(define letter-to-board-row
  (lambda (l)
    ((lambda (n)
       (list (number-to-symbol (remainder (truncate (/ n 9)) 3))
             (number-to-symbol (remainder (truncate (/ n 3)) 3))
             (number-to-symbol (remainder n 3))))
     (index-of l alphabet))))

(define symbol-to-number
  (lambda (s)
    (cond
      ((eq? s 'o) 0)
      ((eq? s 'x) 1)
      ((eq? s 'n) 2)
      (else (error s "Invalid board symbol")))))

(define number-to-symbol
  (lambda (n)
    (cond
      ((eq? n 0) 'o)
      ((eq? n 1) 'x)
      ((eq? n 2) 'n)
      (else (error n "Invalid number")))))

(define alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
                       "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "0"))
; (if only the Latin alphabet had 27 letters instead of only 26,
; we wouldn't need that silly 0)


; ; ; ; ;
; MISC. ;
; ; ; ; ;

(define index-of
  (lambda (a l)
    (call/cc
     (lambda (return)
       (letrec ([index-of* (lambda (a l)
                             (cond
                               ((null? l) (return -1))
                               ((string=? a (car l)) 0)
                               (else (+ 1 (index-of* a (cdr l))))))])
         (index-of* a l))))))
