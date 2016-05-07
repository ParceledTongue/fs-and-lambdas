; F'S AND LAMBDAS
; a Scheme implementation of Tic-Tac-Toe

; A board is a list of nine characters, which are the contents of the board from
; left to right and then top to bottom. E.g., the list '(o n x x x o o n n)
; corresponds to the following board:
;
;  O |   | X 
; ---|---|---
;  X | X | O 
; ---|---|---
;  O |   |


; ; ; ; ; ;
; DISPLAY ;
; ; ; ; ; ;

(define display-board
  (lambda (b)
    (display "BOARD CODE: ") (display (board-to-code b)) (newline)
    (display-board-without-code b)))
  
(define display-board-without-code
  (lambda (b)
    (if (null? b)
        (newline)
        ; if there is still board info to print:
        (begin
          (display " ") (display (symbol-to-string (car b)))
          (if (or (eq? (length b) 7) (eq? (length b) 4))
              ; if this is the last symbol of the first or second row, display
              ; the horizontal break in the board
              (begin (newline) (display "---|---|---") (newline)
                     (display-board-without-code (cdr b)))
              ; otherwise, display the horizontal break (as long as we are not
              ; on the 9th grid space)
              (if (eq? (length b) 1)
                  (display-board-without-code (cdr b))
                  (begin (display " |") (display-board-without-code (cdr b)))))))))

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