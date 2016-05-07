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
    (if (null? b)
        (newline)
        ; if there is still board info to print:
        (begin
          (display " ") (display (string-rep (car b)))
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

(define symbol-string-rep
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
        (string-append (list-ref alphabet (+ (* 9 (symbol-number-rep (car b)))
                                             (* 3 (symbol-number-rep (cadr b)))
                                             (symbol-number-rep (caddr b))))
                       (board-to-code (cdddr b))))))

(define symbol-number-rep
  (lambda (s)
    (cond
      ((eq? s 'o) 0)
      ((eq? s 'x) 1)
      ((eq? s 'n) 2)
      (else (error s "Invalid board symbol")))))

(define alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
                       "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "0"))
; (if only the Latin alphabet had 27 letters instead of only 26,
; we wouldn't need that silly 0)