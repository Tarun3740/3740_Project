#lang racket

(define (startEval lst)
  (cond
    [(null? lst) '()]  ; Return an empty list if the input is empty.
    
    [(not (pair? lst)) lst]  ; If it's not a pair, return it as is.
    
    [(equal? (car lst) 'car)  ; If the first element is car, return the second element.
     (cadr lst)]
    
    [(equal? (car lst) 'cdr)  ; If the first element is cdr, return the third element.
     (caddr lst)] 

    [(equal? (car lst) 'cons)  ; If the first element is 'cons', construct a new list.
     (cons (cadr lst) (caddr lst))]  

    [else lst]
  ))

;; Test cases

(startEval '(car 1 2 3))  ;; returns 1

(startEval '(cdr 1 2 3))  ;; returns (2 3)

(startEval '(cons 0 (1 2 3)))  ;; returns '(0 1 2 3)
