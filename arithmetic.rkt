; Tarun Pokra
; 3740 Group Project Work
; Arithmetic Funtionality
; Nov 30th, 2024
;


#lang racket

;; Function to look up a variable in the environment
(define (assoc var env)
  (cond
    
    [(null? env) (error "variable not found" var)]
    
    [(eq? (caar env) var) (cdar env)]
    [else (assoc var (cdr env))]))

;; Main interpreter function: startEval

(define (startEval expr env)
  (cond
    ;; If it's a number, return it directly
    
    [(number? expr) expr]

    ;; If it's a variable, look it up in the environment
    
    [(symbol? expr) (assoc expr env)]

    ;; If it's a list, evaluate based on the first element (operator)

    [(list? expr)

     (case (car expr)

       ;; Arithmetic operators

       [(+ - * /) (eval-arith (car expr) (cadr expr) (caddr expr) env)]


       ;; Unknown expression

       [else (error "Unknown expression and or operator.")])]

    ;; If it doesn't match any pattern, throw an error
    [else (error "Unknown expression")]))

;; Function to evaluate arithmetic expressions
(define (eval-arith op lhs rhs env)
  (let ((left-val (startEval lhs env))
        (right-val (startEval rhs env)))
    (case op
      [(+) (+ left-val right-val)]
      
      [(-) (- left-val right-val)]
      
      [(*) (* left-val right-val)]
      
      [(/) (/ left-val right-val)]
      
      [else (error "Unknown operator")])))

;; Sample environment for testing
(define env '((x . 5) (y . 10)))

; like assignments i've created some quick easy test cases to test out this functionality


;; Sample test cases

(displayln (startEval 3 env))        ; Output: 3

(displayln (startEval 'x env))       ; Output: 5

(displayln (startEval '(+ 3 4) env)) ; Output: 7
(displayln (startEval '(* x y) env)) ; Output: 50
(displayln (startEval '(/ 20 4) env)) ; Output: 5
(displayln (startEval '(- 15 6) env)) ; Output: 9
