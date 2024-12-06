#lang racket


;; Function to look up a variable in the environment
(define (assoc var env)
  (cond
    
    [(null? env) (error "variable not found" var)]
    
    [(eq? (caar env) var) (cdar env)]
    [else (assoc var (cdr env))]))


;(define (startEval x)
  ;(eval x (env)))

;(define (env)
  ;(env-bind (custom) (env-make)))

;(define (custom)
  ;(list
   ;(custom-bind

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

       ;; Relational operators

       [(equal? = <= < >= >) (eval-relat (car expr) (cadr expr) (caddr expr) env)]


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

;; Function to evaluate relational expressions
(define (eval-relat opr lhs rhs env)
  (let ((left-val (startEval lhs env))
        (right-val (startEval rhs env)))
    [case opr
      [(equal?) (equal? left-val right-val)]
      
      [(=) (= left-val right-val)]

      [(<=) (<= left-val right-val)]

      [(<) (< left-val right-val)]

      [(>=) (>= left-val right-val)]

      [(>) (> left-val right-val)]

      [else (error "Unknown operator")]]))


;;Let

(if (eq? (car expr) 'let)
    (let ([bindings (cadr expr)]
          [body (caddr expr)]
          [new-env env])
      (for-each
       (lambda (binding)
         (set! new-env (cons (cons (car binding)
                                   (startEval (cadr binding) env))
                             new-env)))
       bindings)
      (startEval body new-env)))

;; Letrec

(if (eq? (car expr) 'letrec)
    (let ([bindings (cadr expr)]
          [body (caddr expr)]
          [new-env env])
      (for-each
       (lambda (binding)
         (set! new-env (cons (cons (car binding) #f) new-env)))
       bindings)
      (for-each
       (lambda (binding)
         (set-cdr! (assoc (car binding) new-env)
                   (startEval (cadr binding) new-env)))
       bindings)
      (startEval body new-env)))

;; Conditional

(if (eq? (car expr) 'if)
    (if (startEval (cadr expr) env)
        (startEval (caddr expr) env)
        (startEval (cadddr expr) env)))



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


(displayln (startEval '(equal? x 5) env)) ; Output: #t
(displayln (startEval '(<= 9 20) env)) ; Output: #t
(displayln (startEval '(< 11 7) env)) ; Output: #f
(displayln (startEval '(>= 10 y) env)) ; Output: #t
(displayln (startEval '(> 6 15) env)) ; Output: #f


;; List 
;(define (startEval lst)
  ;(cond
    ;[(null? lst) '()]  ; Return an empty list if the input is empty.
    
    ;[(not (pair? lst)) lst]  ; If it's not a pair, return it as is.
    
    ;[(equal? (car lst) 'car)  ; If the first element is car, return the second element.
     ;(cadr lst)]
    
    ;[(equal? (car lst) 'cdr)  ; If the first element is cdr, return the third element.
     ;(caddr lst)] 

    ;[(equal? (car lst) 'cons)  ; If the first element is 'cons', construct a new list.
     ;(cons (cadr lst) (caddr lst))]  

    ;[else lst]
  ;))

;; Test cases

;(startEval '(car 1 2 3))  ;; returns 1

;(startEval '(cdr 1 2 3))  ;; returns (2 3)

;(startEval '(cons 0 (1 2 3)))  ;; returns '(0 1 2 3)
