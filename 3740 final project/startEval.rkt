;; CPSC 3740 - PROGRAMMING LANGUAGE - GORUP PROJECT
;; this is basically the main file that runs it all, using the environment file as well, it calls it.
;; Connor, Tarun, Chamod, Yufun.
;; Nov 30th, 2024.



#lang racket

;; as stated above uses the envrionment file. 
(require "enviro.rkt")

(provide startEval repl-eval)

;; rename
(define second cadr)
;; defining second and third caddr, etc.

(define third caddr)


;;eval here 

;; Eval lisps

(define (startEval x)
  
  (my-eval x (environment)))

;; Eval lisps program execution

;; t
;; return

(define (my-eval x env)
  
  (if (not (pair? x))
     (eval-atom x env)

     (eval-function x env)))


;; Eval expression made from single element, that is a non list
;;a lisp atom


;; return

(define (eval-atom x env)
  
  (if (symbol? x)
    (let ([ __val (env-lookup x env)])
      (if (unbound? __val)
          
        (ref-error x)
        __val))
  x))


;; Evaluates a lisp expression, a function exp.
;; x now is =  a lisp function expression



;; env now is =   current evaluation envrio
;; return result of expression.

(define (eval-function x env)

  (let ([__proc (car x)]
        [__args (cdr x)])
    
    (cond
     [(pair? __proc)
       ((my-eval __proc env) __args env)]
     
     [(procedure? __proc)
       (__proc __args env)]
     [(symbol? __proc)
      
       (let ([__val (env-lookup __proc env)])
         (if (unbound? __val)
             
           (ref-error x)
           (my-eval (cons __val __args) env)))]
     [else
       (raise-argument-error 'my-eval
                             "a procedure****"
                             (list __proc __args))])))

;; created function here used to intialize repl.

(define (repl-eval x env)
  (my-eval x (env-add-bindings env (environment))))


;; here is bullitin repl. !

;; Create neivro new, with the builtin binds.

;; bindings.
(define (environment)
  (env-add-bindings (builtin) (make-env)))

;; Returning a list basically 
(define (builtin)
  
  (list
    ;; arithmetics
    (make-binding '+ (binary-op +))
    

    (make-binding '- (binary-op -))

    (make-binding '* (binary-op *))
    (make-binding '/ (binary-op /))

    
    ;; comparing
    

    (make-binding '= (binary-op =))
    (make-binding '<= (binary-op <=))

    (make-binding '< (binary-op <))

    (make-binding '>= (binary-op >=))

    (make-binding '> (binary-op >))

    (make-binding 'equal? (binary-op equal?))

    (make-binding 'null? (unary-op null?))
;; null 
    (make-binding 'not (unary-op not))
    
    ;; lists here, pair, cdr, etc.
    
    (make-binding 'pair? (unary-op pair?))
    

    (make-binding 'cdr (unary-op cdr))

    (make-binding 'car (unary-op car))
;; 
    (make-binding 'cons (binary-op cons))
    (make-binding 'list my-list)
    
    ;; cond.
    (make-binding 'if my-if)
    
    (make-binding 'cond my-cond)
    
    ;; more stuff bindings, etc. lamda, and quotes, letrec.
    (make-binding 'quote my-quote)
    
    (make-binding 'lambda my-lambda)
    
    (make-binding 'let my-let)
    
    (make-binding 'letrec my-letrec)
    ;; from instruction pdf, etc.
    
    (make-binding 'map my-map)
    ))

;; Redefine given unary procedure for grabbing list of arguments basically, also calls proc on the first arg.
;;


(define (unary-op proc)
  
  (lambda (x env)
    ;; proc my eval for car x, environment.
    
    (proc (my-eval (car x) env))))

;; Same as unary-op but for binary procedures
(define (binary-op proc)
  (lambda (x env)
    
    (proc (my-eval (car x) env)
          ;; car, and second x
          (my-eval (second x) env))))

;;  ternary procedures here

(define (ternary-op proc)
  (lambda (x env)
    ;; lamda x enivronment, etc.
    (proc (my-eval (car x) env)
          (my-eval (second x) env)
          ;; fix bracket later.
          
          (my-eval (third x) env))))


;; theses are bindings for the variable.


;; Returns new list of bindings for all values.
;;
;; returnig a list of bindings
(define (eval-bindings bindings env)
  
;; map for lamda b, 
  (map (lambda (b)
          (make-binding (binding-symbol b)
                        ;; pulls from lamda b, above, arrow, hmm
                        
                        (my-eval (binding-value b) env)))
       bindings))

;; Error for unbound vars
;; x equal the var name
;; error
(define (ref-error x)
  (raise-syntax-error x
                      
          (string-append "undefined***;\n cannot ref "
                        "an identifer before def.")))
;; split up string, for max char on one line.

;; these are simple expressions here.

;; Exaluates args to an if functions
;; list of func

;; x functions list, etc.

;; 
;; return the result of if exp.

(define (my-if x env)
  ;; if statement x envo
  
  (let ([__cond (car x)]

        [__then (second x)]
;; second x
        [__else (third x)])
    (if (my-eval __cond env)
        ;; then evaluation
      (my-eval __then env)
      (my-eval __else env))))
;; fixed bracket problem here. done/



;; return the quoted result
(define (my-quote x env)
  (quasiquote (unquote (car x))))

;; Evaluates the arguments to a list function
;; same thing, similar stuff from above, returning etc.

(define (my-list x env)
  ;; map, lamda, y and x, eval
  (map (lambda (y) (my-eval y env)) x))


;; this is important, this is for lamda stuff


;; Evaluates the arguments to a lambda function
;;x = a list of arguments
;;
;; returns a procedure that execute lambda when called upon.

(define (my-lambda x env)
  ;;  procedure for lamda made, etc.
  
  (lambda (args __env)
    
    (let ([__param (car x)]
          ;; Evaluate the args
          [__args (map (lambda (x)
                          (my-eval x __env))
                       args)])
      ;; Evaluate 
      (my-eval (second x)
               (env-add-bindings (binding-zip __param __args) env)))))


; let/lectre, etc. stuff from the pdf intstruction.


;; Evaluates let function
;; xa list of arguments

;;same basic concept as above copy pasted code a little and changed it.

;;  a namespace

;;  the
;; returning let expression
(define (my-let x env)
  
  (let ([__defs (car x)])
    ;; Evals the body but after
    
    (my-eval (second x)
             (env-add-bindings (eval-bindings __defs env) env))))
;; adding bindings, defs, etc.

;; Evaluates the arguments to a letrec function.
;; x -> a list of arguments
;; env -> a namespace
;; return -> the result of the letrec expression
(define (my-letrec x env)
  (let* ([__defs (car x)]
         ;; Add unevaluated binding to create a new namespace
         [__env (env-add-bindings __defs env)])
    ;; Evaluate body after adding evaluated local bindings to the
    ;; namespace
    (my-eval (second x)
             (env-add-bindings (eval-bindings __defs __env) env))))

;; Evaluates a conditional expression
;;n

(define (my-cond x env)
  (cond
  [(not (null? x))
   ;; kind of like ptr, ++
   
    (let* ([__stmt (car x)]
           [__cond (car __stmt)]
           [__body (second __stmt)])
    (if (my-eval __cond env)
        
      (my-eval __body env)
      (my-cond (cdr x) env)))]))

;; Evaluates the arguments of a map expression
;; x equals list of arguments
;; env the current eval envroo


;; return  result of the map exp here. 
(define (my-map x env)
  
  (let ([__proc (car x)]
        
        [__list (second x)])
    ;; map rec. 
    (map-rec (my-eval __proc env) (my-eval __list env) env)))

;; recursive stuff for  helper mapping. 
(define (map-rec proc x env)
  (cond
    ;; null, empty, etc.
   [(null? x)
      '()]

   
   [else
    (cons (proc (list (car x)) env)
          (map-rec proc (cdr x) env))]))

;; done. 
