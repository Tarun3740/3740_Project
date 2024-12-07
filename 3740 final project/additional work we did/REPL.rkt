;; repl file, uses starteval, etc.
;; CPSC 3740 - Programming Langauges - Group project
;; Connor, tarun, Chamod, Yufun.
;; 
;; prompts user.
;; additional programming to get a feel for input and output.

#lang racket
(require "startEval.rkt")
;; exact based on instruction for project pdf.
;; changed to all caps.
;; USES repl-eval
;; 
;; Renameing
(define second cadr)


;; REPL #

;; Run this and it will read in input from user and displays eval results
(define (repl ns)
  (let ([__input (get-input)])
    (cond
      
     ;; ending REPL
     [(eof-object? __input)
        #f]
     
     ;; repl functs 
     [(and (pair? __input) (equal? (car __input) 'define))
        (repl (my-define (cdr __input) ns))]
     [(and (pair? __input) (equal? (car __input) 'expect))
        (println (expect-eval (second __input) ns))
        (repl ns)]
     
     ;; print
     ;; else option here. 
     [else
        (println (repl-eval __input ns))
        (repl ns)])))


;; user gives inputs and it shows it with a prompt

;; hit return for it though.
;; lamda return, etc.

(define (get-input)
  (display " >> ")
  (with-handlers ([exn? (lambda (x) (exn-message x))])
          (read)))
;; lamdaa

;; Eval a express with racket interpreter

;; interpreter here.


(define expect-eval
  

  (let [(__ns (make-base-namespace))]

    (lambda (expr ns)

      (eval (cons 'let
                    (list ns
                          expr))
              __ns))))


;; defining here. this is another important thing. for binding definitoin to name and add it to namespace


(define (my-define x ns)
  (if (pair? (car x))
      
    (def-func x ns)
    (cons (list (car x)
                (repl-eval (second x) ns))
          ns)))

;; Binding new function 
;;
;; defining func x, clater, etc.

(define (def-func x ns)
  
  (let* ([__decl (car x)]
         [__name (car __decl)]
         ;; name, args,
         [__args (cdr __decl)]
         [__body (cdr x)])
    (cons (list __name
                ;; creating and construction of a lambda expression from suer input
                
                ;; andgiving it to  interpreter to get right procedure
                
                (repl-eval (cons 'lambda
                                 
                                  (cons __args __body))
                           ns))
          ns)))


;; begin repl here, this block of code i guess kind of just runs repl with the exception of handling so it cant crash even if the user makes a error.

;; crash protection, etc.

(define (run ns)
  (with-handlers ([exn?
                   
                      (lambda (exn)
                        
                        (printf "~a\n" (exn-message exn))
                        (run ns))])
    (repl ns)))

;; show to user
(display "Hello there, type here in repl. \n")

;; execute.
(run '())