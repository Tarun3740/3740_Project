;; CPSC 3740 - GROUP PROJECT
;; PROGRAMMING LANGUAGES
;; tARUN, Chamod, YUfun, Connor.
;; this is the test file, about 50 of them, 3-4 arent working, will fix later.
;; some of the tests examples are added in the report too.

;;Dec 1st, 2024

#lang racket
(require "startEval.rkt")
;; this uses the main starteval file

;; Test numbers
(define failed 0)
(define passed 0)

;; Returns the expected value of a program or expression


(define expect-eval

  (let [(ns (make-base-namespace))]
;; lamda expr, ns, etc.
    ;; namespace stuff here.
    
    (lambda (expr) (eval expr ns))))

;; Testing function that executes given exp with base here

;; watch to see what the result should be because then It will execut the expression with start eval and compare and check resut.
;; If the test passes the test name is printed and if it fails the title

;; expected with actual vals are displayed on

;; x =  racket expression. ie. + 3 4

;; title stuff
;;  title of the test

(define (test x title)
  
  (let ([__expected (expect-eval x)]

        ;; expected and actual results
        
        [__actual (startEval x)])
    (cond
    [(not (equal? __expected __actual))

     ;; set new, failed, etc.
     
      (set! failed (+ failed 1))

      ;; added a new line here.
      
      (newline)
      (display "Failed!!\n")
      (display (format "~a\n" title))
      (display (format "the expected is: ~a\n" __expected))
      ;; print to users.
      
      (display (format " and the actual: ~a\n" __actual))
      
      (display "\n")]
    
    [else
      (set! passed (+ passed 1))

      ;; print line
      (println title)])))


;; easy simple  tests here

;; only single data values
(test '4 " Test with a number ")
(test '"hi" " tests with a string. ")
(test '#t " Testing withempty lists ")

;;  arithmetic operators testing below here.

(test  '(+ (- 20 5)
           (* (/ 10 2)
             (+ 3 1)))
        "  arithmetic operators being tested. ")

;; Test relational operators
(test '(= 5 5) " Test = ")
(test '(< 5 3) " Test < ")

(test '(<= 3 5) " Test <= ")
(test '(> 3 5) " Test > ")

(test '(>= 5 3) " Test >= ")
(test '(equal? 12 (+ 6 6)) " is Test equal? hmm.. ")

;; more tests below for the lists this time. 
(test '(equal? (quote ()) (quote ()))
      " Test equal? with lists ")

;; Test if expressions tested.
(test '(if (< 3 5)
          3
          5)
      " Test if-then ")
      
(test '(if (> 3 5)
          3
          5)
      " testing if-else ")

;; testing the  quote now.
(test '(quote (+ 3 4))
      " testing the quote here. ")

;; Test cons
(test '(cons 4 5)
      " est cons for 2 nums here. ")
      
(test '(cons 4 '(5 6 7))
      " testing w/num & listings ")

;; list testing here.
;; fix later,
;; fixed now. 
(test '(list 4 5 6 7)
      " Test list here. ")

;;  car
(test '(car '(4 5 6 7))
      " Test car.")

;;  cdr
(test '(cdr '(4 5 6 7))
      " test CDR")

;; now Testing pair?
(test '(pair? '(4 5 6 7))
      " Test pair? on the list now here. ")

(test '(pair? 4)
      " Test pair? on num basically ")

;; now testing lamda here below.
;; lamda 

(test '((lambda (x) x) 10)
       " testing  lambda with just return X. ")

(test '((lambda (x y)
          (+ x y))
        10 20)
       " Test lambda included with the arguments below. ")

(test '((lambda (f x y)
          (f x y))
        (lambda (x y) (+ x y)) 10 20)
      ;; testing lamda as a argument, but being passed as one, etc.
      
      " test passing a lambda as  argument to lambda. ")

(test '((lambda (x) ((lambda (y) (+ x y)) x)) 10)
      " testing lambda with lambda inside the body")
;; kind of like lamda within a lamda, etc.
;; stack example,

(test '(((lambda (x) (lambda (y) (+ x y))) 1) 2)
    "-- Function that makes a function --")

;; now basically testing let/letrec

(test '(let ([x 4]
             [y 10])
          (let ([y 5]
                [x y])
            (+ x y)))
       " let with 2 levelss")

(test '(let ([x 4]) 4 5 6 7 x)
      ;;muliple, etc.
      
    " multi literals in  let body")

(test '(letrec ([x 5]
                [y 3])
         (+ x y))
       " testing letrec easy way")

(test '(letrec ([fact
                  (lambda (x)
                    (if (= x 0)
                        ;; fix quote later, 1 as
                      (quote 1)
                      
                      (* x (fact (- x 1)))))])
         ;; missing bracket fixed.
         
                    (fact 10))
       " test letrec with  recursive lambda")
;; factorial stuff, borrowed idea from 2720 class last year

(test '(let ([+ (lambda (x) (car x))]
             [let '(2 3 4 5)])
          (+ let))
        "Test let rebinding keywords included iwth lam ")

(test '(let ([let '(2 3 4 5)])
          let)
      
        " Test let rebinding keywords w/ data")

(test '((let ([x 4])
              (lambda (y) (+ x y)))
          5)
        " test let exp. basically returns procedure like a anonymus lambd")

(test '(let ([fib (lambda (n) (+ n 1))])
         
        (letrec ((fib
                  ;; n lamda, etc. if statementfor less or equal
                  ;;to n 1  basically.
                  
            (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
           
           (fib 7)))
      " Letrec var already def as, fib")

;; got some more tests, this time a bit more detailed. complex, etc.


(test '((lambda (x y)
          
          (let ([z x]
;; indenting error fixed
                [w y])
          (+ w z)))
        10 20)
       " Testing the lambda with let but this time inside the body here. ")

(test '(let ([a (lambda (f y) (f y))]) (a pair? '()))
    "testing  let with function passing ")

(test '((let ([a (lambda (f y) (f y))]) a) pair? '())
    " testing  let with lambda that returns procedure and take in a function basically . ")

(test '((let ([a (lambda (f y) (f y))]) (lambda (x) (a x '()))) pair?)
      
    " TESIGIN let that returns funcn that uses a local func. in the body")

;; Asn

(test '(letrec ([levRec (lambda (n x cur res)

                          ;; if statement equal for ? x 
                          (if (equal? x (quote ()))
                            res
                            (if (= n cur)
                                ;; cur 
                              (cons (car x) res)
                              (levList n (cdr x) (+ cur 1) res))))]
                ;; kinda similar to mid term question,
                ;; added here.
                [levList (lambda (n lx cur res)
                          (if (equal? lx (quote ()))
                              
                            res
                            
                            (levRec n (car lx) cur (levList n (cdr lx) cur res))))])
         ;; lev rec, 
                (levRec
                  3
                  (quote (1 (2 (5 () ()) (6 ())) (3 ()) (4 ())))
                  1
                  (quote ())))
        " fetching every label at tree level here. ")

(test '(letrec ([greater (lambda (a b)
                           
                          (if (> a b) a b))]

                ;; tree h, h, x etc.
                ;; lamda testing.
              [treeH (lambda (h x)
                       
                      (if (equal? x (quote ()))
                          ;;this is for child h, 
                        h
                        (childrenH (+ h 1) (cdr x))))]

              ;; child h, lamda h, l1, etc.
              ;;x 
              [childrenH (lambda (h lx)
                           
                          (if (equal? lx (quote ()))
                              
                            h
                            (greater
                             ;;this time for if greater equal?
                             
                              (treeH h (car lx))
                              (childrenH h (cdr lx)))))])
              (treeH
                0
                (quote (1
                          (2 (3 () ()) (4 () ((6 ()))))
                          
                          (15 () ())
                          
                          (16 () ())))))
        "locate height of tree here basically. pretty simple. ")



;; instruct testing 

(test '(let ([inc (lambda (x) (+ x (quote 1)))])
        (inc (quote 5)))
      " instruct  Test: lambda in let with quotes")

(test '(letrec ((fib
                 ;; lamda n, if statement less or equal to n, 1.
                 ;; - n 2
            (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
         ;; lamda n, if statement less or equal to n, 1
         ;; - n, 2
           
           (fib 7))
      " testing here now Fibonacci ")

(test '(let ([sub1 (lambda (x) (- x 1))]
                  [not (lambda (x) (if x #f #t))])
                  
              ;; chamod fix,
         ;; connor, func.
         
              (letrec ([is-even? (lambda (n)
                                   ;; letrec, if it is even
                                   (if (= n '0)
                                       #t
                                       (is-odd? (sub1 n))))]
                       [is-odd? (lambda (n)
                                  ;; odd func, if statement, simple stuff like in c++.
                                  
                                  (if (not (= n '0))
                                      
                                      (is-even? (sub1 n))
                                      
                                      '#f
                                      ))])
                (is-odd? 11)))
      "i nstruc: Mutally recursive functions here.")

(test '(letrec ((intersect
             (lambda (s t)
               
               (if (equal? s (quote ()))
                   ;; if statement member, car
                   ;; car s, t.
                 (quote ())
                 (if (member (car s) t)
                   (cons (car s) (intersect (cdr s) t))
                   (intersect (cdr s) t)
                 )
               )
              ))
             (member
              (lambda (x s)
                ;; f 
                 (if (equal? s (quote ()))
                   (quote #f)
                   (if (equal? x (car s))
                     (quote #t)
                     ;; t,
                     ;;member x, s
                     (member x (cdr s))
                   )
                 )
              )
             ))
           (intersect (quote (a b c d)) (quote (b c d e f))))
      " instructor test, Intersection")

;; final more tests, because why not.

;;  returns from list are  same  if eva process.
;; returned, list,

;; not symbol

(test '(let ([c 6])
         
          (list (list "3" 4 'c)
                
                (list 4 "5" c)))
      
      " list w/ symbols & vars ")

;;  condition testing ----------

(test '(letrec ([member? (lambda (e x)
                           
                            (cond
                              
                            [(null? x)
                             
                              #f]
                            [(equal? e (car x))
                             ;; checking e, condition.
                             
                              #t]
                            [#t
                              (member? e (cdr x))]))])
              (member? 4 (list 3 6 5 2 4 6)))
      " testing condition with letrec and lambda here.")

;; finally map testing


(test '(list (map (lambda (x) (* x x)) (list 1 4 9 16)))
" Test map with simple lambda")

;; displaying testing results values. 

;; If an error, 

(newline)
;;added new newline, condition,

(cond
 [(= failed 0)
  ;; displaying formating if everything is basically good to go.
  (display (format "every single test is passed (~a)\n" passed))]
 [else
  (display (format " tests passed: ~a\n" passed))
  (display (format "Tests failed: ~a\n" failed))])
;; new line each time here.
;; done
(newline)

;; done here now. 