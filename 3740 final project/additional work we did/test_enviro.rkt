;; CPSC 3740 - PROGRAMMING LANGUAGES - GROUP PROJECT
;; DEc 5th, 2024
;; Tarun, Connor, Yufun, Chamod.
;; testing the enivronment, simple source code.


#lang racket/base
;; there about 80 bound occurnaces for this.


(require rackunit
         rackunit/text-ui
         "enviro.rkt")
;; using the environment file called here.



;; these are basically the tests for the environ mods.

;; i thought of unit testing with racket, learned that from 2720 last year.

(define env-tests
  (test-suite
    "Testing the  envrio mod here."
    
    (let ([env (make-env)]
          ;; more bind x as 5, and y for 10, etc.
          ;; simple stuff
          
          [bind_x (make-binding 'x 5)]
          ;; break up. 
          [bind_y (make-binding 'y 10)])

      (test-case
        " these are tests for binding struct."
        
        (check-equal? (binding-symbol bind_x) 'x "Grabbing the  symbol here, etc.")
        ;; x, grabbing the symbol. 
        (check-equal? (binding-value bind_x) 5 "here grabbing the  value "))

      (test-case
        " testing the enivronment, env. "

        (check-equal? (env-empty? env) #t "new env")
        ;; checking the equals

        
        (check-equal? (env-empty? (env-add-binding bind_x env)) #f " opps, this is not empty"))
     
      (test-case
       
        "testing the  environment of lookup and binding" 
     
        (check-equal? (unbound? (env-lookup 'x env))
                      
                      #t
                      "lookup unbound env emtpy")
        
        (check-equal? (unbound? (env-lookup 'x
                                            ;; x environment x
                                            
                                            (env-add-binding bind_y env)))
                      #t

                      ;; fix this later, et.
                      ;; update, fixed add-bindings.
                      
                      "look-up unbound env thats not empty here. ")
        
        (check-equal? (env-lookup 'x
                                  (env-add-binding bind_x env))
                      ;; 5 as test for look up x, 
                      5
                      "lookup var in top level")
        (check-equal? (env-lookup 'x
                                  (env-add-binding bind_y
                                                   (env-add-binding bind_x
                                                                    
                                                                    env)))
                      5
                      "lookup with more than 1 val in the  top level here")
        (check-equal? (env-lookup 'x
                                  (env-add-binding
                                    (make-binding 'x 10)
                                    (env-add-binding bind_x env)))
                      10
                      "lookup variable re-declared"))


      

      (test-case
        " binding zip"

        (let ([symbols (list 'x 'y 'z)]
              
              [vals (list 5 10 15)])
          
          (check-equal? (env-lookup 'z
                                    (env-add-bindings
                                     
                                      (binding-zip symbols vals)
                                      env))
                        15)))
      
      )))

(run-tests env-tests 'verbose)
;; done. 

