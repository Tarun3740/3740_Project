#lang racket

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