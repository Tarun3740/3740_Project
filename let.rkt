#lang racket

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