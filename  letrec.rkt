#lang racket

(define (startEval expr env)
  (cond
    [(and (list? expr) (eq? (car expr) 'letrec))
     (let* ([bindings (cadr expr)]
            [body (caddr expr)]
            [placeholder-env (foldl
                              (lambda (binding acc-env)
                                (cons (cons (car binding) #f) acc-env))
                              env
                              bindings)]
            [final-env (foldl
                          (lambda (binding acc-env)
                            (assoc-set! acc-env (car binding)
                                        (startEval (cadr binding) acc-env)))
                          placeholder-env
                          bindings)])
       (startEval body final-env))]))
