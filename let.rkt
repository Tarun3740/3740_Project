#lang racket

(define (startEval expr env)
  (cond
    [(and (list? expr) (eq? (car expr) 'let))
     (let* ([bindings (cadr expr)]
            [body (caddr expr)]
            [new-env (foldl
                       (lambda (binding acc-env)
                         (cons (cons (car binding)
                                     (startEval (cadr binding) env))
                               acc-env))
                       env
                       bindings)])
       (startEval body new-env))]))
