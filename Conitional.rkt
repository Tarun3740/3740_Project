#lang racket

(define (startEval expr env)
  (cond
    [(and (list? expr) (eq? (car expr) 'if))
     (if (startEval (cadr expr) env)
         (startEval (caddr expr) env)
         (startEval (cadddr expr) env))]))
