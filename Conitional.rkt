#lang racket

(if (eq? (car expr) 'if)
    (if (startEval (cadr expr) env)
        (startEval (caddr expr) env)
        (startEval (cadddr expr) env)))
