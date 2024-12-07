;; Basically this is a environment file that gives make, looking, adds the bindings, and empty, null (etc.)
;; Tarun, Yufun, Chamod, Connor
;; 3740 CPSC - Programming Languages - Group Project



#lang racket

(provide  make-env
          env-lookup
          

          env-add-binding

          env-add-bindings
          env-empty?
;; null i think, etc. empty 
          make-binding

          binding-symbol

          binding-value

          binding-eq?
          binding-zip
          unbound?)


;; unbound here, 

;; Returned when maybe a  symbol cant be found in the envroronment


(define UNBOUND (gensym))

;; Returns true id a given value is UNBOUND





(define (unbound? value) (equal? value UNBOUND))



;; Enviro

;; Returning a brand new environment here,


(define (make-env) (list)) 

;; Lookup a symbol in the envirionment and return its value or UNBOUND
;; symbol env -> any | unbound-symbol 
(define (env-lookup symbol env)
  

  (if (env-empty? env)
    UNBOUND
    (if (binding-eq? symbol (env-car env))
        ; gonna fix this later.
        
      (binding-value (env-car env))

      (env-lookup symbol (env-rest env)))))

;; Add binding on using namespace.


;; binding here, for the env-add-binding,

(define (env-add-binding binding env)
  (cons binding env))

;; here you're adding a list of the bindings t


;; list of binding env -> env
(define (env-add-bindings bindings env)

  ;; if statement for null void, or i guess empty bindings
  (if (null? bindings)
      
    env
    (env-add-bindings (cdr bindings)
                      (cons (car bindings) env))))

;; env binding
;; binding.
(define (env-car env) (car env))

;; youre getting remaining part of the environmnet

;; env = env

(define (env-rest env) (cdr env))

;; Check if an envirionment is empty
;; env -> bool
(define (env-empty? env)
  (null? env))
;; null above.

;; binding here.

;; Returning  new binding of a symbol.
;;


(define (make-binding symbol value) (list symbol value))

;; Returns symbol
;; def binding for thr car binding.

(define (binding-symbol binding) (car binding))

;; Returns the value of a binding
;; binding, for basically any or all.

(define (binding-value binding) (cadr binding))

;; Checks to see if a binding is for a given synbol basically here.

;; symbol binding is basically a boolean like true of false.

(define (binding-eq? symbol binding)
  (equal? symbol (binding-symbol binding)))

;; T
(define (binding-zip symbols vals)
  (map make-binding symbols vals))
;; for the map done here.
