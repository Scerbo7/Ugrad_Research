#lang racket
(define (expr? e)
  (match e
    [`(λ (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [(? symbol? x) #t]
    [_ #f]))

(define (environment? env)
  (and (hash? env)
       (andmap symbol? (hash-keys env))
       (andmap value? (hash-values env))))

(define (value? v)
  (match v
    [`(closure (λ (,(? symbol? x)) ,(? expr?)) ,(? environment? env))
     #t]
    [(? number? n) #t]  ;include numbers
    [_ #f]))

(define (storable? v)
  (value? v))

(define (alloc-fresh) `(addr ,(gensym 'addr)))

(define (addr? a)
  (match a
    [`(addr ,(? symbol? x)) #t]
    [_ #f]))

(define (store? env)
  (and (hash? env)
       (andmap addr? (hash-keys env))
       (andmap value? (hash-values env))))

(define (continuation? k)
  (match k
    ['done #t]
    [`(ar ,(? expr? e) ,(? environment? env) ,(? continuation? k-next)) #t]
    [`(fn ,(? value? v) ,(? continuation? k-next)) #t]
    [_ #f]))

(define (state? st)
  (match st
    [`(,(? expr? e) ,(? environment? ρ) ,(? store? σ) ,(? continuation? k) ,(? number? t)) #t]
    [_ #f]))

;; Changed steps for  time-stamping
(define (step* ς)
  (match ς
    ;; First rule
    [`(,(? symbol? x) ,env ,sto ,k ,t)
     (let ([result (hash-ref sto (hash-ref env x))])
       (match result
         [`(closure ,syn-v ,env+)
          `(,syn-v ,env+ ,sto ,k ,(add1 t))]))]

    ;; Second rule
    [`((,e0 ,e1) ,env ,sto ,k ,t)
     `(,e0 ,env ,sto (ar ,e1 ,env ,k) ,(add1 t))]

    ;; Third rule
    [`(,v ,env ,sto (ar ,e ,env+ ,k) ,t)
     `(,e ,env+ ,sto (fn ,v ,env ,k) ,(add1 t))]

    ;; Fourth rule
    [`(,v ,env ,sto (fn (λ (,x) ,e) ,env+ ,k) ,t)
     (let ([fresh-addr (alloc-fresh)])
       (define new-sto (hash-set sto fresh-addr `(closure ,v ,env)))
       (define new-env (hash-set env+ x fresh-addr))
       `(,e ,new-env ,new-sto ,k ,(add1 t)))]))

;; Changed injector for time-stamp
(define (inj e)
  `(,e ,(hash) ,(hash) done 0))

(define (final-state? state)
  (match state
    [`(,(? value? v) ,env ,sto ,k ,t) (eq? k 'done)]
    [_ #f]))

;; Running the CESK* machine
(define (run-cesk* expr)
  (let loop ([state (inj expr)])
    (if (final-state? state)
        state
        (loop (step* state)))))



;;simple λ expr w arg
(run-cesk* '((λ (x) x) 5))
;;complex λ expr --ChatGPT
(run-cesk* '(((λ (x) (λ (y) (x y))) (λ (z) z)) 5))

;;(run-cesk* test)