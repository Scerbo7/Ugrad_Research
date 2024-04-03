#lang racket

;; CESK machine

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
    [_ #f]))

;; Machine in 2.2 of Van Horn and Might
(define (storable? v)
  (value? v))

;; generate a fresh, never before seen address
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

(define (ς? st)
  (match st
    [`(,(? expr? e) ,(? environment? ρ) ,(? store? σ) ,(? continuation? k)) #t]
    [_ #f]))

;; Fig 2. ς ⟼CESK ς'
(define (step ς)
  (match ς
    ;; First rule
    [`(,(? symbol? x) ,env ,sto ,k)
     (match (hash-ref sto (hash-ref env x))
       [`(closure ,syn-v ,env+)
        `(,syn-v ,env+ ,sto ,k)])]

    ;; Second rule
    [`((,e0 ,e1) ,env ,sto ,k)
     `(,e0 ,env ,sto (ar ,e1 ,env ,k))]

    ;; Third rule
    [`(,v ,env ,sto (ar ,e ,env+ ,k))
     `(,e ,env+ ,sto (fn ,v ,env ,k))]

    ;; Fourth rule
    [`(,v ,env ,sto (fn (λ (,x) ,e) ,env+ ,k))
     ;(pretty-print "here, in fourth rule")
     (define fresh-addr (alloc-fresh))
     (define built-value (match v
                           [`(λ (,y) ,e+) `(closure (λ (,y) ,e+) ,env)]
                           [(? number? n) n]))
     (define new-sto (hash-set sto fresh-addr built-value))
     (define new-env (hash-set env+ x fresh-addr))
     `(,e ,new-env ,new-sto ,k)]))

;; generate an initial state
(define (inj e)
  `(,e ,(hash) ,(hash) done))


(define bad-state (step (step (step (step (inj '((λ (x) x) ((λ (y) y) (λ (y) y)))))))))


(step bad-state)

;; work through this in the CEK machine
;; *and* CESK machine
;; using the implementation from last time
;; and this time, and write them "on paper,"
;; comparing each step
;;
;; ((λ (x) x) ((λ (y) y) (λ (y) y)))