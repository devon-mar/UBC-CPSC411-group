#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v3)

(provide check-values-lang)

;; Takes an arbitrary value and either returns it,
;; if it is a valid Values-lang v3 program,
;; or raises an error with a descriptive error message.
(define/contract (check-values-lang p)
  (-> any/c values-lang-v3?)

  ;; Returns true if t is a valid defined name or an int64.
  (define/contract (triv? env t)
    (-> generic-set? any/c boolean?)
    (or (and (name? t) (set-member? env t))
        (int64? t)))

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))

  ;; Throws an error if names contains duplicate names.
  (define/contract (check-duplicate-bindings names)
    (-> (listof name?) void?)
    (if (and (check-duplicates names) #t)
      (error "Duplicate parallel bindings found")
      (void)))


  ;; values-lang-v3-tail -> values-lang-v3-tail
  (define/contract (check-values-lang-tail env t)
    (-> generic-set? any/c any/c)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        #:when (andmap name? xs)
        (check-duplicate-bindings xs)
        (for ([v vs])
          (check-values-lang-value env v))
        (check-values-lang-tail
          (foldl (lambda (x acc) (set-add acc x)) env xs)
          tail)]
      [v (check-values-lang-value env v)]))

  (define/contract (check-values-lang-value env v)
    (-> generic-set? any/c any/c)
    (match v
      [_ #:when (triv? env v) (void)]
      [`(,binop ,t1 ,t2)
        #:when (and
                 (binop? binop)
                 (triv? env t1)
                 (triv? env t2))
        (void)]
      [`(let ([,xs ,vs] ...) ,value)
        #:when (andmap name? xs)
        (check-duplicate-bindings xs)
        (for ([v vs])
          (check-values-lang-value env v))
        (check-values-lang-value
          (foldl (lambda (x acc) (set-add acc x)) env xs)
          value)]))

  ;; values-lang-v3-p -> values-lang-v3-p
  (define/contract (check-values-lang-p env p)
    (-> generic-set? any/c any/c)
    (match p
      [`(module ,t)
        (check-values-lang-tail env t)
        p]))

  (check-values-lang-p '() p))

(module+ test
  (require rackunit)

  (define (check-valid p)
    (check-equal? (check-values-lang p) p))

  (define (check-invalid p)
    (check-exn exn:fail? (lambda () (check-values-lang p) p)))
  
  ;; minimal valid programs
  (check-valid
    '(module 42))
  (check-valid
    '(module (+ 40 2)))
  (check-valid
    '(module (let ([x 42]) x)))

  ; minimal invalid programs
  (check-invalid
    '(module "42"))
  ;; x not defined
  (check-invalid
    '(module x))
  (check-invalid
    '(module (let ([x "not a number"]) x)))
  (check-invalid
    '(module (/ 40 2)))

  ;; duplicate parallel bindings
  (check-invalid
    '(module (let ([x 42] [x 40]) x)))
  (check-invalid
    '(module (let ([x 42] [y 41] [x 40]) x)))
  (check-invalid
    '(module
       (let
         ([x 42]
          [y (let ([z 42] [z 42]) y)])
         x)))

  ;; some more complicated tests
  (check-valid
    '(module
       (let ([x 42]
             [y (let ([x 20]) x)])
       x)))
  (check-valid
    '(module
       (let ([y 42])
         (let ([x y])
           x))))
  (check-invalid
    '(module
       (let ([y (let ([x 42]) x)])
         x)))
  (check-invalid
    '(module
       (let ([x 42]
             [y x])
         y))))
