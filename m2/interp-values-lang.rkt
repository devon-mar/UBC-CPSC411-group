#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide interp-values-lang)

;; Milestone 4 Exercise 21
;;
;; Interpret the Values-lang v4 program p as a value. For all p, the value of
;; (interp-values-lang p) should equal to (execute p).
(define/contract (interp-values-lang p)
  (-> values-lang-v4? int64?)

  ;; Return a new environment from the xs and vs of a let.
  ;;
  ;; env: dict?
  ;; xs: list of name from a let
  ;; vs: list of values from a let
  ;; -> dict?
  (define/contract (let->new-env env xs vs)
    (-> dict? (listof name?) (listof any/c) dict?)
    (foldl
      (lambda (x v acc) (dict-set acc x (interp-values-lang-value env v)))
      env
      xs
      vs))

  ;; dict? values-lang-v4-p -> int64?
  (define/contract (interp-values-lang-p env p)
    (-> dict? values-lang-v4? int64?)
    (match p
      [`(module ,tail)
        (interp-values-lang-tail env tail)]))

  ;; dict? values-lang-v4-pred -> boolean?
  (define/contract (interp-values-lang-pred env p)
    (-> dict? any/c boolean?)
    (match p
      [`(true)
        #t]
      [`(false)
        #f]
      [`(not ,pred)
        (not (interp-values-lang-pred env pred))]
      [`(let ([,xs ,vs] ...) ,pred)
        (interp-values-lang-pred (let->new-env env xs vs) pred)]
      [`(if ,p1 ,p2 ,p3)
        (interp-values-lang-pred
          env
          (if (interp-values-lang-pred env p1)
            p2
            p3))]
      [`(,relop ,t1 ,t2)
        ((interp-values-lang-relop relop)
         (interp-values-lang-tail env t1)
         (interp-values-lang-tail env t2))]))

  ;; dict? values-lang-v4-tail -> int64?
  (define/contract (interp-values-lang-tail env t)
    (-> dict? any/c int64?)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (interp-values-lang-tail (let->new-env env xs vs) tail)]
      [`(if ,pred ,t1 ,t2)
        (interp-values-lang-tail
          env
          (if (interp-values-lang-pred env pred)
            t1
            t2))]
      ;; value
      [_ (interp-values-lang-value env t)]))

  ;; dict? values-lang-v4-value -> int64?
  (define/contract (interp-values-lang-value env v)
    (-> dict? any/c int64?)
    (match v
      [`(let ([,xs ,vs] ...) ,value)
        (interp-values-lang-value (let->new-env env xs vs) value)]
      [`(if ,pred ,v1 ,v2)
        (interp-values-lang-value
          env
          (if (interp-values-lang-pred env pred)
            v1
            v2))]
      [`(,binop ,t1 ,t2)
        ((interp-values-lang-binop binop)
         (interp-values-lang-tail env t1)
         (interp-values-lang-tail env t2))]
      ;; triv
      [_ (interp-values-lang-triv env v)]))

  ;; dict? values-lang-v4-triv -> int64?
  (define/contract (interp-values-lang-triv env t)
    (-> dict? any/c int64?)
    (match t
      [(? int64?) t]
      [(? name?) (dict-ref env t)]))

  ;; Returns the corresponding procedure for relop.
  ;;
  ;; b: values-lang-v4-relop
  (define/contract (interp-values-lang-relop relop)
    (-> symbol? procedure?)
    (match relop
      ['< <]
      ['<= <=]
      ['= =]
      ['>= >=]
      ['> >]
      ;; TODO: Is there a built in function??
      ['!= (lambda (x y) (not (= x y)))]))

  ;; Returns the corresponding procedure for b.
  ;;
  ;; b: values-lang-v4-binop
  (define/contract (interp-values-lang-binop b)
    (-> symbol? procedure?)
    (match b
      ['* *]
      ['+ +]))

  (interp-values-lang-p '() p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (define result (interp-values-lang p))
    (check-equal? result 42)
    (check-equal? result (interp-values-lang-v4 p)))


  (check-42
    '(module 42))

  (check-42
    '(module
       (let ([x 30]
             [y 12])
         (if (true) (if (not (true)) (* x y) (+ x y)) 1))))

  (check-42
    '(module
       (let ([x (if (<= 2 3) 4 5)]
             [y (let ([y 38]) y)])
         (+ x y))))

  (check-42
    '(module
      (if (if (false) (true) (false))
        0
        (let ([to 21]
              [two 2])
          (* to two)))))

  (check-42
    '(module
       (let ([x 42])
         (let ()
           (if (let ([x 2] [y 4]) (> x y))
             0
             42)))))

  (check-42
    '(module
       (if (!= 10 0)
         42
         1)))

  (check-42
    '(module
       (if (< 10 0)
         2
         (if (let ([x (if (if (true) (>= 8 9) (true)) 0 10)]
                   [y 10])
               (= x y))
           42
           0))))

  )

