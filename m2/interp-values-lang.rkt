#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v3)

(provide interp-values-lang)

(define/contract (interp-values-lang p)
  (-> values-lang-v3? int64?)

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))

  ;; Returns a int64 from t.
  (define/contract (interp-values-lang-triv env t)
    (-> dict? triv? int64?)
    (match t
      [(? int64?) t]
      ;; name
      [_ (dict-ref env t)]))

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

  ;; Returns the result of interpreting t.
  ;;
  ;; env: dict?
  ;; t: tail
  ;; -> int64?
  (define/contract (interp-values-lang-tail env t)
    (-> dict? any/c int64?)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (interp-values-lang-tail (let->new-env env xs vs) tail)]
      [v (interp-values-lang-value env v)]))

  ;; Returns the racket procedure for b.
  (define/contract (binop->proc b)
    (-> binop? procedure?)

    (match b
      ['* *]
      ['+ +]))

  ;; Returns the result of interpreting v.
  ;;
  ;; env: dict?
  ;; v: value
  ;; -> int64?
  (define/contract (interp-values-lang-value env v)
    (-> dict? any/c int64?)
    (match v
      [(? triv?) (interp-values-lang-triv env v)]
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        ((binop->proc binop) (interp-values-lang-triv env t1) (interp-values-lang-triv env t2))]
      [`(let ([,xs ,vs] ...) ,value)
        (interp-values-lang-value (let->new-env env xs vs) value)]))

  ;; Returns the result of interpreting p.
  ;;
  ;; env: dict?
  ;; p: p
  ;; -> int64?
  (define/contract (interp-values-lang-p env p)
    (-> dict? any/c int64?)
    (match p
      [`(module ,tail)
        (interp-values-lang-tail env tail)]))

  (interp-values-lang-p '() p))

(module+ test
  (require rackunit)

  (define (check-42 p)
    (check-equal?
      (interp-values-lang p)
      42))

  (check-42
    '(module 42))
  (check-42
    '(module (+ 40 2)))
  (check-42
    '(module (* 21 2)))
  (check-42
    '(module (let ([x 42]) x)))
  (check-42
    '(module (let ([x (+ 40 2)]) x)))

  ;; lexical scope
  (check-42
    '(module
       (let ([x 40]
             [y 20])
         (let ([y 2])
           (+ x y)))))

  ;; test shadowing
  (check-42
    '(module
       (let ([x 42]
             [y (let ([x 20]) x)])
       x)))
  ;; let nesting
  (check-42
    '(module
       (let ([y 42])
         (let ([x y])
           x))
       )))
