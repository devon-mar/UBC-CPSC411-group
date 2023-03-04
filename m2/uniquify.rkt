#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide uniquify)

;; Milestone 2 Exercise 1
;; Milestone 4 Exercise 20
;;
;; Compiles Values-lang v4 to Values-unique-lang v4 by resolving all lexical
;; identifiers to abstract locations.
(define/contract (uniquify p)
  (-> values-lang-v4? values-unique-lang-v4?)

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))

  ;; Alocates an aloc for every name in xs, returning the allocations.
  ;; Uses alocs as the base.
  ;;
  ;; alocs: mapping of lexical identifier to name
  ;; xs: list of names that need a lexical identifier assigned
  ;; -> mapping of lexical identifier to name
  (define/contract (set-alocs-let alocs xs)
    (-> dict? (listof name?) dict?)
    (foldl
      (lambda (x acc) (dict-set acc x (fresh x)))
      alocs
      xs))

  ;; Reaplce all [xs vs] declarations
  ;; using the mappings in new-alocs.
  ;; Alocs is used for the values.
  ;;
  ;; new-alocs: mapping of lexical identifier to aloc for the xs.
  ;; alocs: mapping of lexical identifier to aloc for vs.
  ;; xs: list of name
  ;; vs: list of values corresponding to each x in xs
  ;; -> list of bindings to go in a let
  (define/contract (uniquify-let-decl new-alocs alocs xs vs)
    (-> dict? dict? (listof name?) (listof any/c) (listof (cons/c aloc? any/c)))
    (map (lambda (x v) `[,(dict-ref new-alocs x) ,(uniquify-value alocs v)]) xs vs))

  ;; Resolves all lexical identifiers in a tail to alocs.
  ;;
  ;; alocs: mapping of name to aloc
  ;; dict? values-lang-v3-tail -> values-unique-lang-v3-tail
  (define/contract (uniquify-tail alocs t)
    (-> dict? any/c any/c)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (define new-alocs (set-alocs-let alocs xs))
        `(let ,(uniquify-let-decl new-alocs alocs xs vs) ,(uniquify-tail new-alocs tail))]
      [v (uniquify-value alocs v)]))

  ;; Resolves all lexical identifiers in a triv to alocs.
  ;;
  ;; values-lang-v3-triv -> values-unique-lang-v3-triv.
  (define/contract (uniquify-triv alocs t)
    (-> dict? any/c any/c)
    (match t
      [int64 #:when (int64? int64) int64]
      [x #:when (name? x) (dict-ref alocs x)]))

  ;; Resolves all lexical identifiers in a value to alocs.
  ;;
  ;; alocs: mapping of name to aloc
  ;;
  ;; dict? values-lang-v3-value -> values-unique-lang-v3-value
  (define/contract (uniquify-value alocs v)
    (-> dict? any/c any/c)
    (match v
      [triv #:when (triv? triv) (uniquify-triv alocs triv)]
      [`(let ([,xs ,vs] ...) ,value)
        (define new-alocs (set-alocs-let alocs xs))
        `(let ,(uniquify-let-decl new-alocs alocs xs vs) ,(uniquify-value new-alocs value))]
      [`(,binop ,t1 ,t2)
        `(,binop ,(uniquify-triv alocs t1) ,(uniquify-triv alocs t2))]))

  ;; Compiles Values-lang v3 to Values-unique-lang v3 by resolving all lexical
  ;; identifiers to abstract locations.
  ;;
  ;; alocs: dict?
  ;; p: values-lang-v3-p
  ;; -> values-unique-lang-v3-p
  (define/contract (uniquify-p alocs p)
    (-> dict? any/c any/c)
    (match p
      [`(module ,tail)
        `(module ,(uniquify-tail alocs tail))]))

  (uniquify-p '() p))

(module+ test
  (require
   rackunit)

  (define (check-42 p)
    (check-equal?
      (interp-values-unique-lang-v4 (uniquify p))
      42))

  ; simple
  (check-42 '(module (+ 40 2)))

  ; one var
  (check-42 '(module (let ([x 42]) x)))


  ; nested let
  (check-42
    '(module
     (let ([x 40])
       (let ([y 2])
         (+ x y)))))

  ; shadow decl
  ; let in tail of let
  (check-42
      '(module
        (let ([x 20])
          (let ([x 21])
            (+ x x)))))

  ; let in value of let decl
  (check-42
    '(module
       (let ([x 2]
             [y (let ([x 38]) (+ 2 x))])
         (+ x y))))


  ; nesting and shadowing
  (check-42
    '(module
       (let ([foo 20])
         (let ([foo (+ 1 foo)]
               [bar (+ 1 foo)])
           (+ foo bar)))))
  )

