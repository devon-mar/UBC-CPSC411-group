#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide uniquify)

;; Milestone 2 Exercise 1
;; Milestone 4 Exercise 20
;; Milestone 5 Exercise 2
;;
;; Compiles Values-lang v5 to Values-unique-lang v5 by resolving all lexical
;; identifiers to abstract locations.
(define/contract (uniquify p)
  (-> values-lang-v5? values-unique-lang-v5?)

  ;; tail: values-lang-v5-tail
  ;; -> values-unique-lang-v5-tail
  (define/contract (uniquify-define alocs name params tail)
    (-> dict? name? (listof name?) any/c any/c)
    (define new-alocs (foldl (lambda (x acc) (dict-set acc x (fresh x))) alocs params))
    `(define
       ,(dict-ref alocs name)
       (lambda
         ,(map (lambda (p) (dict-ref new-alocs p)) params)
         ,(uniquify-tail new-alocs tail))))

  ;; Replace all xs with alocs. The new name->aloc mappings will be passed
  ;; as the first arg to f. The second arg to f will be body.
  ;; The value of (f new-alocs body) will be used as the body in the returned let.
  (define/contract (uniquify-let alocs xs vs f body)
    (-> dict? (listof name?) (listof any/c) (-> dict? any/c any/c) any/c any/c)
    (define new-alocs (foldl (lambda (x acc) (dict-set acc x (fresh x))) alocs xs))
    `(let
       ,(map (lambda (x v) `[,(dict-ref new-alocs x) ,(uniquify-value alocs v)]) xs vs)
       ,(f new-alocs body)))
    

  ;; dict(name?, aloc?) values-lang-v5-p -> values-unique-lang-v5-p
  (define (uniquify-p alocs p)
    (-> dict? any/c any/c)
    (match p
      [`(module (define ,x1s (lambda (,x2s ...) ,tails)) ... ,tail)
        (define new-alocs (foldl (lambda (x acc) (dict-set acc x (fresh-label x))) alocs x1s))
        `(module
           ,@(map (lambda (name x2s tail) (uniquify-define new-alocs name x2s tail)) x1s x2s tails)
           ,(uniquify-tail new-alocs tail))]))

  ;; dict(name?, aloc?) values-lang-v5-pred -> values-unique-lang-v5-pred
  (define/contract (uniquify-pred alocs p)
    (-> dict? any/c any/c)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,pred)
        `(not ,(uniquify-pred alocs pred))]
      [`(let ([,xs ,vs] ...) ,pred)
        (uniquify-let alocs xs vs uniquify-pred pred)]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(uniquify-pred alocs p1)
           ,(uniquify-pred alocs p2)
           ,(uniquify-pred alocs p3))]
      [`(,relop ,t1 ,t2)
        `(,relop
          ,(uniquify-tail alocs t1)
          ,(uniquify-tail alocs t2))]))

  ;; dict(name?, aloc?) values-lang-v5-tail -> values-unique-lang-v5-tail
  (define/contract (uniquify-tail alocs t)
    (-> dict? any/c any/c)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (uniquify-let alocs xs vs uniquify-tail tail)]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(uniquify-pred alocs p)
           ,(uniquify-tail alocs t1)
           ,(uniquify-tail alocs t2))]
      [`(call ,x ,ts ...)
        `(call
           ,(dict-ref alocs x)
           ,@(map (lambda (t) (uniquify-triv alocs t)) ts))]
          
      [_ (uniquify-value alocs t)]))

  ;; dict(name?, aloc?) values-lang-v5-value -> values-unique-lang-v5-value
  (define/contract (uniquify-value alocs v)
    (-> dict? any/c any/c)
    (match v
      [`(if ,p ,v1 ,v2)
        `(if
           ,(uniquify-pred alocs p)
           ,(uniquify-value alocs v1)
           ,(uniquify-value alocs v2))]
      [`(let ([,xs ,vs] ...) ,v)
        (uniquify-let alocs xs vs uniquify-value v)]
      [`(,binop ,t1 ,t2)
        `(,binop
          ,(uniquify-triv alocs t1)
          ,(uniquify-triv alocs t2))]
      [_ (uniquify-triv alocs v)]))

  ;; dict(name?, aloc?) values-lang-v5-triv -> values-unique-lang-v5-triv
  (define/contract (uniquify-triv alocs t)
    (-> dict? any/c any/c)
    (match t
      [(? int64?) t]
      [(? name?) (dict-ref alocs t)]))

  #;
  (define (uniquify-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  
  #;
  (define (uniquify-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (uniquify-p '() p))

(module+ test
  (require
   rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-values-unique-lang-v5 (uniquify p))
      42))

  (check-42 '(module 42))
  (check-42 '(module (define foo (lambda () 42)) (call foo)))

  ;; proc with args
  (check-42
    '(module
       (define add (lambda (x y) (+ x y)))
       (call add 40 2)))

  ;; proc calling another proc
  (check-42
    '(module
       (define identity (lambda (x) x))
       (define foo
         (lambda (x y z)
           (let ([xy (+ x y)])
             ;; call "down"
             (call bar xy z))))
       (define bar
         (lambda (x y)
           (let ([xy (* x y)])
             ;; call "up'
             (call identity xy))))
       (call foo 20 1 2)))

  ;; M4 tests
  ;; M2 tests
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


  ;; M3 tests

  (check-42
    '(module
       ;; tail/(let ([x value] ...) tail)
       (let ([x 10]
             ;; pred/(not pred)
             ;; pred/(false)
             [y (if (not (false)) 20 10)]
             ;; values/(if pred value value)
             ;; pred/(true)
             [a (if (true) 42 0)]
             [b 1])
         ;; pred/(relop triv triv)
         (if (= x y)
           b
           a))))

  (check-42
    '(module
       ;; pred/(let ([x value] ...) pred)
       (if (let ([x 2]
                 [y 4])
             (< x y))
         42
         0)))

  (check-42
    ;; tail/value
    '(module 42))

  (check-42
    '(module
       ;; values/(let ([x value] ...) value)
       (let ([x (let ([y 40]) (+ y 1))]
             [y 1])
         ;; values/(binop triv triv)
         (+ x y))))

  (check-42
    '(module
       (if (if (true) (false) (true))
         20
         42)))
  )
