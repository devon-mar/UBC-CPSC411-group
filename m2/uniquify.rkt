#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(provide uniquify)

;; Milestone 2 Exercise 1
;; Milestone 4 Exercise 20
;; Milestone 5 Exercise 2
;; Milestone 6 Exercise 2
;; Milestone 7 Exercise 3
;;
;; Compiles Exprs-Lang to Exprs-Unique-Lang by
;; resolving top-level lexical identifiers into unique labels,
;; and all other lexical identifiers into unique abstract locations
(define/contract (uniquify p)
  (-> exprs-lang-v7? exprs-unique-lang-v7?)

  ;; value values-lang-v6-value
  ;; -> values-unique-lang-v6-value
  (define/contract (uniquify-define dict name params value)
    (-> dict? name? (listof name?) any/c any/c)
    (define new-dict (foldl (lambda (x acc) (dict-set acc x (fresh x))) dict params))
    `(define
       ,(dict-ref dict name)
       (lambda
         ,(map (lambda (p) (dict-ref new-dict p)) params)
         ,(uniquify-value new-dict value))))

  ;; Replace all xs with alocs. The new name->aloc|label mappings will be passed
  ;; as the first arg to f. The second arg to f will be body.
  ;; The value of (f new-dict body) will be used as the body in the returned let.
  (define/contract (uniquify-let dict xs vs f body)
    (-> dict? (listof name?) (listof any/c) (-> dict? any/c any/c) any/c any/c)
    (define new-dict (foldl (lambda (x acc) (dict-set acc x (fresh x))) dict xs))
    `(let
      ,(map (lambda (x v) `[,(dict-ref new-dict x) ,(uniquify-value dict v)]) xs vs)
      ,(f new-dict body)))

  ;; dict(name?, aloc?|label?) values-lang-v6-p -> values-unique-lang-v6-p
  (define (uniquify-p dict p)
    (-> dict? any/c any/c)
    (match p
      [`(module (define ,x1s (lambda (,x2s ...) ,vs)) ... ,vt)
       (define new-dict (foldl (lambda (x acc) (dict-set acc x (fresh-label x))) dict x1s))
       `(module
         ,@(map (lambda (name x2s value) (uniquify-define new-dict name x2s value)) x1s x2s vs)
         ,(uniquify-value new-dict vt))]))

  ;; dict(name?, aloc?|label?) values-lang-v6-pred -> values-unique-lang-v6-pred
  (define/contract (uniquify-pred dict p)
    (-> dict? any/c any/c)
    (match p
      [`(true)
       p]
      [`(false)
       p]
      [`(not ,pred)
       `(not ,(uniquify-pred dict pred))]
      [`(let ([,xs ,vs] ...) ,pred)
       (uniquify-let dict xs vs uniquify-pred pred)]
      [`(if ,p1 ,p2 ,p3)
       `(if
         ,(uniquify-pred dict p1)
         ,(uniquify-pred dict p2)
         ,(uniquify-pred dict p3))]
      [`(,relop ,t1 ,t2)
       `(,relop
         ,(uniquify-triv dict t1)
         ,(uniquify-triv dict t2))]))

  ;; dict(name?, aloc?|label?) values-lang-v6-value -> values-unique-lang-v6-value
  (define/contract (uniquify-value dict v)
    (-> dict? any/c any/c)
    (match v
      [`(if ,p ,v1 ,v2)
       `(if
         ,(uniquify-pred dict p)
         ,(uniquify-value dict v1)
         ,(uniquify-value dict v2))]
      [`(let ([,xs ,vs] ...) ,v)
       (uniquify-let dict xs vs uniquify-value v)]
      [`(call ,x ,vs ...)
       `(call
         ,(dict-ref dict x)
         ,@(map (lambda (v) (uniquify-value dict v)) vs))]
      [`(,binop ,v1 ,v2)
       `(,binop
         ,(uniquify-value dict v1)
         ,(uniquify-value dict v2))]
      [_ (uniquify-triv dict v)]))

  ;; dict(name?, aloc?|label?) values-lang-v6-triv -> values-unique-lang-v6-triv
  (define/contract (uniquify-triv dict t)
    (-> dict? any/c any/c)
    (match t
      [(? int64?) t]
      [(? name?) (dict-ref dict t)]))

  #;
  (define (uniquify-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))

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
      (interp-exprs-unique-lang-v7 (uniquify p))
      42))

  ;; M5 tests

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


  ;; M4 tests

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

  ;; M6 tests
  ;; minus
  (check-42 '(module (- -20 -62)))

  ;; call as value
  (check-42
    '(module
      (define fact
        (lambda (n acc)
          (if (<= n 1)
              acc
              (let ([n (- n 1)]
                    [acc (* n acc)])
                (let ([r (call fact n acc)]) r)))))
      (let ([x (call fact 4 1)]) (+ x 18))))
  )
