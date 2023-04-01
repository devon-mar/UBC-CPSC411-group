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

  ;; value exprs-lang-v7-value
  ;; -> exprs-unique-lang-v7-value
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

  ;; dict(name?, aloc?|label?) exprs-lang-v7-p -> exprs-unique-lang-v7-p
  (define/contract (uniquify-p dict p)
    (-> dict? any/c any/c)
    (match p
      [`(module (define ,x1s (lambda (,x2s ...) ,vs)) ... ,vt)
       (define new-dict (foldl (lambda (x acc) (dict-set acc x (fresh-label x))) dict x1s))
       `(module
         ,@(map (lambda (name x2s value) (uniquify-define new-dict name x2s value)) x1s x2s vs)
         ,(uniquify-value new-dict vt))]))

  ;; dict(name?, aloc?|label?) exprs-lang-v7-value -> exprs-unique-lang-v7-value
  (define/contract (uniquify-value dict v)
    (-> dict? any/c any/c)
    (match v
      [`(if ,vp ,v1 ,v2)
       `(if
         ,(uniquify-value dict vp)
         ,(uniquify-value dict v1)
         ,(uniquify-value dict v2))]
      [`(let ([,xs ,vs] ...) ,v)
       (uniquify-let dict xs vs uniquify-value v)]
      [`(call ,vc ,vs ...)
       `(call
         ,(uniquify-value dict vc)
         ,@(map (lambda (v) (uniquify-value dict v)) vs))]
      [_ (uniquify-triv dict v)]))

  ;; dict(name?, aloc?|label?) exprs-lang-v7-triv -> exprs-unique-lang-v7-triv
  (define/contract (uniquify-triv dict t)
    (-> dict? any/c any/c)
    ;; Merged template with x
    (match t
      ;; if prim-f|empty (or any symbol) gets shadowed by user-defined names
      ;; it should be replaced with a label|aloc
      [(? name?) (dict-ref dict t t)]
      [(? int61?) t]
      [#t t]
      [#f t]
      [`empty t]
      [`(void) t]
      [`(error ,_) t]
      [(? ascii-char-literal?) t]
      [prim-f prim-f]))

  #;
  (define (uniquify-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]
      ['eq? (void)]
      ['< (void)]
      ['<= (void)]
      ['>= (void)]
      ['> (void)]))

  #;
  (define (uniquify-unop u)
    (match u
      ['fixnum? (void)]
      ['boolean? (void)]
      ['empty? (void)]
      ['void? (void)]
      ['ascii-char? (void)]
      ['error? (void)]
      ['not (void)]))

  (uniquify-p '() p))

(module+ test
  (require
   rackunit)

  ;; Check that compiled program interprets to 42
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
       (define add (lambda (x y) (call + x y)))
       (call add 40 2)))

  ;; proc calling another proc
  (check-42
    '(module
       (define identity (lambda (x) x))
       (define foo
         (lambda (x y z)
           (let ([xy (call + x y)])
             ;; call "down"
             (call bar xy z))))
       (define bar
         (lambda (x y)
           (let ([xy (call * x y)])
             ;; call "up'
             (call identity xy))))
       (call foo 20 1 2)))

  ;; M2 tests
  ; simple
  (check-42 '(module (call + 40 2)))

  ; one var
  (check-42 '(module (let ([x 42]) x)))


  ; nested let
  (check-42
    '(module
       (let ([x 40])
         (let ([y 2])
           (call + x y)))))

  ; shadow decl
  ; let in tail of let
  (check-42
      '(module
        (let ([x 20])
          (let ([x 21])
            (call + x x)))))

  ; let in value of let decl
  (check-42
    '(module
       (let ([x 2]
             [y (let ([x 38]) (call + 2 x))])
         (call + x y))))


  ; nesting and shadowing
  (check-42
    '(module
       (let ([foo 20])
         (let ([foo (call + 1 foo)]
               [bar (call + 1 foo)])
           (call + foo bar)))))


  ;; M4 tests

  (check-42
    '(module
       ;; value/(let ([x value] ...) value)
       (let ([x 10]
             ;; value/(if (call not value) ...)
             [y (if (call not #f) 20 10)]
             ;; value/(if value value value)
             [a (if #t 42 0)]
             [b 1])
         ;; (if (call relop triv triv) ...)
         (if (call eq? x y)
           b
           a))))

  (check-42
    '(module
       ;; (if (let ([x value] ...) ...)
       (if (let ([x 2]
                 [y 4])
             (call < x y))
         42
         0)))

  (check-42
    ;; tail/value
    '(module 42))

  (check-42
    '(module
       ;; value/(let ([x value] ...) value)
       (let ([x (let ([y 40]) (call + y 1))]
             [y 1])
         ;; value/(call binop triv triv)
         (call + x y))))

  (check-42
    '(module
       (if (if #t #f #t)
         20
         42)))

  ;; M6 tests
  ;; minus
  (check-42 '(module (call - -20 -62)))

  ;; call as value
  (check-42
    '(module
      (define fact
        (lambda (n acc)
          (if (call <= n 1)
              acc
              (let ([n (call - n 1)]
                    [acc (call * n acc)])
                (let ([r (call fact n acc)]) r)))))
      (let ([x (call fact 4 1)]) (call + x 18))))

  ;; M7 tests
  ;; value in if
  (check-42
    '(module (if (error 1) (if empty 42 -2) -1)))

  ;; value in call
  (check-42
    '(module
      (call + (call * (call * (let ([x 2]) x) 4) (call (if #t - eq?) -17 5)) 218)))

  ;; binops
  (check-42
    '(module
      (let ([x (call + 4 2)]      ; +   =>    6
            [y (call * 5 10)]     ; *   =>   50
            [z (call - 9 2)])     ; -   =>  -18
        (let ([a (call eq? x 6)]  ; eq? => true
              [b (call < 0 z)]    ; <   => true
              [c (call <= y 50)]  ; <=  => true
              [d (call > -18 z)]  ; >   => false
              [e (call >= x 10)]) ; >=  => false
          (if a
              (if b
                  (if c
                      (if d
                          -4
                          (if e
                              -5
                              (call - y (call + 2 x))))
                      -3)
                  -2)
              -1)))))

  ;; unops + data types
  (check-42
    '(module
      (define all-true
        (lambda (a b c d e f g)
          (if a (if b (if c (if d (if e (if f (if g #t #f) #f) #f) #f) #f) #f) #f)))
      (let ([x 17]
            [y 2]
            [z -8])
        (let ([ft (call fixnum? x)]
              [bt (call boolean? #f)]
              [emt (call empty? empty)]
              [vt (call void? (void))]
              [at (call ascii-char? #\D)]
              [ert (call error? (error 2))]
              [nt (call not #f)])
          (let ([tr (if (call all-true ft bt emt vt at ert nt)
                        (call * x y)
                        -1)])
            (call - tr z))))))

  ;; shadowing
  (check-42
    '(module
      (define +
        (lambda (eq? >=)
          (let ([not *]
                [< >=])
            (call not eq? <))))
      (let ([- +]
            [fixnum? (call - 24 3)]
            [empty 2])
        (call - fixnum? empty))))
  )
