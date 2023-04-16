#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide check-exprs-lang)

;; Milestone 7 Exercise 2
;; Milestone 8 Exercise 1
;; Milestone 9 Exercise 1
;;
;; Validates that input is a well-bound Exprs-lang v9 program.
;; There are no other static restrictions.
;;
;; Implementation details:
;; Most functions take a parameter named env that is a set of defined names.
(define/contract (check-exprs-lang p)
  (-> any/c exprs-lang-v9?)

  (define/contract (prim-f? p)
    (-> any/c boolean?)
    (and (memq p '(*
                   +
                   -
                   <
                   <=
                   >
                   >=
                   eq?
                   fixnum?
                   boolean?
                   empty?
                   void?
                   ascii-char?
                   error?
                   not
                   pair?
                   procedure?
                   vector?
                   cons
                   car
                   cdr
                   make-vector
                   vector-length
                   vector-set!
                   vector-ref
                   procedure-arity))
         #t))

  ;; Returns true if x is a valid x. Otherwise false.
  (define/contract (x? x)
    (-> any/c boolean?)
    (or (name? x) (prim-f? x)))

  ;; Throws an error if the procedure represented by
  ;; x params and value is invalid.
  (define/contract (check-exprs-lang-proc env params value)
    (-> generic-set? (listof any/c) any/c void?)

    (for ([p params])
      (unless (x? p)
        (error "invalid param:" p)))

    (check-duplicate-bindings params)

    (check-exprs-lang-value (set-union env params) value))

  ;; p: any/c
  ;; -> exprs-lang-v9?
  ;;
  ;; Raises an error if p is an invalid exprs-lang-p
  (define/contract (check-exprs-lang-p p)
    (-> any/c exprs-lang-v9?)
    (match p
      [`(module (define ,xs (lambda (,params ...) ,vs)) ... ,v)
        (unless (andmap name? xs)
          (error "invalid xs in defines:" xs))
        (check-duplicate-bindings xs)

        (for ([params params]
              [v vs])
          (check-exprs-lang-proc xs params v))

        (check-exprs-lang-value xs v)
        p]
      [_ (error "invalid exprs-lang program")]))

  ;; Throws an error if names contains duplicate xs.
  (define/contract (check-duplicate-bindings xs)
    (-> list? void?)
    (when (and (check-duplicates xs) #t)
      (error "duplicate bindings found:" xs)))

  ;; Throws an error if v is an invalid exprs-lang-v9-value.
  (define/contract (check-exprs-lang-value env v)
    (-> generic-set? any/c void?)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
        (for ([x xs]) (unless (x? x) (error "invalid x in let:" x)))
        (for ([v vs]) (check-exprs-lang-value env v))
        (check-duplicate-bindings xs)
        (check-exprs-lang-value (set-union env xs) v)]
      [`(if ,v1 ,v2 ,v3)
        (check-exprs-lang-value env v1)
        (check-exprs-lang-value env v2)
        (check-exprs-lang-value env v3)]
      [`(call ,v ,vs ...)
        (check-exprs-lang-value env v)
        (for ([v vs]) (check-exprs-lang-value env v))]
      ;; triv
      [_ (check-exprs-lang-triv env v)]))

  ;; Throws an error if v is an invalid exprs-lang-v9-triv.
  (define/contract (check-exprs-lang-triv env t)
    (-> generic-set? any/c void?)
    (match t
      [(? fixnum?) (void)]
      [#t (void)]
      [#f (void)]
      ['empty (void)]
      ['(void) (void)]
      [`(error ,uint8)
        (unless (uint8? uint8)
          (error "invalid uint8:" uint8))]
      [(? ascii-char-literal?) (void)]
      [`(lambda (,xs ...) ,v)
        (for ([x xs])
          (unless (x? x)
            (error "invalid x in lambda params" x)))
        (check-duplicate-bindings xs)
        (check-exprs-lang-value (set-union xs env) v)]
      ;; x
      [_ (check-exprs-lang-x env t)]))

  ;; Throws an error if v is an invalid exprs-lang-v9-x or if x is an undefined name.
  (define/contract (check-exprs-lang-x env x)
    (-> generic-set? any/c void?)
    (match x
      [(? prim-f?) (void)]
      [(? name?)
       (unless (set-member? env x)
         (error "undefined name:" x))]
      [_ (error "invalid x:" x)]))

  ;; not used
  #;
  (define (check-exprs-lang-prim-f p)
    (match p
      ['*
       (void)]
      ['+
       (void)]
      ['-
       (void)]
      ['<
       (void)]
      ['<=
       (void)]
      ['>
       (void)]
      ['>=
       (void)]
      ['eq?
       (void)]
      ['fixnum?
       (void)]
      ['boolean?
       (void)]
      ['empty?
       (void)]
      ['void?
       (void)]
      ['ascii-char?
       (void)]
      ['error?
       (void)]
      ['not
       (void)]
      ['pair?
       (void)]
      ['procedure?
       (void)]
      ['vector?
       (void)]
      ['cons
       (void)]
      ['car
       (void)]
      ['cdr
       (void)]
      ['make-vector
       (void)]
      ['vector-length
       (void)]
      ['vector-set!
       (void)]
      ['vector-ref
       (void)]
      ['procedure-arity
       (void)]))

  (check-exprs-lang-p p))

(module+ test
  (require rackunit)

  (define-check (check-valid p)
    (check-equal? (check-exprs-lang p) p)
    (check-true (exprs-lang-v9? p)))

  (define-check (check-invalid p)
    (check-exn exn:fail? (lambda () (check-exprs-lang p) p)))

  ;; basic trivs
  (check-valid '(module 42))
  (check-valid '(module #t))
  (check-valid '(module #f))
  (check-valid '(module empty))
  (check-valid '(module (void)))
  (check-valid '(module (error 42)))
  (check-valid '(module #\a))
  (check-valid '(module +))
  (check-valid '(module (lambda (x) x)))
  (check-valid '(module procedure?))
  (check-valid '(module procedure-arity))

  ;; not a uint8
  (check-invalid '(module (error 100000)))

  ;; undefined name in lambda
  (check-invalid
    '(module
       (lambda (x y)
         z)))

  (check-valid
    '(module
       (define none (lambda () 42))
       (define one (lambda (x) x))
       (define two (lambda (x y) x))
       (let ([x (call one 1)]
             ;; wrong arity but we ignore
             [y (call two 2)]
             [z (call none)])
         z)))

  ;; duplicate proc defs
  (check-invalid
    '(module
       (define one (lambda () 42))
       (define one (lambda () 42))
       42))

  ;; duplicate bindings in let
  (check-invalid
    '(module
       (let ([x 1]
             [x 2])
         x)))

  ;; invalid x in let
  (check-invalid
    '(module
       (let ([1 1])
         1)))

  (check-invalid
    '(module
       (define 1 (lambda () 42))
       42))

  (check-invalid
    '(module
       (call undefined 1)))

  (check-invalid 42)

  (check-invalid
    '(module
       (let ([x 1])
         ;; y is undefined
         y)))

  (check-invalid
    '(module
       (let ([z 1]
             ;; z undefined
             [x z])
         x)))

  (check-valid
    '(module
       (let ([x 42])
         (let ([y x])
           y))))

  (check-invalid
    '(module
       ;; invalid params
       (define x (lambda (1 2 3) 42))
       42))

  ;; invalid params in lambda
  (check-invalid
    '(module
       (let ([x (lambda (1 2 3) 1)])
         (call x 1 2 3))))

  ;; invalid params in define
  (check-invalid
    '(module
       (define x (lambda (1 2 3) 1))
       (call x 1 2 3)))

  ;; undefined x in if cond
  (check-invalid
    '(module
       (if x
         1
         2)))
  (check-invalid
    '(module
       (if #t
         x
         2)))
  (check-invalid
    '(module
       (if #t
         0
         x)))

  (check-invalid
    '(module "x"))

  (define-syntax-rule (prim-f-test prim-f args ...)
    (check-valid '(module (call prim-f args ...))))

  (prim-f-test * 1 2)
  (prim-f-test + 2 3)
  (prim-f-test - 1 2)
  (prim-f-test < 4 5)
  (prim-f-test <= 6 7)
  (prim-f-test > 8 9)
  (prim-f-test >= 10 11)
  (prim-f-test eq? 1 2)
  (prim-f-test fixnum? 1)
  (prim-f-test boolean? 2)
  (prim-f-test empty? 3)
  (prim-f-test void? 4)
  (prim-f-test ascii-char? 5)
  (prim-f-test error? 6)
  (prim-f-test not 7)
  (prim-f-test pair? 8)
  (prim-f-test vector? 9)
  (prim-f-test cons 10)
  (prim-f-test car 11)
  (prim-f-test cdr 12)
  (prim-f-test make-vector 8)
  (prim-f-test vector-length 1)
  (prim-f-test vector-set! 1 2 3)
  (prim-f-test vector-ref 1 2 3)

  )
