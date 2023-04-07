#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide check-values-lang)

;; Milestone 5 Exercise 1
;;
;; Validates that the Values-lang v5 is syntactically well-formed, well bound
;; and well typed: all procedure calls pass the correct number of arguments,
;; and all binop and relop are never used with labels.
(define/contract (check-values-lang p)
  (-> any/c values-lang-v5?)

  ;; Throws an error if p is an invalid values-lang-v5 procedure.
  ;; Otherwise returns p if valid.
  (define (check-values-lang-p p)
    (match p
      [`(module (define ,x1s (lambda (,x2s ...) ,tails)) ... ,tail)
        (for ([name x1s]
              [params x2s]
              [tail tails])
          (check-syntax-proc name params tail))

        (define env (procs->env x1s x2s))
        (for ([params x2s]
              [tail tails])
          (type-check-proc env params tail))

        (check-syntax-tail tail)
        (type-check-tail env tail)
        p]
      [_ (error "invalid program syntax")]))

  ;; Checks the syntax of a proc with name params and tail.
  ;; Returns void or throws an error.
  (define (check-syntax-proc name params tail)
    (unless (name? name)
      (error "invalid procedure name " name))
    (for ([p params])
      (unless (name? p)
        (error "invalid param name " p)))
    (check-syntax-tail tail))

  ;; Checks the syntax of a let with xs and vs.
  ;; Returns void or throws an error.
  (define (check-syntax-let xs vs)
    (unless (andmap name? xs)
      (error "invalid xs in let"))
    (for ([v vs]) (check-syntax-value v)))

  ;; Checks the syntax of a pred.
  ;; Returns void or throws an error.
  (define (check-syntax-pred p)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,p)
        (check-syntax-pred p)]
      [`(let ([,xs ,vs] ...) ,pred)
        (check-syntax-let xs vs)
        (check-syntax-pred pred)]
      [`(if ,p1 ,p2 ,p3)
        (check-syntax-pred p1)
        (check-syntax-pred p2)
        (check-syntax-pred p3)]
      [`(,relop ,t1 ,t2)
        (check-syntax-relop relop)
        (check-syntax-tail t1)
        (check-syntax-tail t2)]
      [_ (error "invalid pred" p)]))

  ;; Checks the syntax of a tail.
  ;; Returns void or throws an error.
  (define (check-syntax-tail t)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (check-syntax-let xs vs)
        (check-syntax-tail tail)]
      [`(if ,p ,t1 ,t2)
        (check-syntax-pred p)
        (check-syntax-tail t1)
        (check-syntax-tail t2)]
      [`(call ,x ,ts ...)
        (unless (name? x)
          (error "invalid name in call" x))
        (for ([t ts]) (check-syntax-triv t))]
      [_ (check-syntax-value t)]))

  ;; Checks the syntax of a value.
  ;; Returns void or throws an error.
  (define (check-syntax-value v)
    (match v
      [`(if ,p ,v1 ,v2)
        (check-syntax-pred p)
        (check-syntax-value v1)
        (check-syntax-value v2)]
      [`(let ([,xs ,vs] ...) ,v)
        (check-syntax-let xs vs)
        (check-syntax-value v)]
      [`(,binop ,t1 ,t2)
        (check-syntax-binop binop)
        (check-syntax-triv t1)
        (check-syntax-triv t2)]
      [_ (check-syntax-triv v)]))

  ;; Checks the syntax of a triv
  ;; Returns void or throws an error.
  (define (check-syntax-triv t)
    (match t
      [(? int64?) (void)]
      [(? name?) (void)]
      [_ (error "invalid triv" t)]))

  ;; Checks the syntax of a binop.
  ;; Returns void or throws an error.
  (define (check-syntax-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      [_ (error "invalid binop")]))

  ;; Checks the syntax of a relop.
  ;; Returns void or throws an error.
  (define (check-syntax-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]
      [_ (error "invalid relop")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; type checking stuff ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; All of these functions assume VALID syntax.
  ;;
  ;; env is a dict of name? to either:
  ;; - int64-t
  ;; - any-t
  ;; - the number of arguments of the function that name points to

  ;; symbol to represent an int
  (define int64-t 'int64-t)
  (define any-t 'any-t)
  (define/contract (any-t? a)
    (-> any/c boolean?)
    (equal? any-t a))
  ;; Return true if i is an int or any.
  (define/contract (int64-t? i)
    (-> any/c boolean?)
    (or (equal? i int64-t)
        (any-t? i)))

  ;; Add the procedures represented by names and params
  ;; to a new environment.
  (define/contract (procs->env names params)
    (-> (listof name?) (listof (listof name?)) dict?)
    (foldl 
      (lambda (name params acc) (dict-set acc name (length params)))
        '()
        names
        params))

  ;; Types check a procedure with params and tail.
  ;; Returns void or throws an error.
  (define/contract (type-check-proc env params tail)
    (-> dict? (listof name?) any/c void)
    (check-duplicate-names params)
    (type-check-tail
      (foldl (lambda (p acc) (dict-set acc p any-t)) env params)
      tail))

  ;; Raises an error if names contains duplicate names.
  (define/contract (check-duplicate-names names)
    (-> (listof name?) void?)
    (if (and (check-duplicates names) #t)
      (error "Duplicate parallel bindings found")
      (void)))

  ;; Returns a new environment from the xs and vs of a let.
  ;; If invalid throws an error.
  (define/contract (let->env env xs vs)
    (-> dict? (listof name?) any/c dict?)
    (check-duplicate-names xs)
    (foldl
      (lambda (x v acc) (dict-set acc x (type-check-value env v)))
      env
      xs
      vs))

  ;; Type checks pred p.
  ;; Returns void or throws an error.
  (define/contract (type-check-pred env p)
    (-> dict? any/c void)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,p)
        (type-check-pred env p)]
      [`(let ([,xs ,vs] ...) ,pred)
        (type-check-pred (let->env env xs vs) pred)]
      [`(if ,p1 ,p2 ,p3)
        (type-check-pred env p1)
        (type-check-pred env p2)
        (type-check-pred env p3)]
      [`(,_ ,t1 ,t2)
        (unless (int64-t? (type-check-triv env t1))
          (error "relop only works on int64s:" t1))
        (unless (int64-t? (type-check-triv env t2))
          (error "relop only works on int64s:" t2))]))

  ;; Returns something representing the type of t that can go in an env (see above comment).
  (define/contract (type-check-tail env t)
    (-> dict? any/c void)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (type-check-tail (let->env env xs vs) tail)]
      [`(if ,p ,t1 ,t2)
        (type-check-pred env p)
        (type-check-tail env t1)
        (type-check-tail env t2)]
      [`(call ,x ,ts ...)
        (unless (dict-has-key? env x)
          (error "call to undefined procedure" x))
        (unless (number? (dict-ref env x))
          (error "not a procedure " x))
        (unless (equal? (length ts) (dict-ref env x))
          (error "unexpected number of arguments to procedure" x))]
      [_
        (unless (int64-t? (type-check-value env t))
          (error "tail doesn't return an int" t))]))

  ;; Returns something representing the type of v that can go in an env (see above comment).
  (define/contract (type-check-value env v)
    (-> dict? any/c (or/c symbol? number?))
    (match v
      [`(if ,p ,v1 ,v2)
        (type-check-pred env p)
        (define typ1 (type-check-value env v1))
        (define typ2 (type-check-value env v2))
        (if (equal? typ1 typ2)
          typ1
          (error "if branches returns different types"))]
      [`(let ([,xs ,vs] ...) ,v)
        (type-check-value (let->env env xs vs) v)]
      [`(,_ ,t1 ,t2)
        (unless (int64-t? (type-check-triv env t1))
          (error "binop only works on int64s:" t1))
        (unless (int64-t? (type-check-triv env t2))
          (error "binop only works on int64s:" t2))
        int64-t]
      [_ (type-check-triv env v)]))

  (define (type-check-triv env t)
    (match t
      [(? int64?) int64-t]
      [(? name?) (dict-ref env t)]))

  (check-values-lang-p p))

(module+ test
  (require rackunit)

  (define-check (check-valid p)
    (check-equal? (check-values-lang p) p)
    (check-true (values-lang-v5? p)))

  (define-check (check-invalid p)
    (check-exn exn:fail? (lambda () (check-values-lang p) p)))

  ;; From M2
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
  (check-invalid
    '(notmodule 42))

  ;; x not defined
  (check-invalid
    '(module x))
  (check-invalid
    '(module (let ([x "not a number"]) x)))
  (check-invalid
    '(module (/ 40 2)))

  ;; invalid name
  (check-invalid
    '(module (let (["x" 42]) x)))

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
         y)))


  ;; M3 tests
  (check-valid
    '(module
       (if (if (not (false)) (true) (= 1 2))
         42
         42)))

  ;; invalid relop
  (check-invalid
    '(module
       (if (== 1 2)
         0
         42)))

  ;; invalid pred
  (check-invalid
    '(module
       (if (t)
         0
         42)))

  ;; duplicate binding in pred let
  (check-invalid
    '(module
       (if (let ([x 10]
                 [x 2])
             (= x 2))
         0
         42)))

  ;; undefined var in pred let
  (check-invalid
    '(module
       (if (let ([x 10]
                 [z 2])
             (= y 2))
         0
         42)))

  ;; value if
  (check-valid
    '(module
       (let ([x (if (true) 42 0)])
         x)))

  ;; M5 tests
  (check-valid
    '(module
       (define foo (lambda () 42))
       (call foo)))
  ;; duplicate params
  (check-invalid
    '(module
       (define foo (lambda (x x) 42))
       (call foo)))

  ;; can't return a function
  (check-invalid
    '(module
       (define foo (lambda () 42))
       foo))

  ;; proc to (binop triv triv)
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x foo]
             [y 42])
         (+ x y))))
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x foo]
             [y 42])
         (+ y x))))

  ;; proc to (relop triv triv)
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x foo]
             [y 42])
         (if (= x y)
           42
           0))))
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x foo]
             [y 42])
         (if (= y x)
           42
           0))))

  ;; value/if different return types
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x foo]
             [y 42]
             [z (if (= 1 2) 1 foo)])
         y)))

  ;; wrong tail return type in if
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x foo]
             [y 42])
         (if (= 1 2)
           x
           y))))
  ;; foo doesn't return an int64
  (check-invalid
    '(module
       (define bar (lambda () 42))
       (define foo (lambda () bar))
       (call foo)))

  ;; int64 to call
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (call 42)))

  ;; wrong number of args
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (call foo 42)))
  (check-invalid
    '(module
       (define foo (lambda (x y) 42))
       (call foo 42)))
  (check-invalid
    '(module
       (define foo (lambda (x y) 42))
       (call foo 42 43 44)))

  ;; right number of args
  (check-valid
    '(module
       (define foo (lambda (x y) 42))
       (call foo 42 0)))

  ;; invalid proc name
  (check-invalid
    '(module
       (define 42 (lambda () 42))
       (call 42)))

  ;; args not name?
  (check-invalid
    '(module
       (define foo (lambda (42) 42))
       (call foo 1)))

  ;; duplicate params
  (check-invalid
    '(module
       (define foo (lambda (p p) 42))
       (call foo 1 1)))

  ;; valid relops and binops
  (check-valid
    '(module
      (define le (lambda (x y) (if (<= x y) (* 42 0) (* 21 2))))
      (define lt (lambda (x y) (if (< x y) 42 0)))
      (define gt (lambda (x y) (if (> x y) 42 0)))
      (define ge (lambda (x y) (if (>= x y) 42 0)))
      (define eq (lambda (x y) (if (= x y) 42 0)))
      (define ne (lambda (x y) (if (!= x y) 42 0)))
      (call le 1 2)))

  ;; yes, this is technically wrong
  (check-valid
    '(module
       (define bar (lambda (x) x))
       (define foo (lambda (x) (call bar x)))
       (call foo 42)))

  ;; undefined in proc
  (check-invalid
    '(module
       (define foo (lambda (x) y))
       (let ([y 42])
         (call foo y))))

  ;; call to undefined proc
  (check-invalid
    '(module (call foo)))

  ;; call to a number
  (check-invalid
    '(module
       (define foo (lambda () 42))
       (let ([x 42])
         (call x))))

  )
