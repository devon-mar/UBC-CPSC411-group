#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide check-exprs-lang)

;; Milestone 7 Exercise 2
;; Milestone 8 Exercise 1
;;
;; Checks that a Exprs-lang v8 program is well typed (only that procedures are
;; called with the right number of arguments), and well scoped.
(define/contract (check-exprs-lang p)
  (-> any/c exprs-lang-v8?)

  ;; -> hash?
  (define-syntax-rule (make-proc-arities-dict [procs arities] ...)
    (let ([h (make-hash)])
      (for ([p (list 'procs ...)]
            [a (list 'arities ...)])
        (hash-set! h p a))
      h))

  (define proc-arities
    (make-proc-arities-dict
      [* 2]
      [+ 2]
      [- 2]
      [< 2]
      [eq? 2]
      [<= 2]
      [> 2]
      [>= 2]

      ;; unops
      [fixnum? 1]
      [boolean? 1]
      [empty? 1]
      [void? 1]
      [ascii-char? 1]
      [error? 1]
      [not 1]
      [pair? 1]
      [vector? 1]
      [cons 2]
      [car 1]
      [cdr 1]
      [make-vector 1]
      [vector-length 1]
      [vector-set! 3]
      [vector-ref 2]))

  (define/contract (prim-f? p)
    (-> any/c boolean?)
    (and
      (memq p '(*
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
                vector?
                cons
                car
                cdr
                make-vector
                vector-length
                vector-set!
                vector-ref))
      #t))

  ;; Throws an error if the procedure represented by
  ;; x params and value is invalid.
  (define/contract (check-exprs-lang-proc env params value)
    (-> generic-set? (listof any/c) any/c void?)
    (unless (andmap name? params)
      (error "invalid xs in procedure params " params))
    (check-duplicate-bindings params)

    (check-exprs-lang-value (set-union env params) value))

  ;; Throws an error if names contains duplicate names.
  (define/contract (check-duplicate-bindings names)
    (-> (listof name?) void?)
    (when (and (check-duplicates names) #t)
      (error "duplicate parallel bindings found")))

  ;; Throws an error if p is an invalid exprs-lang-v8-p.
  (define (check-exprs-lang-p p)
    (match p
      [`(module (define ,xs (lambda (,params ...) ,values)) ... ,value)
        (unless (andmap name? xs)
          (error "invalid xs in defines: " xs))
        (check-duplicate-bindings xs)
        (for ([x xs]
              [p params])
          (hash-set! proc-arities x (length p)))

        (for ([params params]
              [v values])
          (check-exprs-lang-proc xs params v))

        (check-exprs-lang-value xs value)
        p]
      [_ (error "invalid module ")]))

  ;; Throws an error if p is an invalid exprs-lang-v8-value.
  (define/contract (check-exprs-lang-value env v)
    (-> generic-set? any/c void?)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
        (unless (andmap name? xs)
          (error "invalid xs in let: " xs))
        (check-duplicate-bindings xs)
        (for ([v vs]) (check-exprs-lang-value env v))
        (check-exprs-lang-value
          (set-union xs env)
          v)]
      [`(if ,v1 ,v2 ,v3)
        (check-exprs-lang-value env v1)
        (check-exprs-lang-value env v2)
        (check-exprs-lang-value env v3)]
      [`(call ,v1 ,vs ...)
        (check-exprs-lang-value env v1)
        (unless (= (length vs) (dict-ref proc-arities v1))
          (error "arity mismatch: " v1))

        (for ([v vs])
          (check-exprs-lang-value env v))]
      [_ (check-exprs-lang-triv env v)]))

  ;; Throws an error if p is an invalid exprs-lang-v8-triv.
  (define/contract (check-exprs-lang-triv env t)
    (-> generic-set? any/c void?)
    (match t
      [(? int61?)
       (void)]
      [#t
       (void)]
      [#f
       (void)]
      ['empty
       (void)]
      ['(void)
       (void)]
      [`(error ,uint8)
       (unless (uint8? uint8)
         (error "error takes a uint8: " uint8))]
      [(? ascii-char-literal?)
       (void)]
      [_ (check-exprs-lang-x env t)]))

  ;; Throws an error if p is an invalid exprs-lang-v8-x
  (define/contract (check-exprs-lang-x env x)
    (-> generic-set? any/c void?)
    (match x
      [(? prim-f?) (void)]
      [(? name?)
       (unless (set-member? env x)
         (error "undefined: " x))]
      [_ (error "invalid x: " x)]))

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
       (void)]))

  (check-exprs-lang-p p))

(module+ test
  (require rackunit)

  (define-check (check-valid p)
    (check-equal? (check-exprs-lang p) p)
    (check-true (exprs-lang-v8? p)))

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
  (check-valid
    '(module
       (let ([x 4])
         (if #t
           (call + 40 2)
           2))))

  ;; prim-fs
  (define-syntax-rule (prim-f-tests arity prim-fs ...)
    (begin
      (test-case
        (format "prim-f ~a" 'prim-fs)
        (check-valid `(module (call prim-fs ,@(range arity))))
        (check-invalid `(module (call prim-fs ,@(range (add1 arity)))))
        (check-invalid `(module (call prim-fs))))
      ...))
  (prim-f-tests 2 * + - < eq? <= > >= cons vector-ref)
  (prim-f-tests 1
    fixnum?
 	 	boolean?
 	 	empty?
 	 	void?
 	 	ascii-char?
 	 	error?
    pair?
    vector?
    car
    cdr
    make-vector
    vector-length
 	 	not)
  (prim-f-tests 3 vector-set!)

  ;; not a uint8
  (check-invalid '(module (error 100000)))

  (check-valid
    '(module
       (define none (lambda () 42))
       (define one (lambda (x) x))
       (define two (lambda (x y) x))
       (let ([x (call one 1)]
             [y (call two 1 2)]
             [z (call none)])
         z)))
  (check-invalid
    '(module
       (define none (lambda () 42))
       (define one (lambda (x) x))
       (define two (lambda (x y) x))
       (let ([x (call one 1)]
             ;; wrong arity
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

  (check-invalid
    '(module
       (call undefined 1)))

  (check-invalid 42)

  (check-invalid
    '(module
       (let ([x 1])
         y)))

  (check-invalid
    '(module
       (let ([z 1]
             [x z])
         x)))

  (check-valid
    '(module
       (let ([x 42])
         (let ([y x])
           y))))

  (check-invalid
    '(module
       (define x (lambda (1 2 3) 42))
       42))
  )
