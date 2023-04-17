#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-safe-primops)

;; Milestone 7 Exercise 4
;; Milestone 8 Exercise 3
;; Milestone 9 Exercise 3
;;
;; Implement safe primitive operations by inserting procedure definitions for
;; each primitive operation which perform dynamic tag checking, to ensure type safety.
;;
;; Argument type errors will return (error (modulo i+100 uint8-max)) where i is the
;; argument index in left to right order. For example, given the set of arguments '(x y),
;; a type error on x will return 100 and a type error on y will return 101.
;;
;; Any other error on arguments besides type will return 99.
;;
;; All vectors will be initialised to 0.
;;
;; Implementation details:
;; - A mutable set is used to keep track of what safe primop procs we need to insert.
;;   Whenever we see a exprs-unique-lang-v9-primop used, we add the prim-f to the set.
(define/contract (implement-safe-primops p)
  (-> exprs-unique-lang-v9? exprs-unsafe-data-lang-v9?)

  (define uint8-max 255)

  ;; aloc for the vector-init procedure.
  (define vector-init-aloc (fresh 'safe-vector-init))
  ;; The actual vector init procedure.
  (define vector-init-proc
    (let ([vec (fresh 'vec)]
          [len (fresh 'len)]
          [i (fresh 'i)])
      `(define ,vector-init-aloc
         (lambda (,vec ,len ,i)
           (if (eq? ,len ,i)
             ,vec
             (begin
               (unsafe-vector-set! ,vec ,i 0)
               (call ,vector-init-aloc ,vec ,len (unsafe-fx+ ,i 1))))))))

  ;; keep track of the primops that are actually called in p
  (define used-prim-fs (mutable-set))
  ;; Mark the prim-f p as used.
  ;; p: exprs-unique-lang-v9-primop
  (define/contract (mark-prim-f-used! p)
    (-> symbol? void?)
    (set-add! used-prim-fs p))

  ;; Don't call this directly - used the macro below!
  ;; See below for documentation.
  (define/contract (make-safe-primop aloc f checks)
    (-> aloc? (or/c symbol? procedure?) (listof (or/c symbol? procedure?)) any/c)
    (define-values (types preds)
      (splitf-at checks symbol?))

    (define vs (map (lambda (_) (fresh 'tmp)) types))

    (define pred-checks
      (for/fold ([body (if (procedure? f) (apply f vs) (cons f vs))])
                ([p preds])
        `(if
           ,(apply p vs)
           ,body (error 99))))

    `(define ,aloc
       (lambda ,vs
         ,(for/fold ([body pred-checks])
                    ([t types]
                     [v vs]
                     [i (in-naturals 100)])
            (if (equal? 'any/c t)
              body
              `(if (,t ,v)
                 ,body
                 (error ,(modulo i uint8-max))))))))

  ;; Usage:
  ;;
  ;; (make-safe-primop-procs clause ...)
  ;;
  ;; clause = [safe unsafe type ... check ...]
  ;;
  ;; unsafe = symbol?
  ;;        | procedure?
  ;;
  ;; check = procedure?
  ;;
  ;; safe: The "safe" primop. It will be automatically quoted.
  ;; unsafe: The corresponding quoted "unsafe" primop or procedure. If a procedure, it must take
  ;;         exactly (length type) args.
  ;; type: Quoted symbol for each argument. The symbol should most likely end with a ?.
  ;; check: A procedure? that takes (length type) args. It must return an exprs-unsafe-data-lang-v9-value.
  ;;        This should be used to implement any additional argument checks.
  ;;
  ;; ->: A dictionary of safe primop to unsafe define ast that implements the checks.
  (define-syntax-rule (make-safe-primop-procs [safes unsafes checks ...] ...)
    (for/fold ([acc '()])
              ([s (list 'safes ...)]
               [u (list unsafes ...)]
               [types (list (list checks ...) ...)])
      (define aloc (fresh (string->symbol (format "safe-~a" s))))
      (dict-set acc s (cons aloc (make-safe-primop aloc u types)))))

  ;; Contains the safe primop definitions. See above.
  (define safe-procs
    (make-safe-primop-procs
      [* 'unsafe-fx* 'fixnum? 'fixnum?]
      [+ 'unsafe-fx+ 'fixnum? 'fixnum?]
      [- 'unsafe-fx- 'fixnum? 'fixnum?]
      [< 'unsafe-fx< 'fixnum? 'fixnum?]
      [eq? 'eq? 'any/c 'any/c]
      [<= 'unsafe-fx<= 'fixnum? 'fixnum?]
      [> 'unsafe-fx> 'fixnum? 'fixnum?]
      [>= 'unsafe-fx>= 'fixnum? 'fixnum?]
      ;; unops
      [fixnum? 'fixnum? 'any/c]
      [boolean? 'boolean? 'any/c]
      [empty? 'empty? 'any/c]
      [void? 'void? 'any/c]
      [ascii-char? 'ascii-char? 'any/c]
      [error? 'error? 'any/c]
      [not 'not 'any/c]
      [pair? 'pair? 'any/c]
      [procedure? 'procedure? 'any/c]
      [vector? 'vector? 'any/c]
      [cons 'cons 'any/c 'any/c]
      [car 'unsafe-car 'pair?]
      [cdr 'unsafe-cdr 'pair?]
      [make-vector
        (lambda (size)
          (define tmp (fresh 'tmp))
          `(let ([,tmp (unsafe-make-vector ,size)])
             (call ,vector-init-aloc ,tmp ,size 0)))
        'fixnum?
        (lambda (size) `(unsafe-fx>= ,size 0))]
      [vector-length 'unsafe-vector-length 'vector?]
      [vector-set!
        (lambda (vec idx val)
          `(begin
             (unsafe-vector-set! ,vec ,idx ,val)
             (void)))

        'vector? 'fixnum? 'any/c

        (lambda (vec idx _val) `(unsafe-fx< ,idx (unsafe-vector-length ,vec)))
        (lambda (_vec idx _val) `(unsafe-fx>= ,idx 0))]
      [vector-ref
        'unsafe-vector-ref
        'vector? 'fixnum?
        (lambda (vec idx) `(unsafe-fx< ,idx (unsafe-vector-length ,vec)))
        (lambda (_vec idx) `(unsafe-fx>= ,idx 0))]
      [procedure-arity 'unsafe-procedure-arity 'procedure?]))

  ;; not used
  #;
  (define/contract (binop? b)
    (-> any/c boolean?)
    (and
      (memq b '(* + - < eq? <= > >=))
      #t))

  ;; value: exprs-unique-lang-v9-value
  ;; -> exprs-unsafe-data-lang-v9-proc
  (define/contract (implement-safe-primops-proc aloc params value)
    (-> aloc? (listof aloc?) any/c any/c)
    `(define ,aloc
       (lambda ,params ,(implement-safe-primops-value value))))

  ;; exprs-unique-lang-v9-p -> exprs-unsafe-data-lang-v9-p
  (define (implement-safe-primops-p p)
    (match p
      [`(module (define ,alocs (lambda (,params ...) ,values)) ... ,value)
        (define v (implement-safe-primops-value value))
        (define procs (map implement-safe-primops-proc alocs params values))
        (define safe-primops
          ;; need to add vector-init since make-vector calls it
          (for/fold ([acc (if (set-member? used-prim-fs 'make-vector) `(,vector-init-proc) '())])
                    ([u used-prim-fs])
            (cons (cdr (dict-ref safe-procs u)) acc)))

        `(module
           ,@safe-primops
           ,@procs
           ,v)]))

  ;; exprs-unique-lang-v9-value -> exprs-unsafe-data-lang-v9-value
  (define (implement-safe-primops-value v)
    (match v
      [`(call ,v ,vs ...)
       `(call
          ,(implement-safe-primops-value v)
          ,@(map implement-safe-primops-value vs))]
      [`(let ([,as ,vs] ...) ,v)
       `(let
          ,(map (lambda (a v) `[,a ,(implement-safe-primops-value v)]) as vs)
          ,(implement-safe-primops-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if
          ,(implement-safe-primops-value v1)
          ,(implement-safe-primops-value v2)
          ,(implement-safe-primops-value v3))]
      [_ (implement-safe-primops-triv v)]))

  ;; exprs-unique-lang-v9-triv -> exprs-unsafe-data-lang-v9-triv
  (define (implement-safe-primops-triv t)
    (match t
      [`(lambda (,alocs ...) ,value)
        `(lambda ,alocs ,(implement-safe-primops-value value))]
      [(? aloc?) t]
      [(? int61?) t]
      [#t t]
      [#f t]
      ['empty t]
      ['(void) t]
      ;; (error uint8)
      [`(error ,_) t]
      [(? ascii-char-literal?) t]
      [_ (implement-safe-primops-prim-f t)]))

  ;; Returns the aloc of the safe primop corresponding to prim-f
  ;; and marks the prim-f p as used.
  ;;
  ;; exprs-unique-lang-v9-prim-f -> exprs-unsafe-data-lang-v9-aloc
  (define (implement-safe-primops-prim-f p)
    (mark-prim-f-used! p)
    (car (dict-ref safe-procs p)))

  (implement-safe-primops-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v9 (implement-safe-primops p))
      42))

  (define-check (check-eval-true p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v9 (implement-safe-primops p))
      #t))
  (define-check (check-eval-false p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v9 (implement-safe-primops p))
      #f))

  (check-42 '(module (call + 40 2)))
  (check-42 '(module (call * 21 2)))
  (check-42 '(module (call - 50 8)))

  (define-check (check-error-num num p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v9 (implement-safe-primops p))
      `(error ,num)))
  (define-check (check-error-99 p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v9 (implement-safe-primops p))
      '(error 99)))

  (define-syntax-rule (binop-invalid-arg-type-tests binops ...)
    (begin
      (test-case
        (format "binop-invalid-arg-type ~a" 'binops)
        (check-error-num 100 '(module (call binops #t 2)))
        (check-error-num 101 '(module (call binops 2 #t)))
        (check-error-num 101 '(module (call binops #t #t)))

        (check-error-num 100 '(module (call binops #f 2)))
        (check-error-num 101 '(module (call binops 2 #f)))
        (check-error-num 101 '(module (call binops #f #f)))

        (check-error-num 100 '(module (call binops #\a 2)))
        (check-error-num 101 '(module (call binops 2 #\a)))
        (check-error-num 101 '(module (call binops #\a #\b)))

        (check-error-num 100 '(module (call binops (void) 2)))
        (check-error-num 101 '(module (call binops 2 (void))))
        (check-error-num 101 '(module (call binops (void) (void))))

        (check-error-num 100 '(module (call binops empty 2)))
        (check-error-num 101 '(module (call binops 2 empty)))
        (check-error-num 101 '(module (call binops empty empty)))

        (check-error-num 100 '(module (call binops (call make-vector 2) 2)))
        (check-error-num 101 '(module (call binops 2 (call make-vector 2))))
        (check-error-num 101 '(module (call binops (call make-vector 2) (call make-vector 2))))

        (check-error-num 100 '(module (call binops (call cons 1 2) 2)))
        (check-error-num 101 '(module (call binops 2 (call cons 1 2))))
        (check-error-num 101 '(module (call binops (call cons 1 2) (call cons 1 2))))

        (check-error-num 100 '(module (call binops (lambda () 42) 2)))
        (check-error-num 101 '(module (call binops 2 (lambda () 42))))
        (check-error-num 101 '(module (call binops (lambda () 42) (lambda () 42))))) ...))

  (binop-invalid-arg-type-tests * + - < <= > >=)

  (check-eval-true '(module (call < 1 2)))
  (check-eval-false '(module (call < 3 2)))

  (check-eval-true '(module (call eq? 2 2)))
  (check-eval-false '(module (call eq? 3 2)))

  (check-eval-true '(module (call <= 2 2)))
  (check-eval-true '(module (call <= 1 2)))
  (check-eval-false '(module (call <= 3 2)))

  (check-eval-true '(module (call > 2 1)))
  (check-eval-false '(module (call > 2 2)))
  (check-eval-false '(module (call > 1 2)))

  (check-eval-true '(module (call >= 2 1)))
  (check-eval-true '(module (call >= 2 2)))
  (check-eval-false '(module (call >= 1 2)))

  (check-eval-true '(module (call fixnum? 2)))
  (check-eval-false '(module (call fixnum? #f)))

  (check-eval-true '(module (call boolean? #t)))
  (check-eval-true '(module (call boolean? #f)))
  (check-eval-false '(module (call boolean? 2)))

  (check-eval-true '(module (call empty? empty)))
  (check-eval-false '(module (call empty? 1)))

  (check-eval-true '(module (call void? (void))))
  (check-eval-false '(module (call void? 2)))

  (check-eval-true '(module (call ascii-char? #\a)))
  (check-eval-false '(module (call ascii-char? #f)))

  (check-eval-true '(module (call error? (error 2))))
  (check-eval-false '(module (call error? 2)))

  (check-eval-true '(module (call not (call fixnum? #f))))
  (check-eval-false '(module (call not (call fixnum? 2))))

  (check-42 '(module 42))
  (check-42
    '(module
       (define ft.1 (lambda () 42))
       (call ft.1)))

  (check-42
    '(module
       (define ft.1 (lambda (f.1) (call f.1 40 2)))
       (call ft.1 +)))

  (check-42
    '(module
       (let ([f.1 +]
             [a.1 40]
             [b.1 2])
         (call f.1 a.1 b.1))))

  (check-42
    '(module
       (call
         (if (call eq? 2 2) + *)
         40
         2)))

  ;; negative size
  (check-error-99 '(module (call make-vector -1)))
  ;; out of bounds read
  (check-error-99 '(module (call vector-ref (call make-vector 1) 1)))
  (check-error-99 '(module (call vector-ref (call make-vector 1) 2)))
  ;; negative read index
  (check-error-99 '(module (call vector-ref (call make-vector 1) -1)))
  ;; out of bounds write
  (check-error-99 '(module (call vector-set! (call make-vector 1) 1 (void))))
  (check-error-99 '(module (call vector-set! (call make-vector 1) 2 (void))))
  ;; negative write index
  (check-error-99 '(module (call vector-set! (call make-vector 1) -1 (void))))

  (check-error-num 100 '(module (call vector-length (void))))
  (check-error-num 100 '(module (call vector-ref (void) 0)))
  (check-error-num 100 '(module (call vector-set! (void) 0 0)))

  (check-error-num 100 '(module (call car (void))))
  (check-error-num 100 '(module (call cdr (void))))

  (check-42 '(module (call car (call cons 42 #\a))))
  (check-42 '(module (call cdr (call cons (void) 42))))

  (check-eval-true '(module (call pair? (call cons 5 0))))
  (check-eval-false '(module (call pair? #\a)))

  (check-eval-true '(module (call vector? (call make-vector 2))))
  (check-eval-false '(module (call vector? (void))))

  (check-eval-true '(module (call procedure? (lambda () 42))))
  (check-eval-false '(module (call procedure? (void))))

  (check-42
    `(module
       (call procedure-arity
             (lambda
               ,(for/fold ([acc '()]) ([i 42]) (cons (fresh i) acc))
               (void)))))

  ;; check vector init
  (check-42
    '(module
       (let ([v.1 (call make-vector 8)])
         (call + 42 (call vector-ref v.1 4)))))
  )
