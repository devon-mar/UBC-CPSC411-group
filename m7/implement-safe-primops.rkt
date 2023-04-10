#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide implement-safe-primops)

;; Milestone 7 Exercise 4
;; Milestone 8 Exercise 3
;;
;; Implement safe primitive operations by inserting procedure definitions for
;; each primitive operation which perform dynamic tag checking, to ensure type safety.
;;
;; Any type errors will return (error 2).
(define/contract (implement-safe-primops p)
  (-> exprs-unique-lang-v8? exprs-unsafe-data-lang-v8?)

  ;; keep track of the primops that are actually called in p
  (define used-prim-fs (mutable-set))
  (define/contract (mark-prim-f-used! p)
    (-> symbol? void?)
    (set-add! used-prim-fs p))

  ;; Return a definition of a safe primop that calls.
  ;; Don't call this directly - used the macro below!
  ;; 
  ;; f if all arguments correspond to the types in types.
  ;; If the arguments are of the wrong type, (error 2) is returned.
  ;; If an argument is of the right type but fails some predicate check, (error 3) is returned.
  (define (make-safe-primop label f checks)
    (define-values (types preds)
      (splitf-at checks symbol?))


    (define vs (map (lambda (_) (fresh 'tmp)) types))

    ;; some of this stuff is stolen from cpsc411's fvar stuff
    (define/contract (placeholder? x)
      (-> any/c boolean?)
      (and (symbol? x)
           (regexp-match-exact? #rx"^arg[0-9]+" (symbol->string x))))

    ;; return the aloc corresponding to the placeholder from vs.
    (define/contract (placeholder->aloc x)
      (-> placeholder? aloc?)
      (list-ref vs (string->number (substring (symbol->string x) 3))))

    ;; replace all argN placeholders
    (define/contract (replace-placeholders lst)
      (-> (listof any/c) (listof any/c))
      (map (lambda (x)
             (match x
               [(? list?) (replace-placeholders x)]
               [(? placeholder?) (placeholder->aloc x)]
               [_ x]))
           lst))

    (define pred-checks
      (for/fold ([body (cons f vs)])
                ([p preds])
        `(if
           ,(replace-placeholders p)
           ,body (error 3))))

    `(define ,label
       (lambda ,vs
         ,(for/fold ([body pred-checks])
                    ([t types]
                     [v vs])
            (if (equal? 'any/c t)
              body
              `(if (,t ,v)
                 ,body
                 (error 2)))))))

  ;; Checks must start with a list of symbol? corresponding to primops
  ;; that check argument types. After all argument types have been defined,
  ;; arbitrary preds can be given to specify any requirements. In these preds,
  ;; arg0... argN may be used to refer to arguments.
  (define-syntax-rule (make-safe-primop-procs [safes unsafes checks ...] ...)
    (for/fold ([acc '()])
              ([s (list 'safes ...)]
               [u (list 'unsafes ...)]
               [types (list (list 'checks ...) ...)])
      (define l (fresh-label s))
      (dict-set acc s (cons l (make-safe-primop l u types)))))

  ;; Dictionary of primop-f to (cons proc-label proc-define)
  (define safe-procs
    (make-safe-primop-procs
      [* unsafe-fx* fixnum? fixnum?]
      [+ unsafe-fx+ fixnum? fixnum?]
      [- unsafe-fx- fixnum? fixnum?]
      [< unsafe-fx< fixnum? fixnum?]
      [eq? eq? any/c any/c]
      [<= unsafe-fx<= fixnum? fixnum?]
      [> unsafe-fx> fixnum? fixnum?]
      [>= unsafe-fx>= fixnum? fixnum?]
      ;; unops
      [fixnum? fixnum? any/c]
      [boolean? boolean? any/c]
      [empty? empty? any/c]
      [void? void? any/c]
      [ascii-char? ascii-char? any/c]
      [error? error? any/c]
      [not not any/c]
      [pair? pair? any/c]
      [vector? vector? any/c]
      [cons cons any/c any/c]
      [car unsafe-car pair?]
      [cdr unsafe-cdr pair?]
      [make-vector unsafe-make-vector fixnum? (unsafe-fx>= arg0 0)]
      [vector-length unsafe-vector-length vector?]
      [vector-set!
        unsafe-vector-set!
        vector? fixnum? any/c
        (unsafe-fx< arg1 (unsafe-vector-length arg0)) (unsafe-fx>= arg1 0)]
      [vector-ref
        unsafe-vector-ref
        vector? fixnum?
        (unsafe-fx< arg1 (unsafe-vector-length arg0)) (unsafe-fx>= arg1 0)]))

  ;; not used
  #;
  (define/contract (binop? b)
    (-> any/c boolean?)
    (and
      (memq b '(* + - < eq? <= > >=))
      #t))

  ;; value: exprs-unique-lang-v8-value
  ;; -> exprs-unsafe-data-lang-v8-proc
  (define/contract (implement-safe-primops-proc label params value)
    (-> label? (listof aloc?) any/c any/c)
    `(define ,label
       (lambda ,params ,(implement-safe-primops-value value))))

  ;; exprs-unique-lang-v8-p -> exprs-unsafe-data-lang-v8-p
  (define (implement-safe-primops-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,values)) ... ,value)
        (define v (implement-safe-primops-value value))
        (define procs (map implement-safe-primops-proc labels alocs values))
        (define safe-primops
          (for/fold ([acc '()])
                    ([u used-prim-fs])
            (cons (cdr (dict-ref safe-procs u)) acc)))

        `(module
           ,@safe-primops
           ,@procs
           ,v)]))

  ;; exprs-unique-lang-v8-value -> exprs-unsafe-data-lang-v8-value
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

  ;; exprs-unique-lang-v8-triv -> exprs-unsafe-data-lang-v8-triv
  (define (implement-safe-primops-triv t)
    (match t
      [(? label?) t]
      [(? aloc?) t]
      [(? int61?) t]
      [#t t]
      [#f t]
      ['empty t]
      ['(void) t]
      [`(error ,_) t]
      [(? ascii-char-literal?) t]
      [_ (implement-safe-primops-prim-f t)]))

  ;; Returns the label of the safe primop corresponding to prim-f
  ;; and marks the prim-f p as used.
  ;;
  ;; exprs-unique-lang-v8-prim-f -> exprs-unsafe-data-lang-v8-label
  (define (implement-safe-primops-prim-f p)
    (mark-prim-f-used! p)
    (car (dict-ref safe-procs p)))

  (implement-safe-primops-p p))

(module+ test
  (require rackunit)
  
  (define-check (check-42 p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v8 (implement-safe-primops p))
      42))

  (define-check (check-eval-true p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v8 (implement-safe-primops p))
      #t))
  (define-check (check-eval-false p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v8 (implement-safe-primops p))
      #f))

  (check-42 '(module (call + 40 2)))
  (check-42 '(module (call * 21 2)))
  (check-42 '(module (call - 50 8)))

  (define-check (check-error-2 p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v8 (implement-safe-primops p))
      '(error 2)))
  (define-check (check-error-3 p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v8 (implement-safe-primops p))
      '(error 3)))

  (define-syntax-rule (binop-invalid-arg-type-tests binops ...)
    (test-case
      (format "binop-invalid-arg-type ~a" 'binop)
      (begin
        (check-error-2 '(module (call binops #t 2)))
        (check-error-2 '(module (call binops 2 #t)))
        (check-error-2 '(module (call binops #t #t)))

        (check-error-2 '(module (call binops #f 2)))
        (check-error-2 '(module (call binops 2 #f)))
        (check-error-2 '(module (call binops #f #f)))

        (check-error-2 '(module (call binops #\a 2)))
        (check-error-2 '(module (call binops 2 #\a)))
        (check-error-2 '(module (call binops #\a #\b)))

        (check-error-2 '(module (call binops (void) 2)))
        (check-error-2 '(module (call binops 2 (void))))
        (check-error-2 '(module (call binops (void) (void))))

        (check-error-2 '(module (call binops empty 2)))
        (check-error-2 '(module (call binops 2 empty)))
        (check-error-2 '(module (call binops empty empty)))

        (check-error-2 '(module (call binops (call make-vector 2) 2)))
        (check-error-2 '(module (call binops 2 (call make-vector 2))))
        (check-error-2 '(module (call binops (call make-vector 2) (call make-vector 2))))

        (check-error-2 '(module (call binops (call cons 1 2) 2)))
        (check-error-2 '(module (call binops 2 (call cons 1 2))))
        (check-error-2 '(module (call binops (call cons 1 2) (call cons 1 2))))) ...))

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
       (define L.ft.1 (lambda () 42))
       (call L.ft.1)))

  (check-42
    '(module
       (define L.ft.1 (lambda (f.1) (call f.1 40 2)))
       (call L.ft.1 +)))

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
  (check-error-3 '(module (call make-vector -1)))
  ;; out of bounds read
  (check-error-3 '(module (call vector-ref (call make-vector 1) 1)))
  (check-error-3 '(module (call vector-ref (call make-vector 1) 2)))
  ;; negative read index
  (check-error-3 '(module (call vector-ref (call make-vector 1) -1)))
  ;; out of bounds write
  (check-error-3 '(module (call vector-set! (call make-vector 1) 1 (void))))
  (check-error-3 '(module (call vector-set! (call make-vector 1) 2 (void))))
  ;; negative write index
  (check-error-3 '(module (call vector-set! (call make-vector 1) -1 (void))))

  (check-error-2 '(module (call vector-length (void))))
  (check-error-2 '(module (call vector-ref (void) 0)))
  (check-error-2 '(module (call vector-set! (void) 0 0)))

  (check-error-2 '(module (call car (void))))
  (check-error-2 '(module (call cdr (void))))

  (check-42 '(module (call car (call cons 42 #\a))))
  (check-42 '(module (call cdr (call cons (void) 42))))

  (check-eval-true '(module (call pair? (call cons 5 0))))
  (check-eval-false '(module (call pair? #\a)))

  (check-eval-true '(module (call vector? (call make-vector 2))))
  (check-eval-false '(module (call vector? (void))))
  )
