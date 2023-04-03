#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(provide implement-safe-primops)

;; Milestone 7 Exercise 4
;;
;; Implement safe primitive operations by inserting procedure definitions for
;; each primitive operation which perform dynamic tag checking, to ensure type safety.
(define/contract (implement-safe-primops p)
  (-> exprs-unique-lang-v7? exprs-unsafe-data-lang-v7?)

  ;; keep track of the primops that are actually called in p
  (define used-prim-fs (mutable-set))
  (define/contract (mark-prim-f-used! p)
    (-> symbol? void?)
    (set-add! used-prim-fs p))

  ;; Return a definition of a safe primop that calls
  ;; f if all arguments correspond to the types in types.
  (define (make-safe-primop label f types)
    (define vs (map (lambda (_) (fresh 'tmp)) types))
    `(define ,label
       (lambda ,vs
         ,(for/fold ([body (cons f vs)])
                    ([t types]
                     [v vs])
            (if (equal? 'any/c t)
              body
              `(if (,t ,v)
                 ,body
                 (error 2)))))))

  (define-syntax-rule (make-safe-primop-procs [safes unsafes param-types ...] ...)
    (for/fold ([acc '()])
              ([s (list 'safes ...)]
               [u (list 'unsafes ...)]
               [types (list (list 'param-types ...) ...)])
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
      [not not any/c]))

  ;; not used
  #;
  (define/contract (binop? b)
    (-> any/c boolean?)
    (and
      (memq b '(* + - < eq? <= > >=))
      #t))

  ;; value: exprs-unique-lang-v7-value
  ;; -> exprs-unsafe-data-lang-v7-proc
  (define/contract (implement-safe-primops-proc label params value)
    (-> label? (listof aloc?) any/c any/c)
    `(define ,label
       (lambda ,params ,(implement-safe-primops-value value))))

  ;; exprs-unique-lang-v7-p -> exprs-unsafe-data-lang-v7-p
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

  ;; exprs-unique-lang-v7-value -> exprs-unsafe-data-lang-v7-value
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

  ;; exprs-unique-lang-v7-triv -> exprs-unsafe-data-lang-v7-triv
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
  ;; exprs-unique-lang-v7-prim-f -> exprs-unsafe-data-lang-v7-label
  (define (implement-safe-primops-prim-f p)
    (mark-prim-f-used! p)
    (car (dict-ref safe-procs p)))

  ;; not used
  #;
  (define (implement-safe-primops-binop b)
    (match b
      ['*
       (void)]
      ['+
       (void)]
      ['-
       (void)]
      ['<
       (void)]
      ['eq?
       (void)]
      ['<=
       (void)]
      ['>
       (void)]
      ['>=
       (void)]))

  ;; not used
  #;
  (define (implement-safe-primops-unop u)
    (match u
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
       (void)]))

  (implement-safe-primops-p p))

(module+ test
  (require rackunit)
  
  (define-check (check-42 p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v7 (implement-safe-primops p))
      42))

  (define-check (check-eval-true p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v7 (implement-safe-primops p))
      #t))
  (define-check (check-eval-false p)
    (check-equal?
      (interp-exprs-unsafe-data-lang-v7 (implement-safe-primops p))
      #f))

  (check-42 '(module (call + 40 2)))
  (check-42 '(module (call * 21 2)))
  (check-42 '(module (call - 50 8)))

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
  )
