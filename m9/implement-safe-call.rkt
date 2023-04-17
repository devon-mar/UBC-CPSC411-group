#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-safe-call)

;; Milestone 9 Exercise 4
;;
;; Implement call as an unsafe procedure call with dynamic checks.
;; If call is not given a procdure, (error 126) is returned.
;; If a procedure is called with the wrong arity, (error 125) is returned.
(define/contract (implement-safe-call p)
  (-> exprs-unsafe-data-lang-v9? exprs-unsafe-lang-v9?)

  (define bad-proc-error `(error 126))
  (define bad-arity-error `(error 125))

  ;; stores the arities of all top level procedures
  (define arities (make-hash))

  (define/contract (primop? p)
    (-> any/c boolean?)
    (and (memq p
               '(unsafe-fx*
                 unsafe-fx+
                 unsafe-fx-
                 eq?
                 unsafe-fx<
                 unsafe-fx<=
                 unsafe-fx>
                 unsafe-fx>=
                 fixnum?
                 boolean?
                 empty?
                 void?
                 ascii-char?
                 error?
                 not
                 pair?
                 vector?
                 procedure?
                 cons
                 unsafe-car
                 unsafe-cdr
                 unsafe-make-vector
                 unsafe-vector-length
                 unsafe-vector-set!
                 unsafe-vector-ref
                 unsafe-procedure-arity))
         #t))

  ;; Implements safe calls for the given procedure.
  ;;
  ;; v: exprs-unsafe-data-lang-v9-value
  ;; -> exprs-unsafe-lang-v9-proc
  (define/contract (implement-safe-call-proc aloc params v)
    (-> aloc? (listof aloc?) any/c any/c)
    `(define
       ,aloc
       (lambda ,params ,(implement-safe-call-value v))))

  ;; exprs-unsafe-data-lang-v9-p -> exprs-unsafe-lang-v9-p
  (define (implement-safe-call-p p)
    (match p
      [`(module (define ,alocs (lambda (,params ...) ,vs)) ... ,v)
        (for ([a alocs] [p params]) (hash-set! arities a (length p)))
        `(module
           ,@(map implement-safe-call-proc alocs params vs)
           ,(implement-safe-call-value v))]))

  ;; exprs-unsafe-data-lang-v9-value -> exprs-unsafe-lang-v9-value
  (define (implement-safe-call-value v)
    (match v
      [`(call ,p ,vs ...)
        (if (aloc? p)
          `(if (procedure? ,p)
             (if (eq? (unsafe-procedure-arity ,p) ,(length vs))
               (unsafe-procedure-call ,p ,@(map implement-safe-call-value vs))
               ,bad-arity-error)
             ,bad-proc-error)
          (let ([tmp (fresh 'isc-tmp)])
            `(let ([,tmp ,p])
               (if (procedure? ,p)
                 (if (eq? (unsafe-procedure-arity ,tmp) ,(length vs))
                   (unsafe-procedure-call ,tmp ,@(map implement-safe-call-value vs))
                   ,bad-arity-error)
                 ,bad-proc-error))))]
      [`(let ([,as ,vs] ...) ,v)
        `(let
           ,(map (lambda (a v) `[,a ,(implement-safe-call-value v)]) as vs)
           ,(implement-safe-call-value v))]
      [`(if ,v1 ,v2 ,v3)
        `(if
           ,(implement-safe-call-value v1)
           ,(implement-safe-call-value v2)
           ,(implement-safe-call-value v3))]
      [`(begin ,es ... ,v)
        `(begin
           ,@(map implement-safe-call-effect es)
           ,(implement-safe-call-value v))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        `(,primop ,@(map implement-safe-call-value vs))]
      ;; triv
      [_ (implement-safe-call-triv v)]))

  ;; exprs-unsafe-data-lang-v9-effect -> exprs-unsafe-lang-v9-effect
  (define (implement-safe-call-effect e)
    (match e
      ;; modified template - removed tail effect
      [`(begin ,es ...)
        `(begin ,@(map implement-safe-call-effect es))]
      [`(,primop ,vs ...)
        `(,primop ,@(map implement-safe-call-value vs))]))

  ;; exprs-unsafe-data-lang-v9-triv -> exprs-unsafe-lang-v9-triv
  (define (implement-safe-call-triv t)
    (match t
      [#t t]
      [#f t]
      ['empty t]
      ['(void) t]
      ;; (error uint8)
      [`(error ,_) t]
      [(? ascii-char-literal?) t]
      [`(lambda (,as ...) ,v)
        `(lambda ,as ,(implement-safe-call-value v))]
      [(? aloc?) t]
      [(? fixnum?) t]))

  ;; not used
  #;
  (define (implement-safe-call-primop p)
    (match p
      ['unsafe-fx*
       (void)]
      ['unsafe-fx+
       (void)]
      ['unsafe-fx-
       (void)]
      ['eq?
       (void)]
      ['unsafe-fx<
       (void)]
      ['unsafe-fx<=
       (void)]
      ['unsafe-fx>
       (void)]
      ['unsafe-fx>=
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
      ['procedure?
       (void)]
      ['cons
       (void)]
      ['unsafe-car
       (void)]
      ['unsafe-cdr
       (void)]
      ['unsafe-make-vector
       (void)]
      ['unsafe-vector-length
       (void)]
      ['unsafe-vector-set!
       (void)]
      ['unsafe-vector-ref
       (void)]
      ['unsafe-procedure-arity
       (void)]))

  (implement-safe-call-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-exprs-unsafe-lang-v9 (implement-safe-call p))
      42))
  (define-check (check-arity-error p)
    (check-equal?
      (interp-exprs-unsafe-lang-v9 (implement-safe-call p))
      '(error 125)))
  (define-check (check-not-proc-error p)
    (check-equal?
      (interp-exprs-unsafe-lang-v9 (implement-safe-call p))
      '(error 126)))
  (define-check (check-err p)
    (check-exn exn:fail? (lambda() (interp-exprs-unsafe-lang-v9 (implement-safe-call p)))))

  ;; value/(call value value ...)
  (check-42 '(module 42))
  (check-42
    '(module
       (define foo.1 (lambda () 42))
       (call foo.1)))
  (check-42
    '(module
       (define foo.1 (lambda () 42))
       (let ([bar.1 foo.1])
         (call bar.1))))

  (check-arity-error
    '(module
       (define foo.1 (lambda () 42))
       ;; triv/(void)
       (call foo.1 1 2 (void))))
  (check-arity-error
    '(module
       ;; value/triv
       (define foo.1 (lambda () 42))
       (let ([bar.1 foo.1])
         (call bar.1 0 1 2))))

  ;; value/(primop value ...)
  (check-42
    '(module
      (define foo.1 (lambda (x.1) x.1))
      (define bar.1 (lambda (x.1) x.1))
       (unsafe-fx+
         (call foo.1 12)
         (call bar.1 30))))
  (check-err
    '(module
      (define foo.1 (lambda (x.1) x.1))
      (define bar.1 (lambda (x.1) x.1))
       (unsafe-fx+
         (call foo.1)
         ;; triv/empty
         (call bar.1 empty))))

  ;; value/(let ([aloc value] ...) value)
  (check-err
    '(module
      (define bar.1 (lambda (x.1) x.1))
       (let ([foo.1 (call bar.1 1 2 3 4)])
         (unsafe-fx+
           (call foo.1)
           (call bar.1 30)))))

  ;; value/(if value value value)
  (check-not-proc-error
    '(module
       (let ([foo.1 10])
         ;; triv/#t
         (if #t
           (call foo.1 10)
           ;; triv/#f
           #f))))

  ;; value/(begin effect ... value)
  (check-42
    '(module
       (define one.1 (lambda () 1))
       (define ft.1 (lambda () 42))
       (let ([v.1 (unsafe-make-vector 2)])
         (begin
           ;; effect/(begin effect ... effect)
           ;; effect/(primop value ...)
           (begin
             (unsafe-vector-set! v.1 0 (call one.1))
             (unsafe-vector-set! v.1 1 (call ft.1)))
           (unsafe-fx*
             (unsafe-vector-ref v.1 0)
             (unsafe-vector-ref v.1 1))))))

  (check-42
    '(module
       ;; triv/(lambda (aloc ...) value)
       ;; triv/(error uint8)
       (call (lambda (x.1) (if (unsafe-fx> x.1 30) x.1 (error 123))) 42)))
  (check-arity-error
    '(module
       ;; triv/(lambda (aloc ...) value)
       ;; triv/(error uint8)
       (call (lambda (x.1) (if (unsafe-fx> x.1 30) x.1 #\a)) 42 1)))
  (check-not-proc-error
    '(module
       (call #\a)))

  (check-42
    '(module
       ;; triv/ascii-char-literal
       (if (eq? #\a (call (lambda () #\a)))
         42
         (void))))

  (check-42
    '(module
       (define foo.1 (lambda (x.1) (if (unsafe-fx> x.1 4) 2 42)))
       (define bar.1 (lambda () 2))
       (call foo.1 (call bar.1))))
  )
