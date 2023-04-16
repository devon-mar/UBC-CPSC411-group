#lang racket

(require cpsc411/langs/v9)

(provide optimize-direct-calls)

;; Inline all direct calls to first-class procedures.
(define/contract (optimize-direct-calls p)
  (-> just-exprs-lang-v9? just-exprs-lang-v9?)

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

  ;; just-exprs-lang-v9-p -> just-exprs-lang-v9-p
  (define (optimize-direct-calls-p p)
    (match p
      [`(module ,v)
        `(module ,(optimize-direct-calls-value v))]))

  ;; just-exprs-lang-v9-value -> just-exprs-lang-v9-value
  (define (optimize-direct-calls-value v)
    (match v
      [`(unsafe-procedure-call ,v ,vs ...)
        (match v
          [`(lambda (,params ...) ,v)
            `(let ,(map (lambda (p v) `[,p ,(optimize-direct-calls-value v)]) params vs)
               ,(optimize-direct-calls-value v))]
          [_
            `(unsafe-procedure-call
               ,(optimize-direct-calls-value v)
               ,@(map optimize-direct-calls-value vs))])]
      [`(letrec ([,alocs (lambda (,params ...) ,vs)] ...) ,v)
        `(letrec ,(map (lambda (a ps v) `[,a (lambda ,ps ,v)]) alocs params vs)
           ,(optimize-direct-calls-value v))]
      [`(let ([,as ,vs] ...) ,v)
        `(let ,(map (lambda (a v) `[,a ,(optimize-direct-calls-value v)]) as vs)
           ,(optimize-direct-calls-value v))]
      [`(if ,v1 ,v2 ,v3)
        `(if
           ,(optimize-direct-calls-value v1)
           ,(optimize-direct-calls-value v2)
           ,(optimize-direct-calls-value v3))]
      [`(begin ,es ... ,v)
        `(begin
           ,@(map optimize-direct-calls-effect es)
           ,(optimize-direct-calls-value v))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        `(,primop ,@(map optimize-direct-calls-value vs))]
      ;; triv
      [_ v]))

  ;; just-exprs-lang-v9-effect -> just-exprs-lang-v9-effect
  (define (optimize-direct-calls-effect e)
    (match e
      ;; modified template - removed tail e since we assume valid input
      [`(begin ,es ...)
        `(begin ,@(map optimize-direct-calls-effect es))]
      [`(,primop ,vs ...)
        `(,primop ,@(map optimize-direct-calls-value vs))]))

  ;; not used
  #;
  (define (optimize-direct-calls-triv t)
    (match t
      [#t
       (void)]
      [#f
       (void)]
      ['empty
       (void)]
      [`(error ,uint8)
        (void)]
      [(? ascii-char-literal?)
       (void)]
      [`(lambda (,as ...) ,v)
        (void)]
      [(? fixnum?)
       (void)]
      [(? aloc?)
       (void)]))

  ;; not used
  #;
  (define (optimize-direct-calls-primop p)
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

  (optimize-direct-calls-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-just-exprs-lang-v9 (optimize-direct-calls p))
      42))

  (define-syntax (t stx)
    (syntax-case stx ()
      [(_ in out) #`(check-equal? (optimize-direct-calls 'in) 'out)]
      [(_ in) #`(t in in)]))

  (t
    (module (unsafe-procedure-call (lambda (x.1 y.1) 42) 1 2))
    (module (let ([x.1 1] [y.1 2])
              42)))

  (t
    ;; value/(unsafe-procedure-call value value ...)
    (module
      (unsafe-procedure-call
        (lambda (x.1 y.1) 42)
        (unsafe-procedure-call (lambda (x.1) x.1) 10)
        2))
    (module (let ([x.1 (let ([x.1 10]) x.1)]
                  [y.1 2])
              42)))

  (t (module (unsafe-procedure-call foo.1 1 2 3)))

  (check-42
    '(module
       ;; value/(primop value ...)
       (unsafe-fx+
         (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1)) 18 4)
         (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx* x.1 y.1)) 10 2))))

  ;; value/(letrec ([aloc (lambda (aloc ...) value)] ...) value)
  (check-42
    '(module
       (letrec ([foo.1 (lambda (x.1) (unsafe-procedure-call bar.1 x.1 4))]
                [bar.1 (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1))])
         (unsafe-procedure-call (lambda (x.1) (unsafe-procedure-call foo.1 x.1)) 38))))

  ;; value/(let ([aloc value] ...) value)
  (check-42
    '(module
       (let ([bar.1 (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1)) 14 2)]
             [foo.1 (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx* x.1 y.1)) 13 2)])
         (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1)) foo.1 bar.1))))

  (check-42
    '(module
       ;; value/(if value value value)
       (if (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx< x.1 y.1)) 1 2)
         (unsafe-procedure-call (lambda () 42))
         (unsafe-procedure-call (lambda (x.1) (unsafe-fx+ x.1 2)) 2))))

  (check-42
    '(module
       (let ([v.1 (unsafe-make-vector 6)])
         ;; value/(begin effect ... value)
         (begin
           ;; effect/(begin effect ... effect)
           (begin
             ;; effect/(primop value ...)
             (unsafe-vector-set! v.1 (unsafe-procedure-call (lambda () 0)) (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx- y.1 x.1)) 2 50))
             (unsafe-vector-set! (unsafe-procedure-call (lambda () v.1)) 1 (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx* y.1 x.1)) 10000 0)))
           (unsafe-fx-
             (unsafe-fx+ (unsafe-vector-ref v.1 0)
                         (unsafe-vector-ref v.1 1))
             (unsafe-vector-length v.1))))))

  )
