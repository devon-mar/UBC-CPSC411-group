#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide dox-lambdas)

;; Milestone 9 Exercise 7
;;
;; Explicitly binds all procedures to abstract locations.
(define/contract (dox-lambdas p)
  (-> just-exprs-lang-v9? lam-opticon-lang-v9?)

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


  ;; just-exprs-lang-v9-p -> lam-opticon-lang-v9-p
  (define (dox-lambdas-p p)
    (match p
      [`(module ,v)
        `(module
           ,(dox-lambdas-value v))]))

  ;; just-exprs-lang-v9-value -> lam-opticon-lang-v9-value
  (define (dox-lambdas-value v)
    (match v
      ;; modified template - combined both args since they're
      ;; both values
      [`(unsafe-procedure-call ,vs ...)
        `(unsafe-procedure-call ,@(map dox-lambdas-value vs))]
      [`(letrec ([,alocs (lambda (,params ...) ,vs)] ...) ,v)
        `(letrec
           ,(map (lambda (a params v) `[,a (lambda ,params ,(dox-lambdas-value v))]) alocs params vs)
           ,(dox-lambdas-value v))]
      [`(let ([,as ,vs] ...) ,v)
        `(let
           ,(map (lambda (a v) `[,a ,(dox-lambdas-value v)]) as vs)
           ,(dox-lambdas-value v))]
      [`(if ,v1 ,v2 ,v3)
        `(if
           ,(dox-lambdas-value v1)
           ,(dox-lambdas-value v2)
           ,(dox-lambdas-value v3))]
      [`(begin ,es ... ,v)
        `(begin
           ,@(map dox-lambdas-effect es)
           ,(dox-lambdas-value v))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        `(,primop ,@(map dox-lambdas-value vs))]
      ;; triv
      [_ (dox-lambdas-triv v)]))

  ;; just-exprs-lang-v9-effect -> lam-opticon-lang-v9-effect
  (define (dox-lambdas-effect e)
    (match e
      ;; modified template - removed tail e since we assume
      ;; valid input.
      [`(begin ,es ...)
        `(begin ,@(map dox-lambdas-effect es))]
      [`(,primop ,vs ...)
        `(,primop ,@(map dox-lambdas-value vs))]))

  ;; just-exprs-lang-v9-triv -> lam-opticon-lang-v9-value
  (define (dox-lambdas-triv t)
    (match t
      [#t t]
      [#f t]
      ['empty t]
      ;; (error uint8)
      [`(error ,_) t]
      [(? ascii-char-literal?) t]
      ['(void) t]
      [`(lambda (,as ...) ,v)
        (define tmp (fresh 'lambda))
        `(letrec ([,tmp (lambda ,as ,(dox-lambdas-value v))])
           ,tmp)]
      [(? fixnum?) t]
      [(? aloc?) t]))

  ;; not used
  #;
  (define (dox-lambdas-primop p)
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

  (dox-lambdas-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-lam-opticon-lang-v9 (dox-lambdas p))
      42))

  (check-match
    (dox-lambdas '(module (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1))))
    `(module
       (letrec ([,tmp (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1))])
         ,tmp))
    (aloc? tmp))

  (check-42
    '(module (unsafe-procedure-call (lambda () 42))))
  (check-42
    '(module (unsafe-procedure-call (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1)) 22 20)))
  (check-42
    '(module
       ;; (unsafe-procedure-call value value ...)
       (unsafe-procedure-call
         (lambda (p.1 x.1 y.1) (unsafe-procedure-call p.1 x.1 y.1))
         (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1))
         22
         20)))

  (check-42
    '(module
       ;; (primop value ...)
       (unsafe-fx+
         (unsafe-procedure-call (lambda () 14))
         28)))

  (check-42
    '(module
       ;; (letrec ([aloc (lambda (aloc ...) value)] ...) value)
       (letrec ([a.1 (lambda (x.1 y.1) (unsafe-fx- x.1 y.1))]
                [b.1 (lambda (x.1 y.1) (unsafe-fx* x.1 y.1))])
         (unsafe-procedure-call
          (lambda (c.1 d.1) (unsafe-fx+ c.1 d.1))
          (unsafe-procedure-call a.1 16 4)
          (unsafe-procedure-call b.1 10 3)))))
  ;; (let ([aloc value] ...) value)
  (check-42
    '(module
       (let ([p.1 (lambda () (lambda (a.1 b.1) (eq? a.1 b.1)))])
         (unsafe-procedure-call
           ;; (if value value value)
           (if (unsafe-procedure-call (unsafe-procedure-call p.1) 1 1)
             (lambda () 42)
             (lambda () 0))))))

  (check-42
    '(module
       (let ([v.1 (unsafe-make-vector 8)])
         ;; (begin effect ... value)
         (begin
           ;; (begin effect ... effect)
           (begin
             (unsafe-vector-set! v.1 0 (lambda (x.1) x.1))
             ;; effect/(primop value ...)
             (unsafe-vector-set! v.1 1 (lambda (x.1) (unsafe-vector-length x.1))))
           (unsafe-procedure-call (unsafe-vector-ref v.1 0) 42)))))

  (check-42
    '(module
       (if #t
         (if #f #\a (if (eq? 1 1) 42 empty))
         (error 1))))

  (check-42
    '(module
       (if #t
         42
         (unsafe-procedure-call (lambda () (void))))))
  )
