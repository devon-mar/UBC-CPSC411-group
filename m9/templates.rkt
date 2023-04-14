#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(define/contract (just-exprs-lang-v9-template p)
  (-> just-exprs-lang-v9? any/c)

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

  (define (just-exprs-lang-v9-template-p p)
    (match p
      [`(module ,v)
        (void)]))

  (define (just-exprs-lang-v9-template-value v)
    (match v
      [`(unsafe-procedure-call ,v ,vs ...)
        (void)]
      [`(letrec ([,alocs (lambda (,params ...) ,vs)] ...) ,v)
        (void)]
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,v1 ,v2 ,v3)
        (void)]
      [`(begin ,es ... ,v)
        (void)]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (void)]
      [triv
        (void)]))

  (define (just-exprs-lang-v9-template-effect e)
    (match e
      [`(begin ,es ... ,e)
        (void)]
      [`(,primop ,vs ...)
        (void)]))

  (define (just-exprs-lang-v9-template-triv t)
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

  (define (just-exprs-lang-v9-template-primop p)
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

  (void))

(define/contract (exprs-unsafe-lang-v9-template p)
  (-> exprs-unsafe-lang-v9? any/c)

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

  (define (exprs-unsafe-lang-v9-template-p p)
    (match p
      [`(module (define ,alocs (lambda (,params ...) ,vs)) ... ,v)
        (void)]))

  (define (exprs-unsafe-lang-v9-template-value v)
    (match v
      [`(unsafe-procedure-call ,v ,vs ...)
        (void)]
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,v1 ,v2 ,v3)
        (void)]
      [`(begin ,es ... ,v)
        (void)]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (void)]
      [triv (void)]))

  (define (exprs-unsafe-lang-v9-template-effect e)
    (match e
      [`(begin ,es ... ,e)
        (void)]
      [`(,primop ,vs ...)
        (void)]))

  (define (exprs-unsafe-lang-v9-template-triv t)
    (match t
      [#t
       (void)]
      [#f
       (void)]
      ['empty
       (void)]
      ['(void)
       (void)]
      [`(error ,uint8)
        (void)]
      [(? ascii-char-literal?)
       (void)]
      [`(lambda (,as ...) ,v)
        (void)]
      [(? aloc?)
       (void)]
      [(? fixnum?)
       (void)]))

  (define (exprs-unsafe-lang-v9-template-primop p)
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

  (void))

(define/contract (exprs-unsafe-data-lang-v9-template p)
  (-> exprs-unsafe-data-lang-v9? any/c)

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

  (define (exprs-unsafe-data-lang-v9-template-p p)
    (match p
      [`(module (define ,alocs (lambda (,params ...) ,vs)) ... ,v)
        (void)]))

  (define (exprs-unsafe-data-lang-v9-template-value v)
    (match v
      [`(call ,v ,vs ...)
        (void)]
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,v1 ,v2 ,v3)
        (void)]
      [`(begin ,es ... ,v)
        (void)]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (void)]
      [triv (void)]))

  (define (exprs-unsafe-data-lang-v9-template-effect e)
    (match e
      [`(begin ,es ... ,e)
        (void)]
      [`(,primop ,vs ...)
        (void)]))

  (define (exprs-unsafe-data-lang-v9-template-triv t)
    (match t
      [#t
       (void)]
      [#f
       (void)]
      ['empty
       (void)]
      ['(void)
       (void)]
      [`(error ,uint8)
        (void)]
      [(? ascii-char-literal?)
       (void)]
      [`(lambda (,as ...) ,v)
        (void)]
      [(? aloc?)
       (void)]
      [(? fixnum?)
       (void)]))

  (define (exprs-unsafe-data-lang-v9-template-primop p)
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

  (void))
