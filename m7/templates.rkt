#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(define/contract (exprs-unsafe-data-lang-v7-template p)
  (-> exprs-unsafe-data-lang-v7? any/c)

  (define/contract (primop? p)
    (-> any/c boolean?)
    (or (binop? p) (unop? p)))


  (define/contract (binop? b)
    (-> any/c boolean?)
    (and (memq
           b
           '(unsafe-fx*
             unsafe-fx+
             unsafe-fx-
             eq?
             unsafe-fx<
             unsafe-fx<=
             unsafe-fx>
             unsafe-fx>=))
         #t))

  (define/contract (unop? u)
    (-> any/c boolean?)
    (and (memq
           u
           '(fixnum?
             boolean?
             empty?
             void?
             ascii-char?
             error?
             not))
         #t))

  (define (exprs-unsafe-data-lang-v7-template-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,values)) ... ,value)
        (void)]))

  (define (exprs-unsafe-data-lang-v7-template-value v)
    (match v
      [`(call ,v ,vs ...)
        (void)]
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,v1 ,v2 ,v3)
        (void)]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (void)]
      [triv
        (void)]))

  (define (exprs-unsafe-data-lang-v7-template-triv t)
    (match t
      [(? label?)
       (void)]
      [(? aloc?)
       (void)]
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
        (void)]
      [(? ascii-char-literal?)
       (void)]))

  (define (exprs-unsafe-data-lang-v7-template-primop p)
    (match p
      [(? binop?)
       (void)]
      [(? unop?)
       (void)]))

  (define (exprs-unsafe-data-lang-v7-template-binop b)
    (match b
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
       (void)]))

  (define (exprs-unsafe-data-lang-v7-template-unop u)
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

  (void))

(define/contract (exprs-unique-lang-v7-template p)
  (-> exprs-unique-lang-v7? any/c)

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and
      (memq b '(* + - < eq? <= > >=))
      #t))

  (define (exprs-unique-lang-v7-template-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,values)) ... ,value)
        (void)]))

  (define (exprs-unique-lang-v7-template-value v)
    (match v
      [`(call ,v ,vs ...)
        (void)]
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,v1 ,v2 ,v3)
        (void)]
      [triv
        (void)]))

  (define (exprs-unique-lang-v7-template-triv t)
    (match t
      [(? label?)
       (void)]
      [(? aloc?)
       (void)]
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
        (void)]
      [(? ascii-char-literal?)
       (void)]
      [prim-f
        (void)]))

  (define (exprs-unique-lang-v7-template-prim-f p)
    (match p
      [(? binop?)
       (void)]
      [unop
        (void)]))

  (define (exprs-unique-lang-v7-template-binop b)
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

  (define (exprs-unique-lang-v7-template-unop u)
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

  (void))
