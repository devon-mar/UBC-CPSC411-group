#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(define/contract (values-unique-lang-v5-template p)
  (-> values-unique-lang-v5? any/c)

  (define (values-unique-lang-v5-template-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,tails)) ... ,tail)
        (void)]))

  (define (values-unique-lang-v5-template-pred p)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,p)
        (void)]
      [`(let ([,as ,vs] ...) ,pred)
        (void)]
      [`(if ,p1 ,p2 ,p3)
        (void)]
      [`(,r ,o1 ,o2)
        (void)]))

  (define (values-unique-lang-v5-template-tail t)
    (match t
      [`(let ([,as ,vs] ...) ,tail)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [`(call ,t ,os ...)
        (void)]
      [value (void)]))

  (define (values-unique-lang-v5-template-value v)
    (match v
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,p ,v1 ,v2)
        (void)]
      [`(,b ,o1 ,o2)
        (void)]
      [triv
        (void)]))

  (define (values-unique-lang-v5-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  (define (values-unique-lang-v5-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
       (void)]))

  (define (values-unique-lang-v5-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  (define (values-unique-lang-v5-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))
