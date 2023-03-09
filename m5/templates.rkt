#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(define/contract (values-lang-v5-template p)
  (-> values-lang-v5? any/c)

  (define (values-lang-v5-template-p p)
    (match p
      [`(module (define ,x1s (lambda (,x2s ...) ,tails)) ... ,tail)
        (void)]))

  (define (values-lang-v5-template-pred p)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,pred)
        (void)]
      [`(let ([,xs ,vs] ...) ,pred)
        (void)]
      [`(if ,p1 ,p2 ,p3)
        (void)]
      [`(,relop ,t1 ,t2)
        (void)]))

  (define (values-lang-v5-template-tail t)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [`(call ,x ,ts ...)
        (void)]
      [value (void)]))

  (define (values-lang-v5-template-value v)
    (match v
      [`(if ,p ,v1 ,v2)
        (void)]
      [`(let ([,xs ,vs] ...) ,v)
        (void)]
      [`(,binop ,t1 ,t2)
        (void)]
      [triv (void)]))

  (define (values-lang-v5-template-triv t)
    (match t
      [(? int64?) (void)]
      [(? name?) (void)]))

  (define (values-lang-v5-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  
  (define (values-lang-v5-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))

