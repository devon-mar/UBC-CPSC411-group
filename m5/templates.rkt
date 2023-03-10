#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(define/contract (proc-imp-cmf-lang-v5-template p)
  (-> proc-imp-cmf-lang-v5? any/c)

  (define (proc-imp-cmf-lang-v5-template-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,tails)) ... ,tail)
        (void)]))

  (define (proc-imp-cmf-lang-v5-template-pred p)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,p)
        (void)]
      [`(begin ,effects ... ,pred)
        (void)]
      [`(if ,p1 ,p2 ,p3)
        (void)]
      [`(,relop ,o1 ,o2)
        (void)]))

  (define (proc-imp-cmf-lang-v5-template-tail t)
    (match t
      [`(call ,triv ,os ...)
        (void)]
      [`(begin ,es ... ,t)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [value (void)]))

  (define (proc-imp-cmf-lang-v5-template-value v)
    (match v
      [`(,b ,o1 ,o2)
        (void)]
      [triv (void)]))

  (define (proc-imp-cmf-lang-v5-template-effect e)
    (match e
      [`(set! ,a ,v)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]))

  (define (proc-imp-cmf-lang-v5-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  (define (proc-imp-cmf-lang-v5-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
       (void)]))

  (define (proc-imp-cmf-lang-v5-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  (define (proc-imp-cmf-lang-v5-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))
