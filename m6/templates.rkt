#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6)

(define/contract (proc-imp-cmf-lang-v6-template p)
  (-> proc-imp-cmf-lang-v6? any/c)

  (define (proc-imp-cmf-lang-v6-template-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,entries)) ... ,entry)
        (void)]))

  (define (proc-imp-cmf-lang-v6-template-entry e)
    (match e
      [tail (void)]))

  (define (proc-imp-cmf-lang-v6-template-pred p)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,p)
        (void)]
      [`(begin ,es ... ,p)
        (void)]
      [`(if ,p1 ,p2 ,p3)
        (void)]
      [`(,r ,o1 ,o2)
        (void)]))

  (define (proc-imp-cmf-lang-v6-template-tail t)
    (match t
      [`(call ,t ,os ...)
        (void)]
      [`(begin ,es ... ,t)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [value (void)]))

  (define (proc-imp-cmf-lang-v6-template-value v)
    (match v
      [`(call ,t ,os ...)
        (void)]
      [`(,b ,o1 ,o2)
        (void)]
      [triv (void)]))

  (define (proc-imp-cmf-lang-v6-template-effect e)
    (match e
      [`(set! ,a ,v)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]))

  (define (proc-imp-cmf-lang-v6-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))
  
  (define (proc-imp-cmf-lang-v6-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  (define (proc-imp-cmf-lang-v6-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))

  (define (proc-imp-cmf-lang-v6-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))
