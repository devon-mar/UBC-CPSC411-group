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

(define/contract (imp-mf-lang-v5-template p)
  (-> imp-mf-lang-v5? any/c)

  (define (imp-mf-lang-v5-template-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,tails)) ... ,tail)
        (void)]))

  (define (imp-mf-lang-v5-template-pred p)
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

  (define (imp-mf-lang-v5-template-tail t)
    (match t
      [`(begin ,es ... ,tail)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [`(call ,t ,os ...)
        (void)]
      [value (void)]))

  (define (imp-mf-lang-v5-template-value v)
    (match v
      [`(begin ,es ... ,value)
        (void)]
      [`(if ,p ,v1 ,v2)
        (void)]
      [`(,b ,o1 ,o2)
        (void)]
      [triv
        (void)]))

  (define (imp-mf-lang-v5-template-effect e)
    (match e
      [`(set! ,a ,v)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]))

  (define (imp-mf-lang-v5-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  (define (imp-mf-lang-v5-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
       (void)]))

  (define (imp-mf-lang-v5-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  (define (imp-mf-lang-v5-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))

(define/contract (imp-cmf-lang-v5-template p)
  (-> imp-cmf-lang-v5? any/c)

  (define (imp-cmf-lang-v5-template-p p)
    (match p
      [`(module (define ,labels ,tails) ... ,tail)
        (void)]))

  (define (imp-cmf-lang-v5-template-pred p)
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

  (define (imp-cmf-lang-v5-template-tail t)
    (match t
      [`(jump ,trg ,locs ...)
        (void)]
      [`(begin ,es ... ,tail)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [value (void)]))

  (define (imp-cmf-lang-v5-template-value v)
    (match v
      [`(,b ,o1 ,o2) 
        (void)]
      [triv (void)]))

  (define (imp-cmf-lang-v5-template-effect e)
    (match e
      [`(set! ,l ,v)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]))

  (define (imp-cmf-lang-v5-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc (void)]))

  (define (imp-cmf-lang-v5-template-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  (define (imp-cmf-lang-v5-template-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  (define (imp-cmf-lang-v5-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  
  (define (imp-cmf-lang-v5-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))

(define/contract (asm-pred-lang-v5-template p)
  (-> asm-pred-lang-v5? any/c)

  (define (asm-pred-lang-v5-template-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        (void)]))

  (define (asm-pred-lang-v5-template-pred p)
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
      [`(,r ,l ,o)
        (void)]))

  (define (asm-pred-lang-v5-template-tail t)
    (match t
      [`(halt ,o)
        (void)]
      [`(jump ,trg ,locs ...)
        (void)]
      [`(begin ,es ... ,t)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]))

  (define (asm-pred-lang-v5-template-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,opand))
        (void)]
      [`(set! ,loc ,triv)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]))

  (define (asm-pred-lang-v5-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc (void)]))

  (define (asm-pred-lang-v5-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand (void)]))

  (define (asm-pred-lang-v5-template-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  (define (asm-pred-lang-v5-template-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  (define (asm-pred-lang-v5-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  
  (define (asm-pred-lang-v5-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))
