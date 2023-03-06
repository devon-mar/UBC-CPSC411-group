#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(define/contract (nested-asm-lang-v4-template p)
  (-> nested-asm-lang-v4? any/c)

  (define (nested-asm-lang-v4-template-p p)
    (match p
      [`(module ,tail)
        (void)]))

  (define (nested-asm-lang-v4-template-pred p)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,pred)
        (void)]
      [`(begin ,effects ... ,pred)
        (void)]
      [`(if ,p1 ,p2 ,p3)
        (void)]
      [`(,relop ,loc ,triv)
        (void)]))

  (define (nested-asm-lang-v4-template-tail t)
    (match t
      [`(halt ,triv)
        (void)]
      [`(begin ,effects ... ,tail)
        (void)]
      [`(if ,pred ,t1 ,t2)
        (void)]))

  (define (nested-asm-lang-v4-template-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
        (void)]
      [`(set! ,loc ,triv)
        (void)]
      [`(begin ,effects ... ,effect)
        (void)]
      [`(if ,pred ,e1 ,e2)
        (void)]))

  (define (nested-asm-lang-v4-template-triv t)
    (match t
      [(? int64?)
       (void)]
      [loc
        (void)]))

  (define (nested-asm-lang-v4-template-loc l)
    (match l
      [(? register?)
       (void)]
      [(? fvar?)
       (void)]))

  (void))
