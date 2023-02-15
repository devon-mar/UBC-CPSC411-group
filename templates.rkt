#lang racket

(require cpsc411/compiler-lib)

(define (asm-lang-v2/undead-template p)
  (define (asm-lang-v2/undead-template-binop b)
    (match b
      ['* (TODO)]
      ['+ (TODO)]))

  (define (asm-lang-v2/undead-template-triv t)
    (match t
      [(? int64?) (TODO)]
      [aloc (TODO)]))

  (define (asm-lang-v2/undead-template-tail t)
    (match t
      [`(halt ,triv)
        (TODO)]
      [`(begin ,effects ... ,tail)
        (TODO)]))

  (define (asm-lang-v2/undead-template-effect e)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv))
        (TODO)]
      [`(set! ,aloc ,triv)
        (TODO)]
      [`(begin ,es ... ,etail)
        (TODO)]))

  (define (asm-lang-v2/undead-template-p p)
    (match p
      [`(module ,info ,tail)
        (TODO)]))
  (TODO))

