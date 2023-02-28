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

(require
  cpsc411/compiler-lib)

(define (asm-lang-v2/locals-template p)
  (define (asm-lang-v2/locals-template-p p)
    (match p
      [`(module ,info ,tail)
        (void)]))

  (define (asm-lang-v2/locals-template-tail t)
    (match t
      [`(halt ,triv)
        (void)]
      [`(begin ,effects ... ,tail)
        (void)]))

  (define (asm-lang-v2/locals-template-effect e)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv))
        (void)]
      [`(set! ,aloc ,triv)
        (void)]
      [`(begin ,effects ... ,effect)
        (void)]))

  (define (asm-lang-v2/locals-template-triv t)
    (match t
      [(? int64?) (void)]
      [(? aloc?) (void)]))

  (define (asm-lang-v2/locals-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  (void))
