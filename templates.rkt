#lang racket

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
