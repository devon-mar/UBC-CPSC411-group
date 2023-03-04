#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

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

(define/contract (paren-x64-v4-template p)
  (-> paren-x64-v4? any/c)

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (trg? t) (int64? t)))

  (define/contract (trg? t)
    (-> any/c boolean?)
    (or (register? t)
        (label? t)))


  (define (paren-x64-v4-template-p p)
    (match p
      [`(begin ,s ...)
        (void)]))

  (define (paren-x64-v4-template-s s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (void)]
      [`(set! ,reg (,binop ,reg ,loc))
        (void)]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (void)]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (void)]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (void)]
      [`(set! ,addr ,trg)
        (void)]
      [`(with-label ,label ,s)
        (void)]
      [`(jump ,trg)
        (void)]
      [`(compare ,reg ,opand)
        (void)]
      [`(jump-if ,relop ,label)
        (void)]))

  (define (paren-x64-v4-template-trg t)
    (match t
      [(? register?) (void)]
      [(? label?) (void)]))

  (define (paren-x64-v4-template-triv t)
    (match t
      [(? trg?) (void)]
      [(? int64?) (void)]))

  (define (paren-x64-v4-template-opand o)
    (match o
      [(? int64?) (void)]
      [(? register?) (void)]))

  (define (paren-x64-v4-template-loc l)
    (match l
      [(? register?) (void)]
      [addr (void)]))

  (void))

(define/contract (para-asm-lang-v4-template p)
  (-> para-asm-lang-v4? any/c)

  (define (para-asm-lang-v4-template-p p)
    (match p
      [`(begin ,s ...)
        (void)]))

  (define (para-asm-lang-v4-template-s s)
    (match s
      [`(halt ,opand)
        (void)]
      [`(set! ,loc ,triv)
        (void)]
      [`(set! ,loc (,binop ,loc ,opand))
        (void)]
      [`(jump ,trg)
        (void)]
      [`(with-label ,label s)
        (void)]
      [`(compare ,loc ,opand)
        (void)]
      [`(jump-if ,relop ,trg)
        (void)]))

  (define (para-asm-lang-v4-template-triv t)
    (match t
      [(? label?) (void)]
      [opand opand]))

  (define (para-asm-lang-v4-template-opand o)
    (match o
      [(? int64?) (void)]
      [loc (void)]))

  (define (para-asm-lang-v4-template-trg t)
    (match t
      [(? label?) (void)]
      [loc (void)]))

  (define (para-asm-lang-v4-template-loc l)
    (match l
      [(? register?) (void)]
      [fvar (void)]))

  (void))
