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

(define/contract (imp-cmf-lang-v4-template p)
  (-> imp-cmf-lang-v4? any/c)                


  (define (imp-cmf-lang-v4-template-p p)
    (match p
      [`(module ,tail)
        (void)]))

  (define (imp-cmf-lang-v4-template-pred p)
    (match p
      ['(true)
       (void)]
      ['(false)
       (void)]
      [`(not ,pred)
        (void)]
      [`(begin ,effects ... ,pred)
        (void)]
      [`(if ,p1 ,p2 ,p3)
        (void)]
      [`(,relop ,triv1 ,triv2)
        (void)]))

  (define (imp-cmf-lang-v4-template-tail t)
    (match t
      [`(begin ,effects ... ,tail)
        (void)]
      [`(if ,pred ,t1 ,t2)
        (void)]
      [value (void)]))

  (define (imp-cmf-lang-v4-template-value v)
    (match v
      [`(,binop ,t1 ,t2)
        (void)]
      [triv (void)]))

  (define (imp-cmf-lang-v4-template-effect e)
    (match e
      [`(set! ,aloc ,value)
        (void)]
      [`(begin ,effects ... ,effect)
        (void)]
      [`(if ,pred ,e1 ,e2)
        (void)]))

  (define (imp-cmf-lang-v4-template-triv t)
    (match t
      [(? aloc?) (void)]
      [(? int64?) (void)]))

  (define (imp-cmf-lang-v4-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  (define (imp-cmf-lang-v4-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))
