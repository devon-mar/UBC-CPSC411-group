#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(define (addr? a)
  (-> any/c boolean?)
  (match a
    [`(,fbp - ,dispoffset)
      #:when (and
        (frame-base-pointer-register? fbp)
        (dispoffset? dispoffset))
      #t]
    [_ #f]))

(define (loc? loc)
  (-> any/c boolean?)
  (or (register? loc) (addr? loc)))

(define (triv? t)
  (-> any/c boolean?)
  (or (register? t) (int64? t)))

(define (binop? b)
  (-> any/c boolean?)
  (and 
    (member b '(* +))
    #t))

(define (paren-x64-v2-template p)
  (-> paren-x64-v2? any/c)

  (define (loc? loc)
    (-> any/c boolean?)
    (or (register? loc) (addr? loc)))

  (define (paren-x64-v2-template-p p)
    (match p
      [`(begin ,s ...) (TODO)]))

  (define (paren-x64-v2-template-triv t)
    (match t
      [(? int64?) t]
      [reg (TODO)]))

  (define (paren-x64-v2-template-s s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (TODO)]
      [`(set! ,reg (,binop ,reg ,loc))
        (TODO)]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (TODO)]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (TODO)]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (TODO)]
      [`(set! ,addr ,reg)
        (TODO)]))
  (TODO))

(define (values-lang-v3-template p)
  (define (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))
  (define (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))
  (define (values-lang-v3-template-tail t)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (TODO)]
      [v (TODO)]))

  (define (values-lang-v3-template-value v)
    (match v
      [triv #:when (triv? triv) (TODO)]
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        (TODO)]
      [`(let ([,xs ,vs] ...) ,value)
        (TODO)]))

  (define (values-lang-v3-template-p p)
    (match p
      [`(module ,tail)
        (TODO)]))

  (TODO))

(define (values-unique-lang-v3-template p)
  (define (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))
  (define (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))
  (define (values-unique-lang-v3-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (values-unique-lang-v3-template-tail t)
    (match t
      [`(let ([,alocs ,vs] ...) ,tail)
        (TODO)]
      [v (TODO)]))

  (define (values-unique-lang-v3-template-value v)
    (match v
      [triv #:when (triv? triv) (TODO)]
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        (TODO)]
      [`(let ([,alocs ,vs] ...) ,value)
        (TODO)]))

  (define (values-unique-lang-v3-template-p p)
    (match p
      [`(module ,tail)
        (TODO)]))

  (TODO))

(define (imp-mf-lang-v3-template p)
  (define (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))
  (define (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))
  (define (imp-mf-lang-v3-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (imp-mf-lang-v3-template-effect e)
    (match e
      [`(set! ,aloc ,value) (TODO)]
      [`(begin es ..., e) (TODO)]))

  (define (imp-mf-lang-v3-template-tail t)
    (match t
      [`(begin ,es ... ,tail) (TODO)]
      [v (TODO)]))

  (define (imp-mf-lang-v3-template-value v)
    (match v
      [triv #:when (triv? triv) (TODO)]
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        (TODO)]
      [`(begin ,es ... ,value) (TODO)]))

  (define (imp-mf-lang-v3-template-p p)
    (match p
      [`(module ,tail)
        (TODO)]))

  (TODO))

(define (imp-cmf-lang-v3-template p)
  (define (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))
  (define (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))
  (define (imp-cmf-lang-v3-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (imp-cmf-lang-v3-template-effect e)
    (match e
      [`(set! ,aloc ,value) (TODO)]
      [`(begin es ..., e) (TODO)]))

  (define (imp-cmf-lang-v3-template-tail t)
    (match t
      [`(begin ,es ... ,tail) (TODO)]
      [v (TODO)]))

  (define (imp-cmf-lang-v3-template-value v)
    (match v
      [triv #:when (triv? triv) (TODO)]
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        (TODO)]))

  (define (imp-cmf-lang-v3-template-p p)
    (match p
      [`(module ,tail)
        (TODO)]))

  (TODO))

(define (asm-lang-v2-template p)
  (define (asm-lang-v2-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (asm-lang-v2-template-effect e)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv)) (TODO)]
      [`(set! ,aloc ,triv) (TODO)]
      [`(begin ,es ..., e) (TODO)]))

  (define (asm-lang-v2-template-tail t)
    (match t
      [`(halt ,triv) (TODO)]
      [`(begin ,es ... ,tail) (TODO)]))

  (define (asm-lang-v2-template-p p)
    (match p
      [`(module ,info ,tail)
        (TODO)]))

  (TODO))

(define (asm-lang-v2/locals-template p)
  (define (asm-lang-v2/locals-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (asm-lang-v2/locals-template-effect e)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv)) (TODO)]
      [`(set! ,aloc ,triv) (TODO)]
      [`(begin ,es ..., e) (TODO)]))

  (define (asm-lang-v2/locals-template-tail t)
    (match t
      [`(halt ,triv) (TODO)]
      [`(begin ,es ... ,tail) (TODO)]))

  (define (asm-lang-v2/locals-template-p p)
    (match p
      [`(module ,info ,tail)
        (TODO)]))

  (TODO))

(define (asm-lang-v2/assignments-template p)
  (define (asm-lang-v2/assignments-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (asm-lang-v2/assignments-template-effect e)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv)) (TODO)]
      [`(set! ,aloc ,triv) (TODO)]
      [`(begin ,es ..., e) (TODO)]))

  (define (asm-lang-v2/assignments-template-tail t)
    (match t
      [`(halt ,triv) (TODO)]
      [`(begin ,es ... ,tail) (TODO)]))

  (define (asm-lang-v2/assignments-template-p p)
    (match p
      [`(module ,info ,tail)
        (TODO)]))

  (TODO))

(define (nested-param-asm-lang-v2-template p)
  (define (nested-param-asm-lang-v2-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (nested-param-asm-lang-v2-template-loc l)
    (match l
      [reg #:when (register? reg) (TODO)]
      [fvar (TODO)]))

  (define (nested-param-asm-lang-v2-template-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv)) (TODO)]
      [`(set! ,loc ,triv) (TODO)]
      [`(begin ,es ... ,effect) (TODO)]))

  (define (nested-param-asm-lang-v2-template-tail t)
    (match t
      [`(begin ,es ... ,tail) (TODO)]
      [`(halt ,triv) (TODO)]))

  (TODO))

(define (param-asm-lang-v2-template p)
  (define (param-asm-lang-v2-template-fvar fv)
    (match fv 
      [fvar #:when (fvar? fvar) (TODO)]
      [reg (TODO)]))

  (define (param-asm-lang-v2-template-triv t)
    (match t
      [int64 #:when (int64? int64) (TODO)]
      [aloc (TODO)]))

  (define (param-asm-lang-v2-template-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv)) (TODO)]
      [`(set! ,loc ,triv) (TODO)]))

  (define (param-asm-lang-v2-template-p p)
    (match p
      [`(begin ,es ... (halt ,triv)) (TODO)]))

  (TODO))

(define (paren-x64-fvars-v2-template p)
  (define (loc? l)
    (-> any/c boolean?)
    (or (register? l) (fvar? l)))

  (define (paren-x64-fvars-v2-template-triv t)
    (match t
      [reg #:when (register? reg) (TODO)]
      [int64 (TODO)]))

  (define (paren-x64-fvars-v2-template-loc l)
    (match l
      [reg #:when (register? reg) (TODO)]
      [fvar (TODO)]))

  (define (paren-x64-fvars-v2-template-s s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (TODO)]
      [`(set! ,reg (,binop ,reg ,loc))
        (TODO)]
      [`(set! ,fvar ,int32)
        #:when (and (fvar? fvar) (int32? int32))
        (TODO)]
      [`(set! ,fvar ,reg)
        #:when (and (fvar? fvar) (register? reg))
        (TODO)]
      [`(set! ,reg ,loc)
        #:when (and (register? reg) (loc? loc))
        (TODO)]
      [`(set! ,reg ,triv)
        (TODO)]))
  (define (paren-x64-fvars-v2-template-p p)
    (match p
      [`(begin ,s ...) (TODO)]))

  (TODO))
