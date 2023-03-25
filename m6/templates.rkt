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

(define/contract (asm-pred-lang-v6-template p)
  (-> asm-pred-lang-v6? any/c)

  (define (asm-pred-lang-v6-template-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        (void)]))

  (define (asm-pred-lang-v6-template-pred p)
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

  (define (asm-pred-lang-v6-template-tail t)
    (match t
      [`(jump ,trg ,locs ...)
        (void)]
      [`(begin ,es ... ,t)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]))

  (define (asm-pred-lang-v6-template-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,opand))
        (void)]
      [`(set! ,loc ,triv)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]
      [`(return-point ,label ,tail)
        (void)]))

  (define (asm-pred-lang-v6-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc (void)]))

  (define (asm-pred-lang-v6-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand (void)]))

  (define (asm-pred-lang-v6-template-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  (define (asm-pred-lang-v6-template-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  (define (asm-pred-lang-v6-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))
  
  (define (asm-pred-lang-v6-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))

(define/contract (imp-cmf-lang-v6-template p)
  (-> imp-cmf-lang-v6? any/c)

  (define (imp-cmf-lang-v6-template-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        (void)]))

  (define (imp-cmf-lang-v6-template-pred p)
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

  (define (imp-cmf-lang-v6-template-tail t)
    (match t
      [`(begin ,es ... ,t)
        (void)]
      [`(if ,p ,t1 ,t2)
        (void)]
      [`(jump ,trg ,locs ...)
        (void)]))

  (define (imp-cmf-lang-v6-template-value v)
    (match v
      [`(,b ,o1 ,o2)
        (void)]
      [triv (void)]))

  (define (imp-cmf-lang-v6-template-effect e)
    (match e
      [`(set! ,l ,v)
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]
      [`(return-point ,l ,t)
        (void)]))

  (define (imp-cmf-lang-v6-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))
  
  (define (imp-cmf-lang-v6-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  (define (imp-cmf-lang-v6-template-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  (define (imp-cmf-lang-v6-template-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  (define (imp-cmf-lang-v6-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))

  (define (imp-cmf-lang-v6-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))

(define/contract (para-asm-lang-v6-template p)
  (-> para-asm-lang-v6? any/c)

  (define (para-asm-lang-v6-template-p p)
    (match p
      [`(begin ,s ...)
        (void)]))

  (define (para-asm-lang-v6-template-s s)
    (match s
      [`(set! ,loc (,b ,loc ,o))
        (void)]
      [`(set! ,loc ,triv)
        (void)]
      [`(jump ,trg)
        (void)]
      [`(with-label ,l ,s)
        (void)]
      [`(compare ,l ,o)
        (void)]
      [`(jump-if ,r ,t)
        (void)]))

  (define (para-asm-lang-v6-template-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  (define (para-asm-lang-v6-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc
        (void)]))

  (define (para-asm-lang-v6-template-trg t)
    (match t
      [(? label?)
       (void)]
      [loc
       (void)]))

  (define (para-asm-lang-v6-template-loc l)
    (match l
      [(? register?)
       (void)]
      [addr (void)]))

  (define (para-asm-lang-v6-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))

  (define (para-asm-lang-v6-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))

(define/contract (paren-x64-v6-template p)
  (-> paren-x64-v6? any/c)

  (define/contract (trg? t)
    (-> any/c boolean?)
    (or (register? t) (label? t)))

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (trg? t) (int64? t)))

  (define (paren-x64-v6-template-p p)
    (match p
      [`(begin ,s ...)
        (void)]))

  (define (paren-x64-v6-template-s s)
    (match s
      [`(set! ,reg (,b ,reg ,int32))
        #:when (int32? int32)
        (void)]
      [`(set! ,reg (,b ,reg ,loc))
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

  (define (paren-x64-v6-template-trg t)
    (match t
      [(? register?)
        (void)]
      [(? label?)
       (void)]))

  (define (paren-x64-v6-template-triv t)
    (match t
      [(? int64?)
       (void)]
      [trg 
        (void)]))

  (define (paren-x64-v6-template-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? register?)
       (void)]))

  (define (paren-x64-v6-template-loc l)
    (match l
      [(? register?)
       (void)]
      [addr
        (void)]))

  (define (paren-x64-v6-template-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))

  (define (paren-x64-v6-template-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))
