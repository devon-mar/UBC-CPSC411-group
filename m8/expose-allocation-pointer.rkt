#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

;; Compiles asm-alloc-lang-v8 to asm-pred-lang-v8
;; Converts alloc effects into 
(define/contract (expose-allocation-pointer p)
  (asm-alloc-lang-v8? -> asm-pred-lang-v8? any/c)

  (define (expose-allocation-pointer-p p)
    (match p
      [`(module ,info ,procs ... ,tail)
        (void)]))
  
  (define (expose-allocation-pointer-proc p)
    (match p
      [`(define ,label ,info ,tail)
        (void)]))

  (define (expose-allocation-pointer-pred p)
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
      [`(,r ,opand1 ,opand2)
        (void)]))

  (define (expose-allocation-pointer-tail t)
    (match t
      [`(jump ,trg ,locs ...)
        (void)]
      [`(begin ,effects ... ,tail)
        (void)]
      [`(if ,pred ,tail1 ,tail2)
        (void)]))

  (define (expose-allocation-pointer-effect e)
    (match e
      [`(set! ,loc (alloc ,idx))
        (void)]
      [`(begin ,es ... ,e)
        (void)]
      [`(if ,p ,e1 ,e2)
        (void)]
      [`(return-point ,label ,tail)
        (void)]
      [`(set! ,loc1 (mref ,loc2 ,idx))
        (void)]
      [`(set! ,loc1 (,binop ,loc1 ,opand))
        (void)]
      [`(mset! ,loc ,idx ,triv)
        (void)]
      [`(set! ,loc ,triv)
        (void)]))

  ;; Unused
  #;
  (define (expose-allocation-pointer-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  ;; Unused
  #;
  (define (expose-allocation-pointer-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  ;; Unused
  #;
  (define (expose-allocation-pointer-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]))

  ;; Unused
  #;
  (define (expose-allocation-pointer-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (void))


(module+ test
  (require rackunit)

)
