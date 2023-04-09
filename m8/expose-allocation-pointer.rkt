#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

;; Milestone 8 Exercise 10
;;
;; Compiles asm-alloc-lang-v8 to asm-pred-lang-v8
;; Converts alloc effects into 
(define/contract (expose-allocation-pointer p)
  (-> asm-alloc-lang-v8? asm-pred-lang-v8?)

  ;; p -> p
  (define (expose-allocation-pointer-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
          ,info
          ,@(map expose-allocation-pointer-proc labels infos tails)
          ,(expose-allocation-pointer-tail tail))]))

  ;; label info tail -> proc
  ;; proc ::= (define label info tail)
  (define (expose-allocation-pointer-proc label info tail)
    `(define ,label ,info ,(expose-allocation-pointer-tail tail)))

  ;; pred -> pred
  (define (expose-allocation-pointer-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,pred)
        `(not ,(expose-allocation-pointer-pred pred))]
      [`(begin ,effects ... ,pred)
        `(begin
          ,@(map expose-allocation-pointer-effect effects)
          ,(expose-allocation-pointer-pred pred))]
      [`(if ,p1 ,p2 ,p3)
        `(if ,(expose-allocation-pointer-pred p1)
            ,(expose-allocation-pointer-pred p2)
            ,(expose-allocation-pointer-pred p3))]
      [`(,r ,opand1 ,opand2)
        p]))

  ;; tail -> tail
  (define (expose-allocation-pointer-tail t)
    (match t
      [`(jump ,trg ,locs ...)
        t]
      [`(begin ,effects ... ,tail)
        `(begin
          ,@(map expose-allocation-pointer-effect effects)
          ,(expose-allocation-pointer-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
        `(if
          ,(expose-allocation-pointer-pred pred)
          ,(expose-allocation-pointer-tail tail1)
          ,(expose-allocation-pointer-tail tail2))]))

  ;; effect -> effect
  (define (expose-allocation-pointer-effect e)
    (match e
      [`(set! ,loc (alloc ,idx))
        (define hbp (current-heap-base-pointer-register))
        `(begin
          (set! ,loc ,hbp)
          (set! ,hbp (+ ,hbp ,idx)))]
      [`(begin ,effects ... ,effect)
        `(begin
          ,@(map expose-allocation-pointer-effect effects)
          (expose-allocation-pointer-effect effect))]
      [`(if ,p ,e1 ,e2)
        `(if
          ,(expose-allocation-pointer-pred p)
          ,(expose-allocation-pointer-effect e1)
          ,(expose-allocation-pointer-effect e2))]
      [`(return-point ,label ,tail)
        `(return-point
          ,label
          ,(expose-allocation-pointer-tail tail))]
      [`(set! ,loc1 (mref ,loc2 ,idx))
        e]
      [`(set! ,loc1 (,binop ,loc1 ,opand))
        e]
      [`(mset! ,loc ,idx ,triv)
        e]
      [`(set! ,loc ,triv)
        e]))

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

  (expose-allocation-pointer-p p))


(module+ test
  (require rackunit)

  ;; Checks allocs in pred, effect, and in the effect of a proc
  (check-equal?
    (expose-allocation-pointer
      `(module
        ((new-frames (())))
        (define L.foo.1
          ((new-frames ()))
          (begin
            (set! fv0 (alloc 1))
            (jump r15)))
        (begin
          (if 
            (begin
              (set! r9 (alloc 1))
              (mset! r9 0 0)
              (set! r8 (mref r9 0))
              (= tmp.4 2))
            (begin
              (set! r8 (alloc 3))
              (jump r15))
            (begin
              (set! rax (- rax 8))
              (jump r15))))))
    `(module
        ((new-frames (())))
        (define L.foo.1
          ((new-frames ()))
          (begin
            (begin
              (set! fv0 ,(current-heap-base-pointer-register))
              (set! ,(current-heap-base-pointer-register) (+ ,(current-heap-base-pointer-register) 1)))
            (jump r15)))
        (begin
          (if 
            (begin
              (begin
              (set! r9 ,(current-heap-base-pointer-register))
              (set! ,(current-heap-base-pointer-register) (+ ,(current-heap-base-pointer-register) 1)))
              (mset! r9 0 0)
              (set! r8 (mref r9 0))
              (= tmp.4 2))
            (begin
              (begin
              (set! r8 ,(current-heap-base-pointer-register))
              (set! ,(current-heap-base-pointer-register) (+ ,(current-heap-base-pointer-register) 3)))
              (jump r15))
            (begin
              (set! rax (- rax 8))
              (jump r15)))))
    )



;; Check parameterization works
(check-equal?
  (parameterize ([current-heap-base-pointer-register `r10])
    (expose-allocation-pointer
      `(module
        ((new-frames (())))
        (define L.foo.1
          ((new-frames ()))
          (begin
            (set! fv0 (alloc 1))
            (jump r15)))
        (begin
          (if 
            (begin
              (set! r9 (alloc 1))
              (mset! r9 0 0)
              (set! r8 (mref r9 0))
              (= tmp.4 2))
            (begin
              (set! r8 (alloc 3))
              (jump r15))
            (begin
              (set! rax (- rax 8))
              (jump r15)))))))
    `(module
        ((new-frames (())))
        (define L.foo.1
          ((new-frames ()))
          (begin
            (begin
              (set! fv0 r10)
              (set! r10 (+ r10 1)))
            (jump r15)))
        (begin
          (if 
            (begin
              (begin
                (set! r9 r10)
                (set! r10 (+ r10 1)))
              (mset! r9 0 0)
              (set! r8 (mref r9 0))
              (= tmp.4 2))
            (begin
              (begin
              (set! r8 r10)
              (set! r10 (+ r10 3)))
              (jump r15))
            (begin
              (set! rax (- rax 8))
              (jump r15)))))
    )
)
