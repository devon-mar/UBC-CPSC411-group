#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide select-instructions)

;; Milestone 4 Exercise 17
;; Milestone 5 Exercise 5
;;
;; Compiles Imp-cmf-lang v5 to Asm-pred-lang v5,
;; selecting appropriate sequences of abstract assembly instructions to
;; implement the operations of the source language.
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v5? asm-pred-lang-v5?)

  ;; Returns an asm-pred-lang-v5-proc from the label and tail
  ;; of an imp-cmf-lang-v5-proc.
  ;;
  ;; tail: imp-cmf-lang-v5-tail
  ;; -> asm-pred-lang-v5-proc
  (define/contract (select-instructions-proc label tail)
    (-> label? any/c any/c)
    `(define
       ,label
       ()
       ,(select-instructions-tail tail)))
    
  ;; imp-cmf-lang-v5-p -> asm-pred-lang-v5-p
  (define (select-instructions-p p)
    (match p
      [`(module (define ,labels ,tails) ... ,tail)
        `(module
           ()
           ,@(map select-instructions-proc labels tails)
           ,(select-instructions-tail tail))]))

  ;; imp-cmf-lang-v5-pred -> asm-pred-lang-v5-pred
  (define (select-instructions-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(select-instructions-pred p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map select-instructions-effect es)
           ,(select-instructions-pred p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(select-instructions-pred p1)
           ,(select-instructions-pred p2)
           ,(select-instructions-pred p3))]
      ;; first argument must be a loc - (rloc/aloc)
      [`(,r ,o1 ,o2)
        (select-instructions-opand
          o1
          (lambda (l) `(,r ,l ,o2)))]))

  ;; imp-cmf-lang-v5-tail -> asm-tail-lang-v5-tail
  (define (select-instructions-tail t)
    (match t
      [`(jump ,_ ,_ ...)
        t]
      [`(begin ,es ... ,tail)
        `(begin
           ,@(map select-instructions-effect es)
           ,(select-instructions-tail tail))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(select-instructions-pred p)
           ,(select-instructions-tail t1)
           ,(select-instructions-tail t2))]
      ;; value
      [_ (select-instructions-value
           t
           (lambda (t) `(halt ,t)))]))

  ;; If v is a binop, introduces a begin with the result of
  ;; (f triv) as the last statement, triv being some temporary
  ;; aloc which holds the result of the binop.
  ;; Otherwise returns (f v).
  ;;
  ;; v: imp-cmf-lang-v5-value
  ;; f: (triv? -> asm-pred-lang-v5 tail or effect)
  ;; -> asm-pred-lang-v5 tail or effect
  (define (select-instructions-value v f)
    (match v
      [`(,b ,o1 ,o2) 
        (define tmp (fresh "tmp"))
        `(begin
           (set! ,tmp ,o1)
           (set! ,tmp (,b ,tmp ,o2))
           ,(f tmp))]
      [_ (f v)]))

  ;; imp-cmf-lang-v5-effect -> asm-effect-lang-v5-effect
  (define (select-instructions-effect e)
    (match e
      ;; we must ensure v is a triv
      [`(set! ,l ,v)
        (select-instructions-value
          v
          (lambda (t) `(set! ,l ,t)))]
      ;; modified template - removed tail effect since we assume valid input
      [`(begin ,es ...)
        `(begin ,@(map select-instructions-effect es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(select-instructions-pred p)
           ,(select-instructions-effect e1)
           ,(select-instructions-effect e2))]))

  ;; Intoruce a tmp aloc and begin if o is an int64
  ;; with (f loc) as the last statement.
  ;;
  ;; o: imp-cmf-lang-v5-opand
  ;; f: (loc -> asm-pred-lang-v5-effect)
  ;; -> asm-pred-lang-v5-effect
  (define (select-instructions-opand o f)
    (match o
      [(? int64?)
       (define tmp (fresh "tmp"))
       `(begin
          (set! ,tmp ,o)
          ,(f tmp))]
          
      [loc (f loc)]))

  ;; not used
  #;
  (define (select-instructions-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  ;; not used
  #;
  (define (select-instructions-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  ;; not used
  #;
  (define (select-instructions-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  
  ;; not used
  #;
  (define (select-instructions-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (select-instructions-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-asm-pred-lang-v5 (select-instructions p))
      42))

  ;; basic
  (check-42 '(module 42))
  (check-42 '(module (+ 40 2)))
  (check-42 '(module (* 21 2)))
  
  ;; relop with 2 int64s
  (check-42
    '(module
       (if (= 1 1)
         42
         0)))

  ;; relop with int64, triv
  (check-42
    '(module
       (begin
         (set! x.1 1)
         (if (= 1 x.1)
           42
           0))))

  ;; relop with triv, int64
  ;; (no changes needed)
  (check-42
    '(module
       (begin
         (set! x.1 1)
         (if (= x.1 1)
           42
           0))))

  ;; lots of ifs
  (check-42
   '(module
     (begin
       (if
         (begin
           (set! x.2 10)
           (> x.2 1))
         (set! x.1 42)
         (set! x.1 2))
       (if
         (if (false) (true) (false))
         0
         (if (not (true)) 0 x.1)))))

  ;; M2 tests
  ; simple
  (check-42 '(module 42))

  ; basic binop
  (check-42 '(module (+ 40 2)))

  ; basic set and return
  (check-42 '(module (begin (set! x.1 42) x.1)))

  ; add from two alocs and return result
  (check-42
   '(module
      (begin
        (set! x.1 40)
        (set! x.2 2)
        (+ x.1 x.2))))

  ; nested begins
  (check-42
    '(module
       (begin
         (begin
           (set! x.1 40)
           (set! x.2 2)
           (begin
             (set! x.1 (+ x.1 x.2))))
         x.1)))


  ;; nothing to change here
  (check-42
    '(module
       (begin
         (set! x.2 21)
         (set! x.2 (* x.2 2))
         x.2)))

  (check-42
    '(module
       (begin
         (set! x.2 21)
         (set! z.3 (* x.2 2))
         z.3)))

  ;; M5 tests
  (check-42
    '(module
       (define L.foo.1
         42)
       (jump L.foo.1)))

  (check-42
    '(module
       (define L.foo.1
         (+ 40 2))
       (jump L.foo.1)))

  (check-42
    '(module 
       (define L.foo.1
         (begin
           (set! x.1 (+ 40 2))
           x.1))
       (jump L.foo.1)))

  (check-42
    `(module
       (define L.foo.1
         (if (> 10 2)
           42
           0))
       (jump L.foo.1))))
