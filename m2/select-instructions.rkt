#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide select-instructions)

;; Milestone 4 Exercise 17
;;
;; Compiles Imp-cmf-lang v4 to Asm-pred-lang v4,
;; selecting appropriate sequences of abstract assembly instructions to
;; implement the operations of the source language.
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v4? asm-pred-lang-v4?)


  (define (select-instructions-p p)
    (match p
      [`(module ,tail)
       `(module
          ()
          ,(select-instructions-tail tail))]))

  (define (select-instructions-pred p)
    (match p
      ['(true)
       p]
      ['(false)
       p]
      [`(not ,pred)
        `(not ,(select-instructions-pred pred))]
      [`(begin ,effects ... ,pred)
        `(begin
           ,@(map select-instructions-effect effects)
           ,(select-instructions-pred pred))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(select-instructions-pred p1)
           ,(select-instructions-pred p2)
           ,(select-instructions-pred p3))]
      [`(,relop ,triv1 ,triv2)
        (select-instructions-triv
          triv1
          (lambda (aloc) `(,relop ,aloc ,triv2)))]))

  (define (select-instructions-tail t)
    (match t
      [`(begin ,effects ... ,tail)
       `(begin
          ,@(map select-instructions-effect effects)
          ,(select-instructions-tail tail))]
      [`(if ,pred ,t1 ,t2)
       `(if
          ,(select-instructions-pred pred)
          ,(select-instructions-tail t1)
          ,(select-instructions-tail t2))]
      ;; value
      [_
        (select-instructions-value
          t
          (lambda (triv) `(halt ,triv)))]))

  ;; If v is a binop, introduces a begin with the result of
  ;; (f triv) as the last statement, triv being some temporary
  ;; aloc which holds the result of the binop.
  ;; Otherwise returns (f v).
  ;;
  ;; v: imp-cmf-lang-v3 value
  ;; f: (triv? -> asm-pred-lang-v3 tail or effect)
  ;; -> asm-pred-lang-v3 tail or effect
  (define (select-instructions-value v f)
    (match v
      [`(,binop ,t1 ,t2)
        (define tmp (fresh "tmp"))
        `(begin
           (set! ,tmp ,t1)
           (set! ,tmp (,binop ,tmp ,t2))
           ,(f tmp))]
      ;; triv
      [_ (f v)]))

  (define (select-instructions-effect e)
    (match e
      [`(set! ,aloc ,value)
        (select-instructions-value
          value
          (lambda (t) `(set! ,aloc ,t)))]
      ;; modified template - removed extra e since we assume input is valid
      [`(begin ,effects ...)
        `(begin
           ,@(map select-instructions-effect effects))]
      [`(if ,pred ,e1 ,e2)
        `(if
           ,(select-instructions-pred pred)
           ,(select-instructions-effect e1)
           ,(select-instructions-effect e2))]))

  ;; If t is an int64, introduces a temp aloc
  ;; with t's value and calls f with the temp variable.
  ;; Otherwise (f t) if t is already an aloc.
  ;;
  ;; t: triv?
  ;; f: (aloc? -> pred)
  ;; -> pred
  (define (select-instructions-triv t f)
    (match t
      [(? aloc?) (f t)]
      [(? int64?)
       (define tmp (fresh "tmp"))
       `(begin
          (set! ,tmp ,t)
          ,(f tmp))]))

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

  (define (check-42 p)
    (check-equal?
      (interp-asm-pred-lang-v4 (select-instructions p))
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
  )
