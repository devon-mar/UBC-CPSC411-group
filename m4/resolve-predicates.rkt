#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide resolve-predicates)

;; Milestone 4 Exercise 9
;; Milestone 8 Exercise 11
;;
;; Compiles preds in if statements to equivalent if statements or into
;; equivalent instructions
(define/contract (resolve-predicates p)
  (-> block-pred-lang-v8? block-asm-lang-v8?)

  ;; s -> s
  (define (convert-s s)
    (match s
      [`(set! ,loc ,triv) s]
      [`(set! ,loc_1 (mref ,loc_2 ,index)) s]
      [`(set! ,loc_1 (,binop ,loc_1 ,opand)) s]
      [`(mset! ,loc ,index ,triv) s]))

  ;; tail -> tail
  (define (convert-tail t)
    (match t
      [`(jump ,trg) `(jump ,trg)]
      [`(begin ,s ... ,tail)
       `(begin
          ,@(map convert-s s)
          ,(convert-tail tail))]
      [`(if ,pred ,t1 ,t2) (convert-if pred t1 t2)]))

  ;; (if pred (jump trg) (jump trg)) -> (jump trg) | (if pred (jump trg) (jump trg))
  (define (convert-if pred t1 t2)
    (match pred
      [`(true) t1]
      ['(false) t2]
      [`(not ,nested-pred) (convert-if nested-pred t2 t1)]
      [`(,relop ,loc ,opand) `(if (,relop ,loc ,opand) ,t1 ,t2)]))

  ;; b -> b
  (define (convert-b b)
    (match b
      [`(define ,label ,tail) `(define ,label ,(convert-tail tail))]))

  (match p
    [`(module ,b ...)
     `(module ,@(map convert-b b))]))

(module+ test
  (require rackunit)
  ;; No preds is identity function
  (define in1
    '(module
      (define L.start.1
        (begin
          (set! rax 4)
          (jump done)))))
  (define out1 in1)

  ;; Check that if true, replace predicate with the first
  ;; statement
  (define in2
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (true) (jump L.A.1) (jump L.B.1)))))
  (define out2
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (jump L.A.1))))

  ;; Check if false, replace predicate with second statement
  ;; in turnary
  (define in3
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (false) (jump L.A.1) (jump L.B.1)))))
  (define out3
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (jump L.B.1))))

  ;; Check handling not correctly
  (define in4
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (not (< rax 2)) (jump L.A.1) (jump L.B.1)))))
  (define out4
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (< rax 2) (jump L.B.1) (jump L.A.1)))))

  ;; Check handling not not correctly
  (define in5
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (not (not (< rax 2))) (jump L.A.1) (jump L.B.1)))))
  (define out5
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (< rax 2) (jump L.A.1) (jump L.B.1)))))

  ;; Identity function when pred must be evaluated
  (define in6
    '(module
      (define L.A.1 (jump done))
      (define L.B.1 (jump done))
      (define L.start.1 (if (< rax 2) (jump L.A.1) (jump L.B.1)))))
  (define out6 in6)

  ;; Identity function on effect `(set! loc triv)
  (define in7
    '(module
      (define L.A.1 (begin (set! (rbp - 0) 2) (jump done)))))
  (define out7 in7)

  ;; Identity function on effect `(set! loc_1 (+ loc_1 opand))
  (define in8
    '(module
      (define L.A.1
        (begin
          (set! (rbp - 0) 2)
          (set! (rbp - 0) (+ (rbp - 0) 2))
          (jump done)))))
  (define out8 in8)

  ;; Identity function on effect `(set! loc_1 (+ loc_1 opand))
  (define in9
    '(module
      (define L.A.1
        (begin
          (set! (rbp - 0) 2)
          (set! (rbp - 0) (* (rbp - 0) 2))
          (jump done)))))
  (define out9 in9)

  ;; Identity function on tail (jump opand)
  (define in10
    '(module
      (define L.A.1 (begin (set! (rbp - 8) done) (jump (rbp - 8))))
      (define L.B.1 (jump L.A.1))))
  (define out10 in10)

  (check-equal? (resolve-predicates in1) out1)
  (check-equal? (resolve-predicates in2) out2)
  (check-equal? (resolve-predicates in3) out3)
  (check-equal? (resolve-predicates in4) out4)
  (check-equal? (resolve-predicates in5) out5)
  (check-equal? (resolve-predicates in6) out6)
  (check-equal? (resolve-predicates in7) out7)
  (check-equal? (resolve-predicates in8) out8)
  (check-equal? (resolve-predicates in9) out9)
  (check-equal? (resolve-predicates in10) out10)

  ;; Check that the compiled program interprets to 42
  (define-check (check-42 p)
    (check-equal?
      (interp-block-asm-lang-v8 (resolve-predicates p))
      42))

  ;; mref, mset!
  (check-42
    '(module
      (define L.main.1
        (begin
          (mset! r12 8 42)
          (set! rax (mref r12 8))
          (jump done)))))
  )

