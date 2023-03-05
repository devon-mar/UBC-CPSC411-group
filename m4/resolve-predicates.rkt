#lang racket

(require cpsc411/compiler-lib)

(provide resolve-predicates)

;; Exercise 9
;; Compiles preds in if statements to equivalent if statements or into
;; equivalent instructions
;; block-pred-lang-v4? -> block-asm-lang-v4?
(define (resolve-predicates p)
  ;; effect -> effect
  (define (convert-effect e)
    (match e
      [`(set! ,loc ,triv) `(set! ,loc ,triv)]
      [`(set! ,loc_1 (,binop ,loc_1 ,opand)) `(set! ,loc_1 (,binop ,loc_1 ,opand))]))

  ;; tail -> tail
  (define (convert-tail t)
    (match t
      [`(halt ,opand) `(halt ,opand)]
      [`(jump ,trg) `(jump ,trg)]
      [`(begin
          ,effects ...
          ,tail)
       `(begin
          ,@(map convert-effect effects)
          ,(convert-tail tail))]
      [`(if ,pred ,t1 ,t2) (convert-if pred t1 t2)]))

  ;; (if pred (jump trg) (jump trg)) -> (jump trg) | (if pred (jump trg) (jump trg))
  (define (convert-if pred t1 t2)
    (match pred
      [`(,relop ,loc ,opand) `(if (,relop ,loc ,opand) ,t1 ,t2)]
      [`(true) t1]
      ['(false) t2]
      [`(not ,nested-pred) (convert-if nested-pred t2 t1)]))

  ;; b -> b
  (define (convert-b b)
    (match b
      [`(define ,label ,tail) `(define ,label ,(convert-tail tail))]))

  (match p
    [`(module ,b ...
        )
     `(module ,@(map convert-b b))]))

(module+ test
  (require rackunit)
  ;; No preds is identity function
  (define in1
    '(module (define start
               (begin
                 (set! rsp 4)
                 (halt rsp)))))
  (define out1 in1)

  ;; Check that if true, replace predicate with the first
  ;; statement
  (define in2
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (true) (jump A) (jump B)))))
  (define out2
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (jump A))))

  ;; Check if false, replace predicate with second statement
  ;; in turnary
  (define in3
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (false) (jump A) (jump B)))))
  (define out3
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (jump B))))

  ;; Check handling not correctly
  (define in4
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (not (< 1 2)) (jump A) (jump B)))))
  (define out4
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (< 1 2) (jump B) (jump A)))))

  ;; Check handling not not correctly
  (define in5
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (not (not (< 1 2))) (jump A) (jump B)))))
  (define out5
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (< 1 2) (jump A) (jump B)))))

  ;; Identity function when pred must be evaluated
  (define in6
    '(module (define A (halt 1)) (define B (halt 2))
       (define start (if (< 1 2) (jump A) (jump B)))))
  (define out6 in6)

  (check-equal? (resolve-predicates in1) out1)
  (check-equal? (resolve-predicates in2) out2)
  (check-equal? (resolve-predicates in3) out3)
  (check-equal? (resolve-predicates in4) out4)
  (check-equal? (resolve-predicates in5) out5)
  (check-equal? (resolve-predicates in6) out6))
