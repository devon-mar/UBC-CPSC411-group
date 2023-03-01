#lang racket

(require
  cpsc411/langs/v3)

(provide sequentialize-let)

;; Milestone 2 Exercise 2
;;
;; Compiles Values-unique-lang v3 to Imp-mf-lang v3 by picking a particular
;; order to implement let expressions using set!.
;;
;; p: values-unique-lang-v3 -> imp-mf-lang-v3
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v3? imp-mf-lang-v3?)

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))

  ;; map an aloc and value pair to a set!
  ;;
  ;; a: aloc
  ;; v: value
  ;; -> set statement
  (define (aloc-value->set a v)
    `(set! ,a ,(sequentialize-let-value v)))
  
  (define (sequentialize-let-tail t)
    (match t
      [`(let ([,alocs ,vs] ...) ,tail)
        `(begin
          ,@(map aloc-value->set alocs vs)
          ,(sequentialize-let-tail tail))]
      [v (sequentialize-let-value v)]))

  ;; values-unique-lang-v3-value -> imp-mf-lang-v3-value
  (define (sequentialize-let-value v)
    (match v
      [`(,binop ,_ ,_)
        #:when (binop? binop)
        v]
      [`(let ([,alocs ,vs] ...) ,value)
        `(begin
          ,@(map aloc-value->set alocs vs)
          ,(sequentialize-let-value value))]
      [triv triv]))

  ;; values-unique-lang-v3-p -> imp-mf-lang-v3-p
  (define (sequentialize-let-p p)
    (match p
      [`(module ,tail) `(module ,(sequentialize-let-tail tail))]))

  (sequentialize-let-p p))

(module+ test
  (require rackunit)

  (define (check-42 p)
    (check-equal?
      (interp-values-unique-lang-v3 (sequentialize-let p))
      42))

  ;; very simple
  (check-42 '(module 42))

  ;; simple
  (check-42 '(module (let ([x.3 42]) x.3)))

  ;; with binop
  (check-42 '(module (let ([x.4 (+ 40 2)]) x.4)))

  ;; let in tail of let
  (check-42 '(module (let ([x.7 2]) (let ([x.8 21]) (+ x.8 x.8)))))

  ;; nested let
  ;; bug??
  ;; should x.9 be accessible?
  ;; (interp '(module (let ([x.7 5] [x.8 (let ([x.9 40]) (+ x.9 2))]) x.9)))
  (check-42 '(module (let ([x.7 5] [x.8 (let ([x.9 40]) (+ x.9 2))]) x.8)))

  )
