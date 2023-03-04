#lang racket

(require
  cpsc411/langs/v4)

(provide sequentialize-let)

;; Milestone 2 Exercise 2
;; Milestone 4 Exercise 18
;;
;; Compiles Values-unique-lang v4 to Imp-mf-lang v4 by picking a particular
;; order to implement let expressions using set!.
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v4? imp-mf-lang-v4?)

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

  (define (sequentialize-let-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,pred)
        `(not ,(sequentialize-let-pred pred))]
      [`(let ([,xs ,vs] ...) ,pred)
        `(begin
           ,@(map aloc-value->set xs vs)
           ,(sequentialize-let-pred pred))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(sequentialize-let-pred p1)
           ,(sequentialize-let-pred p2)
           ,(sequentialize-let-pred p3))]
      [`(,relop ,t1 ,t2)
        `(,relop
           ,(sequentialize-let-tail t1)
           ,(sequentialize-let-tail t2))]))
  
  (define (sequentialize-let-tail t)
    (match t
      [`(if ,pred ,t1 ,t2)
       `(if 
          ,(sequentialize-let-pred pred)
          ,(sequentialize-let-tail t1)
          ,(sequentialize-let-tail t2))]
      [`(let ([,alocs ,vs] ...) ,tail)
        `(begin
          ,@(map aloc-value->set alocs vs)
          ,(sequentialize-let-tail tail))]
      [v (sequentialize-let-value v)]))

  ;; values-unique-lang-v4-value -> imp-mf-lang-v4-value
  (define (sequentialize-let-value v)
    (match v
      [`(,binop ,_ ,_)
        #:when (binop? binop)
        v]
      [`(let ([,alocs ,vs] ...) ,value)
        `(begin
          ,@(map aloc-value->set alocs vs)
          ,(sequentialize-let-value value))]
      [`(if ,pred ,v1 ,v2)
        `(if
           ,(sequentialize-let-pred pred)
           ,(sequentialize-let-value v1)
           ,(sequentialize-let-value v2))]
      [triv triv]))

  ;; not used
  #;
  (define (sequentialize-let-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  ;; values-unique-lang-v4-p -> imp-mf-lang-v4-p
  (define (sequentialize-let-p p)
    (match p
      [`(module ,tail) `(module ,(sequentialize-let-tail tail))]))

  (sequentialize-let-p p))

(module+ test
  (require rackunit)

  (define (check-42 p)
    (check-equal?
      (interp-values-unique-lang-v4 (sequentialize-let p))
      42))

  ;; new m3 stuff
  (check-42
    '(module
       (let ([x.1 1])
         (if (not (let ([x.2 (if (true) 2 1)]) (= x.2 2)))
           0
           (if (if (true) (false) (> 10 1))
             0
             42)))))


  ;; M2 tests
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
