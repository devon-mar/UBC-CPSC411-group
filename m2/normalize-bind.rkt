#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v3)

(provide normalize-bind)

;; Milestone 2 Exercise 3
;;
;; Compiles Imp-mf-lang v3 to Imp-cmf-lang v3, pushing set! under begin so that
;; the right-hand-side of each set! is simple value-producing operation.
;; This normalizes Imp-mf-lang v3 with respect to the equations
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v3? imp-cmf-lang-v3?)

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))

  ;; Replace any set! statement with a begin
  ;; containing the original set at the end if value is a begin.
  ;;
  ;; aloc: aloc?
  ;; value: imp-mf-lang-v3-value
  ;; -> imp-cmf-lang-v3-effect
  (define/contract (normalize-set aloc value)
    (-> aloc? any/c any/c)
    (match value
      [`(begin ,es ... ,value)
        `(begin
           ,@(map normalize-bind-effect es)
           (set! ,aloc ,value))]
      [_ `(set! ,aloc ,value)]))


  ;; imp-mf-lang-v3-effect -> imp-cmf-lang-v3-effect
  (define (normalize-bind-effect e)
    (match e
      [`(set! ,aloc ,value) (normalize-set aloc value)]
      [`(begin ,es ..., e)
        `(begin ,@(map normalize-bind-effect es) ,(normalize-bind-effect e))]))

  ;; imp-mf-lang-v3-tail -> imp-cmf-lang-v3-tail
  (define (normalize-bind-tail t)
    (match t
      [`(begin ,es ... ,t) `(begin ,@(map normalize-bind-effect es) ,(normalize-bind-tail t))]
      [v (normalize-bind-value v)]))

  ;; imp-mf-lang-v3-value -> imp-cmf-lang-v3-tail
  (define (normalize-bind-value v)
    (match v
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        `(,binop, t1, t2)]
      [`(begin ,es ... ,v) `(begin ,@(map normalize-bind-effect es) ,(normalize-bind-value v))]
      [triv triv]))

  ;; imp-mf-lang-v3-p -> imp-cmf-lang-v3-p
  (define (normalize-bind-p p)
    (match p
      [`(module ,tail)
        `(module ,(normalize-bind-tail tail))]))

  (normalize-bind-p p))


(module+ test
  (require rackunit)

  ; very simple
  (check-equal? (normalize-bind '(module 42)) '(module 42))

  ; begin with one effect under a set
  (check-equal?
    (normalize-bind
      '(module
         (begin
           (set! x.7 5)
           (set! x.8
             (begin
               (set! x.9 40)
               (+ x.9 2)))
           x.9)))
    '(module
      (begin
        (set! x.7 5)
        (begin
          (set! x.9 40)
          (set! x.8 (+ x.9 2)))
        x.9)))

  ; begin with multiple effects under a set
  (check-equal?
    (normalize-bind
      '(module
         (begin
           (set! x.7 5)
           (set! x.8
             (begin
               (set! x.9 40)
               (set! x.4 10)
               (+ x.9 2)))
           x.9)))
    '(module
      (begin
        (set! x.7 5)
        (begin
          (set! x.9 40)
          (set! x.4 10)
          (set! x.8 (+ x.9 2)))
        x.9)))

  (check-equal?
    (normalize-bind
      '(module
         (begin
           (set! x.3
             (begin
               (set! y.4
                 (begin
                   (set! z.4 (+ 40 2))
                   z.4))
               y.4))
           x.3)))
    '(module
      (begin
        (begin
          (begin
            (set! z.4 (+ 40 2))
            (set! y.4 z.4))
          (set! x.3 y.4))
        x.3))))

