#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide normalize-bind)

;; Milestone 2 Exercise 3
;; Milestone 4 Exercise 19
;;
;; Compiles Imp-mf-lang v4 to Imp-cmf-lang v4, pushing set! under begin so that
;; the right-hand-side of each set! is simple value-producing operation.
;; This normalizes Imp-mf-lang v3 with respect to the equations
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v4? imp-cmf-lang-v4?)

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and 
      (member b '(* +))
      #t))

  ;; Replace any set! statement with a begin
  ;; containing the original set at the end if value is a begin.
  ;; Also pushes any set! under an if if value is an if.
  ;;
  ;; aloc: aloc?
  ;; value: imp-mf-lang-v4-value
  ;; -> imp-cmf-lang-v4-effect
  (define/contract (normalize-set aloc value)
    (-> aloc? any/c any/c)
    (match value
      [`(begin ,es ... ,value)
        `(begin
           ,@(map normalize-bind-effect es)
           ,(normalize-set aloc value))]
      [`(if ,pred ,vt ,vf)
       `(if ,(normalize-bind-pred pred)
          ,(normalize-set aloc vt)
          ,(normalize-set aloc vf))]
      [_ `(set! ,aloc ,value)]))


  ;; imp-mf-lang-v4-effect -> imp-cmf-lang-v4-effect
  (define (normalize-bind-effect e)
    (match e
      [`(set! ,aloc ,value) (normalize-set aloc value)]
      [`(if ,pred ,e1 ,e2)
        `(if
           ,(normalize-bind-pred pred)
           ,(normalize-bind-effect e1)
           ,(normalize-bind-effect e2))]
      [`(begin ,es ..., e)
        `(begin ,@(map normalize-bind-effect es) ,(normalize-bind-effect e))]))

  ;; imp-mf-lang-v4-tail -> imp-cmf-lang-v4-tail
  (define (normalize-bind-tail t)
    (match t
      [`(begin ,es ... ,t) `(begin ,@(map normalize-bind-effect es) ,(normalize-bind-tail t))]
      [`(if ,pred ,t1 ,t2)
        `(if
           ,(normalize-bind-pred pred)
           ,(normalize-bind-tail t1)
           ,(normalize-bind-tail t2))]
      [v (normalize-bind-value v)]))

  ;; imp-mf-lang-v4-value -> imp-cmf-lang-v4-tail
  (define (normalize-bind-value v)
    (match v
      [`(,binop ,t1 ,t2)
        #:when (binop? binop)
        `(,binop, t1, t2)]
      [`(begin ,es ... ,v) `(begin ,@(map normalize-bind-effect es) ,(normalize-bind-value v))]
      [triv triv]))

  ;; imp-mf-lang-v4-p -> imp-cmf-lang-v4-p
  (define (normalize-bind-p p)
    (match p
      [`(module ,tail)
        `(module ,(normalize-bind-tail tail))]))

  (define (normalize-bind-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(normalize-bind-pred p))]
      [`(begin ,effects ... ,pred)
        `(begin
           ,@(map normalize-bind-effect effects)
           ,(normalize-bind-pred pred))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(normalize-bind-pred p1)
           ,(normalize-bind-pred p2)
           ,(normalize-bind-pred p3))]
      [`(,_ ,_ ,_)
        p]))

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

  ;; simple (set aloc if (...))
  (check-equal?
    (normalize-bind
      '(module
         (begin
           (set! x.1
             (if (true)
               42
               0))
           42)))
    '(module
       (begin
         (if (true)
           (set! x.1 42)
           (set! x.1 0))
         42)))

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
        x.3)))

  ;; M3 tests
	(define (check-42 p)
		(check-equal?
			(interp-imp-cmf-lang-v4 (normalize-bind p))
			42))

  (check-42
	  ;; tail/value
	  ;; value/triv
    '(module 42))

	(check-42
		'(module
	     ;; pred/(relop triv triv)
	     ;; pred/(true)
       (if (true)
	       ;; pred/(not pred)
         (if (not (true)) 0 42)
         ;; pred/(false)
         (if (false) 1 0))))

  (check-42
    '(module
	     ;; value/(if pred value value)
       (if (begin (set! x.1 (if (true) 20 0)) (= x.1 20))
         42
         0)))

  (check-42
    '(module
       ;; pred/(if pred pred pred)
       (if (if (true) (false) (true))
         0
         42)))

  (check-42
    '(module
	     ;; tail/(begin effect ... tail)
       (begin
         (set! x.1 1)
         (set! x.2 41)
	       ;; tail/(if pred tail tail)
	       ;; value/(binop triv triv)
         (if (false) 40 (+ x.2 x.1)))))

  (check-42
    '(module
       (if (true)
         ;; TODO: is this one even possible? (it's not the one below)...
         ;; value/(begin effect ... value)
         (begin
           (set! x.1 (if (true) (if (true) 40 3) 2))
           (set! x.2 (begin (set! x.3 (begin (set! x.4 1) x.4)) (+ x.3 1)))
           (+ x.1 x.2))
         0)))

  (check-42
    '(module
       (begin
	       ;; effect/(begin effect ... effect)
         (begin
           (set!
             x.1
             (begin
               ;; effect/(set! aloc value)
               (set! x.2 10)
               (set! x.3 20)
               (+ x.2 x.3)))
	         ;; effect/(if pred effect effect)
           (if (true) (set! x.4 12) (set! x.4 40)))
         (+ x.1 x.4))))
  )
