#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide normalize-bind)

;; Milestone 2 Exercise 3
;; Milestone 4 Exercise 19
;; Milestone 5 Exercise 4
;;
;; Compiles Imp-mf-lang v5 to Imp-cmf-lang v5, pushing set! under begin so that
;; the right-hand-side of each set! is simple value-producing operation.
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v5? proc-imp-cmf-lang-v5?)

  ;; Pushes a (set! ,aloc ,value) under a begin so that
  ;; the right-hand-size of each set! is a simple value producing operation.
  ;;
  ;; aloc: imp-mf-lang-v5-aloc?
  ;; value: imp-mf-lang-v5-value
  ;; -> proc-imp-cmf-lang-v5-effect
  (define/contract (normalize-bind-set aloc value)
    (-> aloc? any/c any/c)
    (match value
      [`(begin ,es ... ,value)
        `(begin
           ,@(map normalize-bind-effect es)
           ,(normalize-bind-set aloc value))]
      [`(if ,pred ,vt ,vf)
       `(if ,(normalize-bind-pred pred)
          ,(normalize-bind-set aloc vt)
          ,(normalize-bind-set aloc vf))]
      [_ `(set! ,aloc ,value)]))

  ;; tail: imp-mf-lang-v5-tail?
  ;; -> proc-imp-cmf-lang-v5-proc
  (define/contract (normalize-bind-proc label params tail)
    (-> label? (listof aloc?) any/c any/c)
    `(define
       ,label
       (lambda ,params ,(normalize-bind-tail tail))))

  ;; imp-mf-lang-v5-p -> proc-imp-cmf-lang-v5-p
  (define (normalize-bind-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,tails)) ... ,tail)
        `(module
           ,@(map normalize-bind-proc labels alocs tails)
           ,(normalize-bind-tail tail))]))

  ;; imp-mf-lang-v5-pred -> proc-imp-cmf-lang-v5-pred
  (define (normalize-bind-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(normalize-bind-pred p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map normalize-bind-effect es)
           ,(normalize-bind-pred p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(normalize-bind-pred p1)
           ,(normalize-bind-pred p2)
           ,(normalize-bind-pred p3))]
      [`(,_ ,_ ,_) p]))

  ;; imp-mf-lang-v5-tail -> proc-imp-cmf-lang-v5-tail
  (define (normalize-bind-tail t)
    (match t
      [`(begin ,es ... ,tail)
        `(begin
           ,@(map normalize-bind-effect es)
           ,(normalize-bind-tail tail))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(normalize-bind-pred p)
           ,(normalize-bind-tail t1)
           ,(normalize-bind-tail t2))]
      [`(call ,_ ,_ ...) t]
      ;; value
      [_ (normalize-bind-value t)]))

  ;; imp-mf-lang-v5-value -> proc-imp-cmf-lang-v5-value
  (define (normalize-bind-value v)
    (match v
      [`(begin ,es ... ,v)
        `(begin
           ,@(map normalize-bind-effect es)
           ,(normalize-bind-value v))]
      [`(if ,p ,v1 ,v2)
        `(if
           ,(normalize-bind-pred p)
           ,(normalize-bind-value v1)
           ,(normalize-bind-value v2))]
      [`(,_ ,_ ,_) v]
      ;; triv
      [_ v]))

  ;; imp-mf-lang-v5-effect -> proc-imp-cmf-lang-v5-effect
  (define (normalize-bind-effect e)
    (match e
      [`(set! ,a ,v)
        (normalize-bind-set a v)]
			;; modified template - removed tail effect.
      [`(begin ,es ...)
        `(begin
           ,@(map normalize-bind-effect es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(normalize-bind-pred p)
           ,(normalize-bind-effect e1)
           ,(normalize-bind-effect e2))]))

  ;; not used
  #;
  (define (normalize-bind-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  ;; not used
  #;
  (define (normalize-bind-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
       (void)]))

  ;; not used
  #;
  (define (normalize-bind-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  ;; not used
  #;
  (define (normalize-bind-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

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
	(define-check (check-42 p)
		(check-equal?
			(interp-imp-cmf-lang-v5 (normalize-bind p))
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

  ;; M5 tests
  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1 z.1)
           (begin
             (set! a.1 (if (> x.1 z.1) (if (true) x.1 y.1) z.1))
             (set! b.1 (* y.1 -1))
             (+ a.1 b.1))))
       (call L.foo.1 43 1 42)))

  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1 z.1)
           (begin
             (set!
               a.1
               (begin
                 (set! b.1 (if (false) (* x.1 y.1) (+ x.1 y.1)))
                 (set! b.1 (+ b.1 z.1))
                 (begin
                   (set! c.1 b.1)
                   c.1)))
             a.1)))
       (call L.foo.1 16 10 16)))

  (check-42
    '(module
       (define L.foo.1
         (lambda (a.1 b.1 c.1 d.1)
           (begin
             (set! x.1 (if (> a.1 b.1) c.1 0))
             (set! z.1
               (begin
                 (set! m.1 1)
                 (set! n.1 d.1)
                 (+ m.1 n.1)))
             (begin
               (set! o.1 z.1)
               (* x.1 o.1)))))
       (call L.foo.1 30 20 21 1)))
           
  )

