#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  cpsc411/langs/v2-reg-alloc)

;; Performs undeadness analysis, decorating the program with undead-set tree.
;; Only the info field of the program is modified.
(define (undead-analysis p)
  (-> asm-lang-v2/locals? asm-lang-v2/undead?)

  ;; Performs undeadness analysis, decorating the program with undead-set tree.
  ;; Only the info field of the program is modified.
  (define (undead-analysis-p p)
    (-> asm-lang-v2/locals? asm-lang-v2/undead?)
    (match p
      [`(module ,info ,tail)
        `(module
          ,(info-set
             info
             'undead-out
             (let-values
               ([(ust _) (undead-analysis-tail tail '())])
               ust))
          ,tail)]))

  ;; Computes the undead-in set for instruction t
  ;; given the undead-out set for t.
  ;;
  ;; tail undead-set? undead-set-tree? -> (values undead-set-tree? undead-set?)
  (define (undead-analysis-tail t uo)
    (-> any/c undead-set? 
        (values undead-set-tree? undead-set?))

    (match t
      [`(halt ,triv)
        (values
          '()
          (undead-analysis-triv triv uo))]
      [`(begin ,effects ... ,tail)
        (define-values (tail-ust tail-ui)
          (undead-analysis-tail tail uo))

        (for/foldr
          ([acc-ust (list tail-ust)]
           [acc-ui tail-ui])
          ([e effects])
          (let-values
            ([(new-ust new-ui)
              (undead-analysis-effect e acc-ui)])
            (values
              (cons new-ust acc-ust)
              new-ui)))]))

  ;; Computes the undead-in set for instruction e
  ;; given the undead-out set for e.
  ;;
  ;; effect undead-set? -> (values undead-set-tree? undead-set?)
  (define (undead-analysis-effect e uo)
    (-> any/c undead-set? undead-set-tree?
        (values undead-set-tree? undead-set?))

    (match e
      [`(set! ,aloc (,_binop ,aloc ,triv))
        ;; 1. define aloc -> set-remove
        ;; 2. reference aloc -> set-add
        ;; 3. reference triv -> undead-analysis-triv
        (values
          uo
          (undead-analysis-triv
            triv
            (set-add uo aloc)))]
      [`(set! ,aloc ,triv)
        (values
          uo
          (undead-analysis-triv
            triv
            (set-remove uo aloc)))]
      ;; NOTE: Removed 'tail' effect
      ;; since we assume valid input
      [`(begin ,effects ...)
        (for/foldr
          ([acc-ust '()]
           [acc-ui uo])
          ([e effects])
          (let-values
            ([(new-ust new-ui)
              (undead-analysis-effect e acc-ui)])
            (values
              (cons new-ust acc-ust)
              new-ui)))]))

  ;; Returns uo with t if t is a triv AND t is in uo,
  ;; otherwise returns uo.
  ;;
  ;; t: triv
  ;; uo: undead-set?
  (define (undead-analysis-triv t uo)
    (-> any/c undead-set? undead-set?)
    (match t
      [(? int64?) uo]
      [(? aloc?) 
       (set-add uo t)]))

  ;; Don't need
  #;
  (define (undead-analysis-binop b uo)
    (match b
      ['* (void)]
      ['+ (void)]))
  (undead-analysis-p p))

(module+ test
  (require rackunit)

  ;; For CPSC411 test suite
  (require
   rackunit/text-ui
   cpsc411/test-suite/public/v2-reg-alloc)

  (define (set-equal? s1 s2)
    (-> set? set? boolean?)
    (empty? (set-symmetric-difference s1 s2)))

  (check-true (set-equal? '(1 2) '(1 2)))
  (check-true (set-equal? '(2 1) '(1 2)))
  (check-false (set-equal? '(2 1 3) '(1 2)))
  (check-false (set-equal? '(2 1) '(1 2 3)))

  ;; Quick helper to check that two undead-set-trees 
  ;; are equal.
  (define (ust-equal? have want)
    (-> undead-set-tree? undead-set-tree? boolean?)
    (match (cons have want)
      [(cons '() '()) #t]
      [(cons '() _) #f]
      [(cons _ '()) #f]
      [(cons s1 s2)
       #:when (aloc? (car s2))
       (set-equal? s1 s2)]
      [(cons s1 s2)
       (andmap ust-equal? s1 s2)]))
  (check-true (ust-equal? '() '()))
  (check-true (ust-equal? '(x.1) '(x.1)))
  (check-true (ust-equal? '((x.1 x.2) (x.1)) '((x.1 x.2) (x.1))))
  (check-true (ust-equal? '((x.2 x.1) (x.1)) '((x.1 x.2) (x.1))))
  (check-true (ust-equal? '((((x.2 x.1) (x.1)))) '((((x.2 x.1) (x.1))))))

  ;; Checks that the ust produced by `undead-analysis` is
  ;; equal to want. Also checks that the rest of the program
  ;; is the same.
  ;;
  ;; locals: (aloc ...)
  ;; tail: tail
  ;; want: the expected locals
  (define (check-ust locals tail want)
    (match (undead-analysis `(module ([locals ,locals]) ,tail))
      [`(module ,info ,tail2)
        ;; Should just contain locals and undead-out
        (check-equal?
          (length info)
          2)
        (check-true
          (ust-equal?
            (info-ref info 'undead-out)
            want)
          "undead-out not equal")
        ;; Should remain unchanged
        (check-equal? tail2 tail)]))

  ;; Simple
  (check-ust
    '()
    '(halt 42)
    '())

  (check-ust
    '()
    '(begin (halt 42))
    '(()))

  ;; Unused set!
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 42)
       (halt 42))
    '(()
      ()))

  ;; Referenced set!
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 42)
       (halt x.1))
    '((x.1)
      ()))

  ;; Referenced binop
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 40)
       (set! x.1 (+ x.1 2))
       (halt x.1))
    '((x.1)
      (x.1)
      ()))

  ;; Unreferenced binop
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 40)
       (set! x.1 (+ x.1 2))
       (halt 42))
    '((x.1)
      ()
      ()))

  ;; binop lhs aloc
  (check-ust
    '(x.1 x.2)
    '(begin
       (set! x.2 2)
       (set! x.1 40)
       (set! x.1 (+ x.1 x.2))
       (halt x.1))
    '((x.2)
      (x.2 x.1)
      (x.1)
      ()))

  ;; nested begin in tail-tail
  (check-ust
    '(x.1 x.2)
    '(begin
       (set! x.1 40)
       (begin
         (set! x.2 2)
         (set! x.1 (+ x.1 x.2))
         (halt x.1)))
    '((x.1)
      ((x.2 x.1)
       (x.1)
       ())))

  ;; nested begin in tail-effect
  (check-ust
    '(x.1 x.2)
    '(begin
       (set! x.1 40)
       (begin
         (set! x.2 2)
         (set! x.1 (+ x.1 x.2)))
       (halt x.1))
    '((x.1)
      ((x.2 x.1)
       (x.1))
      ()))

  ;; from book 1
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 42)
       (halt x.1))
    '((x.1)
      ()))

	;; from book 2
	(check-ust
		'(v.1 w.2 x.3 y.4 z.5 t.6 p.1)
    '(begin
       (set! v.1 1)
       (set! w.2 46)
       (set! x.3 v.1)
       (set! p.1 7)
       (set! x.3 (+ x.3 p.1))
       (set! y.4 x.3)
       (set! p.1 4)
       (set! y.4 (+ y.4 p.1))
       (set! z.5 x.3)
       (set! z.5 (+ z.5 w.2))
       (set! t.6 y.4)
       (set! p.1 -1)
       (set! t.6 (* t.6 p.1))
       (set! z.5 (+ z.5 t.6))
       (halt z.5))

    '((v.1)
      (v.1 w.2)
      (x.3 w.2)
      (p.1 x.3 w.2)
      (x.3 w.2)
      (y.4 x.3 w.2)
      (p.1 y.4 x.3 w.2)
      (x.3 w.2 y.4)
      (w.2 z.5 y.4)
      (y.4 z.5)
      (t.6 z.5)
      (p.1 t.6 z.5)
      (t.6 z.5)
      (z.5)
      ()))

  (run-tests (v2-reg-alloc-public-test-suite undead-analysis))
)
