#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v4
  "../utils/compiler-utils.rkt")

(provide conflict-analysis)

;; Milestone 3 Exercise 2
;; Milestone 4 Exercise 14
;;
;; Create a conflict graph for the Asm-lang program with the undead-set tree
(define/contract (conflict-analysis p)
  (-> asm-pred-lang-v4/undead? asm-pred-lang-v4/conflicts?)

  (define conflict-graph (new-graph))

  ;; Updates conflict graph for the alocs in the tail
  ;; tail undead-set-tree -> void
  (define (conflict-tail tail ust)
    (match (cons tail ust)
      [(cons `(halt ,_) _) (void)]
      [(cons `(begin ,effects ... ,tail) `(,usts ...))
       (for ([e effects] [u usts])
         (conflict-effect e u))
       (conflict-tail tail (last ust))]
      [(cons `(if ,pred ,tail1 ,tail2) `(,ustp ,ust1 ,ust2))
       (conflict-pred pred ustp)
       (conflict-tail tail1 ust1)
       (conflict-tail tail2 ust2)]))
  
  ;; Updates conflict graph for the alocs in the effect
  ;; effect undead-set-tree -> void
  (define (conflict-effect effect ust)
    (match (cons effect ust)
      [(cons `(set! ,aloc (,_ ,aloc ,_)) undead-out)
       (for ([u undead-out])
         (unless
          (equal? aloc u)
          (set! conflict-graph (add-edge conflict-graph aloc u))))]
      [(cons `(set! ,aloc ,triv) undead-out)
       (for ([u undead-out])
         (unless
          (or (equal? aloc u) (and (aloc? triv) (equal? triv u)))
          (set! conflict-graph (add-edge conflict-graph aloc u))))]
      [(cons `(begin ,effects ...) `(,usts ...))
       (for ([e effects] [u usts])
         (conflict-effect e u))]
      [(cons `(if ,pred ,effect1 ,effect2) `(,ustp ,ust1 ,ust2))
       (conflict-pred pred ustp)
       (conflict-effect effect1 ust1)
       (conflict-effect effect2 ust2)]))

  ;; Updates conflict graph for the alocs in the pred
  ;; pred undead-set-tree -> void
  (define (conflict-pred pred ust)
    (match (cons pred ust)
      [(cons `(true) _)
       (void)]
      [(cons `(false) _)
       (void)]
      [(cons `(not ,ipred) iust)
       (conflict-pred ipred iust)]
      [(cons `(begin ,effect ... ,tpred) `(,usts ...))
       (for ([e effect] [u usts])
         (conflict-effect e u))
       (conflict-pred tpred (last usts))]
      [(cons `(if ,ppred ,pred1 ,pred2) `(,ustp ,ust1 ,ust2))
       (conflict-pred ppred ustp)
       (conflict-pred pred1 ust1)
       (conflict-pred pred2 ust2)]
      [(cons `(,relop ,_ ,_) _)
       #:when(relop? relop)
       (void)]))

  (match p
    [`(module ,info ,tail)
     (set! conflict-graph (new-graph (info-ref info 'locals)))
     (conflict-tail tail (info-ref info 'undead-out))
     (define newinfo
       (info-remove (info-set info 'conflicts conflict-graph) 'undead-out))
     `(module ,newinfo ,tail)]))

;; graph graph -> boolean
;; Return true if graph associates each vertex to the same set of vertices
(define (graph-equals? g1 g2)
  (define keys1 (sort (sequence->list (in-dict-keys g1)) symbol<?))
  (define keys2 (sort (sequence->list (in-dict-keys g2)) symbol<?))
  (and (equal? (length g1) (length g2))
       (equal? keys1 keys2)
       (for/and ([k keys1])
         (define s1 (get-neighbors g1 k))
         (define s2 (get-neighbors g2 k))
         (and
           (equal? (length s1) (length s2))
           (equal? (list->set s1) (list->set s2))))))

(module+ test
  (require rackunit)

  ;; graph-equals? tests
  (check-true (graph-equals? '() '()))
  (check-false (graph-equals? '() '((x.1 (x.2)))))
  (check-true (graph-equals? '((x.1 (x.2))) '((x.1 (x.2)))))
  ; check key and value flipped
  (check-false (graph-equals? '((x.1 (x.2))) '((x.2 (x.1)))))
  ; check duplicate in values
  (check-false (graph-equals? '((x.1 (x.2 x.2))) '((x.2 (x.1)))))
  ; check duplicate associations
  (check-false (graph-equals? '((x.1 (x.2)) (x.1 (x.2))) '((x.1 (x.2)))))
  ; check out of order
  (check-true (graph-equals?
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)))
    '((x.2 (x.3 x.1)) (x.3 (x.4)) (x.1 (x.3 x.2)))))
  ; check extra vertex
  (check-false (graph-equals?
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)))
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)) (x.4 ()))))
  ; check extra edge
  (check-false (graph-equals?
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4 x.2)) (x.4 (x.3)))
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)) (x.4 (x.3)))))

  ;; Check that the graphs are equal
  ;; any any -> void
  (define-binary-check (check-graph? actual expected)
    (graph-equals? actual expected))

  ;; Check that a program w/ undead-out compiles into a program w/ conflicts
  ;; by compiling with conflict-analysis
  ;; asm-pred-lang-v4/locals undead-set-tree conflicts -> void
  (define-check (check-conflict program undead-out conflicts)
    (define-values (compiled etail)
      (match program
        [`(module ,info ,etail)
          (values
            (conflict-analysis
              `(module ,(info-set info 'undead-out undead-out) ,etail))
            etail)]))
    (match compiled
      [`(module ,info ,atail)
       (check-equal? etail atail)
       (check-graph? (info-ref info 'conflicts) conflicts)]))
  
  ;; conflict-analysis tests
  (check-conflict
    '(module
        ((locals ()))
        (begin (halt 0)))
    '(())
    '())

  ; No conflicts
  (check-conflict
    '(module
        ((locals (x.1 x.2)))
        (begin (set! x.2 12) (set! x.1 x.2) (halt x.1)))
    '((x.2) (x.1) ())
    '((x.1 ()) (x.2 ())))

  ; One-to-one conflict
  (check-conflict
    '(module
        ((locals (x.1 x.2)))
        (begin
          (set! x.1 5)
          (begin
            (set! x.2 10)
            (set! x.1 (+ x.1 x.2)))
          (halt 0)))
    '((x.1)
      ((x.1 x.2)
       ())
      ())
    '((x.1 (x.2)) (x.2 (x.1))))

  ; Move optimization
  (check-conflict
    '(module
        ((locals (x.1 x.2 x.3)))
        (begin
          (set! x.1 5)
          (set! x.2 x.1)
          (set! x.3 x.1)
          (halt x.2)))
    '((x.1)
      (x.1 x.2)
      (x.2)
      ())
    '((x.1 ()) (x.2 (x.3)) (x.3 (x.2))))
  
  ; No move optimization for binop
  (check-conflict
    '(module
        ((locals (x.1 x.2 x.3)))
        (begin
          (set! x.1 5)
          (set! x.2 x.1)
          (set! x.2 (+ x.2 x.1))
          (set! x.3 x.1)
          (halt x.2)))
    '((x.1)
      (x.1 x.2)
      (x.1 x.2)
      (x.2)
      ())
    '((x.1 (x.2)) (x.2 (x.1 x.3)) (x.3 (x.2))))

  ; Many alocs + conflicts
  (check-conflict
    '(module
      ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1 a.10)))
      (begin
        (set! v.1 1)
        (set! w.2 46)
        (begin (set! x.3 v.1))
        (set! p.1 7)
        (set! x.3 (+ x.3 p.1))
        (set! y.4 x.3)
        (begin
          (set! p.1 4)
          (begin
            (set! y.4 (+ y.4 p.1))
            (set! z.5 x.3)
            (set! z.5 (+ z.5 w.2))))
        (set! t.6 y.4)
        (set! p.1 -1)
        (set! a.10 10)
        (set! a.10 (* a.10 10))
        (begin
          (set! t.6 (* t.6 p.1))
          (set! z.5 (+ z.5 t.6))
          (begin (halt z.5)))))
    '((v.1)
      (v.1 w.2)
      ((w.2 x.3))
      (p.1 w.2 x.3)
      (w.2 x.3)
      (y.4 w.2 x.3)
      ((p.1 y.4 w.2 x.3)
       ((y.4 w.2 x.3)
        (z.5 y.4 w.2)
        (z.5 y.4)))
      (t.6 z.5)
      (t.6 z.5 p.1)
      (t.6 z.5 p.1 a.10)
      (t.6 z.5 p.1)
      ((t.6 z.5)
       (z.5)
       (())))
    '((v.1 (w.2))
      (w.2 (z.5 y.4 p.1 x.3 v.1))
      (x.3 (y.4 p.1 w.2))
      (y.4 (z.5 x.3 p.1 w.2))
      (z.5 (p.1 t.6 w.2 y.4 a.10))
      (t.6 (p.1 z.5 a.10))
      (p.1 (z.5 t.6 y.4 x.3 w.2 a.10))
      (a.10 (t.6 z.5 p.1))))

  ; if in tail
  (check-conflict
    '(module
      ((locals (x.1 x.2 x.3)))
      (begin
        (set! x.1 5)
        (if (= x.1 2)
          (begin (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.1))
          (begin (set! x.3 9) (set! x.1 x.3) (halt x.1)))))
    '((x.1)
      ((x.1)
       ((x.1 x.2) (x.1) ())
       ((x.3) (x.1) ())))
    '((x.1 (x.2)) (x.2 (x.1)) (x.3 ())))
  
   ; if in effect
  (check-conflict
    '(module
      ((locals (x.1 x.2 x.3)))
      (begin
        (set! x.1 5)
        (if (not (>= x.1 2))
          (begin (set! x.3 9) (set! x.1 x.3))
          (begin (set! x.2 10) (set! x.1 (* x.1 x.2))))
        (halt x.1)))
    '((x.1)
      ((x.1)
       ((x.3) (x.1))
       ((x.1 x.2) (x.1)))
      ())
    '((x.1 (x.2)) (x.2 (x.1)) (x.3 ())))
  
  ;; if in pred
  (check-conflict
    '(module
      ((locals (x.1 x.2 x.3 x.4 x.5 x.6 x.7)))
      (begin
        (set! x.6 0)
        (set! x.1 5)
        (set! x.2 8)
        (set! x.4 x.2)
        (if
          (not
            (begin
              (set! x.4 (+ x.4 9))
              (if (= x.2 x.4)
                  (begin
                    (set! x.3 9)
                    (set! x.1 x.3)
                    (set! x.5 9)
                    (= x.1 x.5))
                  (begin
                    (set! x.3 10)
                    (set! x.4 (* x.4 9))
                    (set! x.7 x.3)
                    (set! x.1 (* x.1 x.3))
                    (not (< x.4 x.7))))))
          (halt x.2)
          (halt x.3))))
    '(()
      (x.1)
      (x.1 x.2)
      (x.4 x.1 x.2)
      (((x.4 x.1 x.2)
        ((x.4 x.1 x.2)
         ((x.3 x.2)
          (x.1 x.3 x.2)
          (x.5 x.1 x.3 x.2)
          (x.3 x.2))
         ((x.1 x.4 x.3 x.2)
          (x.1 x.4 x.3 x.2)
          (x.1 x.7 x.4 x.3 x.2)
          (x.7 x.4 x.3 x.2)
          (x.3 x.2))))
       ()
       ()))
    '((x.1 (x.2 x.3 x.4 x.5 x.7))
      (x.2 (x.1 x.3 x.4 x.5 x.7))
      (x.3 (x.1 x.2 x.4 x.5))
      (x.4 (x.1 x.2 x.3 x.7))
      (x.5 (x.1 x.2 x.3))
      (x.6 ())
      (x.7 (x.1 x.2 x.4))))
  )