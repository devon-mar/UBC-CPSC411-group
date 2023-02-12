#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib)

;; M3 Exercise 2
;; Asm-lang-v2/undead -> Asm-lang-v2/conflicts
;; Create a conflict graph for the Asm-lang program with the undead-set tree
(define (conflict-analysis p)
  ;; (Asm-lang-v2/undead tail) undead-set-tree graph -> graph
  ;; Returns the updated conflict graph for the alocs in the tail
  (define (conflict-tail tail ust graph)
    (match (cons tail ust)
      [(cons `(halt ,_) _) graph]
      [(cons `(begin ,effects ... ,tail) `(,usts ...))
       (define g (for/fold ([g graph]) ([u usts] [e effects])
         (conflict-effect e u g)))
       (conflict-tail tail (last ust) g)]))
  
  ;; (Asm-lang-v2/undead effect) undead-set-tree graph -> graph
  ;; Returns the updated conflict graph for the alocs in the effect
  (define (conflict-effect effect ust graph)
    (match (cons effect ust)
      [(cons `(set! ,aloc ,_) undead-out)
       ;; for both:
       ;; (set! aloc triv)
       ;; (set! aloc_1 (binop aloc_1 triv))
       (for/fold ([g graph]) ([u undead-out])
         (if (equal? aloc u)
             g
             (add-edge g aloc u)))]
      [(cons `(begin ,effects ...) `(,usts ...))
       (for/fold ([g graph]) ([u usts] [e effects])
         (conflict-effect e u g))]))

  (match p
    [`(module ,info ,tail)
     (define init-graph (new-graph (info-ref info 'locals)))
     (define conflict-graph
       (conflict-tail tail (info-ref info 'undead-out) init-graph))
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
  
  ;; conflict-analysis tests
  (check-equal?
    (conflict-analysis
      '(module
        ((locals ()) (undead-out (())))
        (begin (halt 0))))
    '(module
        ((locals ()) (conflicts ()))
        (begin (halt 0))))

  ; No conflicts
  (check-match
    (conflict-analysis
      '(module
        ((locals (x.1 x.2)) (undead-out ((x.2) (x.1) ())))
        (begin (set! x.2 12) (set! x.1 x.2) (halt x.1))))
    `(module
        ((locals (x.1 x.2)) (conflicts ,conflicts))
        (begin (set! x.2 12) (set! x.1 x.2) (halt x.1)))
    (graph-equals? conflicts '((x.1 ()) (x.2 ()))))

  ; One-to-one conflict
  (check-match
    (conflict-analysis
      '(module
        ((locals (x.1 x.2))
         (undead-out
          ((x.1)
           ((x.1 x.2)
            ())
           ())))
        (begin
          (set! x.1 5)
          (begin
            (set! x.2 10)
            (set! x.1 (+ x.1 x.2)))
          (halt 0))))
    `(module
      ((locals (x.1 x.2)) (conflicts ,conflicts))
      (begin
        (set! x.1 5)
        (begin
          (set! x.2 10)
          (set! x.1 (+ x.1 x.2)))
        (halt 0)))
    (graph-equals? conflicts '((x.1 (x.2)) (x.2 (x.1)))))

  ; Many alocs + conflicts
  (check-match
    (conflict-analysis
      '(module
        ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1 a.10))
         (undead-out
           ((v.1)
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
             (())))))
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
            (begin (halt z.5))))))
    `(module
      ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1 a.10))
       (conflicts ,conflicts))
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
    (graph-equals?
      conflicts
      '((v.1 (w.2))
        (w.2 (z.5 y.4 p.1 x.3 v.1))
        (x.3 (y.4 p.1 w.2))
        (y.4 (z.5 x.3 p.1 w.2))
        (z.5 (p.1 t.6 w.2 y.4 a.10))
        (t.6 (p.1 z.5 a.10))
        (p.1 (z.5 t.6 y.4 x.3 w.2 a.10))
        (a.10 (t.6 z.5 p.1)))))

  )
  