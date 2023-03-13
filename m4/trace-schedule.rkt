#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v4)

(provide trace-schedule)

;; Optimize by rearranging blocks in Block-Asm-Lang to take
;; advantage of fall-through between blocks
(define/contract (trace-schedule p)
  (-> block-asm-lang-v4? block-asm-lang-v4?)

  ;; graph of tail jumps to another label
  ;; edge from u to v, if jump from label u to label v
  (define jump-graph (new-graph))

  ;; graph -> graph
  ;; Return a graph with each of the directed edges flipped
  (define (flip-graph graph)
    (define vertices (map car graph))
    (for/fold ([g (new-graph vertices)]) ([v vertices])
      (for/fold ([g g]) ([nb (get-neighbors graph v)])
        (add-directed-edge g nb v))))

  ;; tail -> void
  ;; Update the jump graph with jump information in tail
  (define (update-graph-tail t label)
    (match t
      [`(halt ,_) (void)]
      [`(jump ,trg)
       (when (label? trg) (set! jump-graph (add-directed-edge jump-graph label trg)))
       (void)]
      [`(begin ,_ ... ,tail)
       (update-graph-tail tail label)]
      [`(if (,_ ,_ ,_) (jump ,_) (jump ,trg2))
       ; Only consider second jump since it is at the end
       (when (label? trg2) (set! jump-graph (add-directed-edge jump-graph label trg2)))
       (void)]))

  ;; b -> void
  ;; Update the jump-graph with jump information in b
  (define (update-graph-b b)
    (match b
      [`(define ,label ,tail)
       (update-graph-tail tail label)]))
  
  ;; b -> label
  ;; Get the label from b
  (define (get-label-b b)
    (match b
      [`(define ,label ,_) label]))
  
  ;; label (List-of label) -> (List-of label)
  ;; Generate a label ordering to encourage fall through
  ;; Algorithm:
  ;; - Pick the first label as start label
  ;; - To pick the next label
  ;;  - If the last picked label has a neighbour (note: it should only have one),
  ;;    pick the neighbour as the next label
  ;;  - Otherwise, pick a remaining label with the least amount of incoming edges
  (define (get-order first-label rest-labels)
    (define flipped (flip-graph jump-graph))
    (for/fold ([order (list first-label)]) ([_ rest-labels])
      (define prevlabel (last order))
      (define nbs (get-neighbors jump-graph prevlabel))
      (set! jump-graph (remove-vertex jump-graph prevlabel))
      (set! flipped (remove-vertex flipped prevlabel))
      (define pick
        (if (empty? nbs)
            (argmin
              (lambda (x) (length (get-neighbors flipped x)))
              (map car jump-graph))
            (first nbs)))
      (append order (list pick))))

  (match p
    ;; For (module b ... b)
    [`(module ,bs ...)
     (define-values (labels label-dict)
       (for/fold ([labels '()]
                  [ldict '()])
                 ([b bs])
         (define l (get-label-b b))
         (values
           (append labels (list l))
           (dict-set ldict l b))))
     (if (equal? (length labels) (dict-count label-dict))
         (let ()
           (set! jump-graph (new-graph labels))
           (for ([b bs]) (update-graph-b b))
           (unless (dict-has-key? label-dict 'done)
                   (set! jump-graph (remove-vertex jump-graph 'done)))
           (define order (get-order (first labels) (rest labels)))
           (define new-bs
             (for/list ([l order])
               (dict-ref label-dict l)))
           `(module ,@new-bs))
         p)]))

(module+ test
  (require rackunit)

  (check-equal?
    (trace-schedule '(module (define L.test.1 (halt 0))))
    '(module (define L.test.1 (halt 0))))
  
  ;; duplicate label - invalid, do not change
  (check-equal?
    (trace-schedule '(module (define L.test.1 (jump L.test.1)) (define L.test.1 (halt 0))))
    '(module (define L.test.1 (jump L.test.1)) (define L.test.1 (halt 0))))
  
  ;; reorder - jump to done
  (check-equal?
    (trace-schedule
      '(module
        (define L.test.1 (jump L.test.3))
        (define L.test.2 (begin (set! rax 10) (jump done)))
        (define L.test.3 (jump L.test.2))))
    '(module
        (define L.test.1 (jump L.test.3))
        (define L.test.3 (jump L.test.2))
        (define L.test.2 (begin (set! rax 10) (jump done)))))
  
  ;; define done
  (check-equal?
    (trace-schedule
      '(module
        (define L.test.1 (jump L.test.3))
        (define done (begin (set! rax 10) (halt 0)))
        (define L.test.3 (jump done))))
    '(module
        (define L.test.1 (jump L.test.3))
        (define L.test.3 (jump done))
        (define done (begin (set! rax 10) (halt 0)))))
  
  ;; no reordering for first label
  (check-equal?
    (trace-schedule
      '(module
        (define L.test.1 (halt 0))
        (define L.test.2 (jump L.test.1))))
    '(module
      (define L.test.1 (halt 0))
      (define L.test.2 (jump L.test.1))))

  ;; reorder for one path of jumps
  (check-equal?
    (trace-schedule
      '(module
        (define L.test.1 (jump L.test.4))
        (define L.test.2 (jump L.test.5))
        (define L.test.3 (halt 0))
        (define L.test.4 (jump L.test.2))
        (define L.test.5 (jump L.test.3))))
    '(module
      (define L.test.1 (jump L.test.4))
      (define L.test.4 (jump L.test.2))
      (define L.test.2 (jump L.test.5))
      (define L.test.5 (jump L.test.3))
      (define L.test.3 (halt 0))))
  
  ;; (List-of b) (List-of (List-of b)) -> boolean
  ;; Return true iff bs has the same ordering as groups
  ;; while allowing ordering of each group to be shuffled around
  ;; (but not allowing ordering of each b in a group to be shuffled)
  (define (check-b-order bs groups)
    (define m '())
    (for/and ([b bs])
      (when
        (empty? m)
        (set! m (findf (lambda (x) (equal? b (first x))) groups)))
      (begin0
        (and (not (empty? m)) (equal? (first m) b))
        (set! m (rest m)))))

  ;; reorder for disjointed paths
  (check-match
    (trace-schedule
      '(module
        (define L.test.1 (begin (set! fv0 10) (jump L.test.4)))
        (define L.test.2 (if (< fv0 9) (jump L.test.11) (jump L.test.5)))
        (define L.test.3 (halt 0))
        (define L.place.1 (halt 4)) 
        (define L.test.4 (jump L.test.2))
        (define L.test.5 (if (= fv0 9) (jump L.test.8) (jump L.test.3)))
        (define L.test.6 (begin (set! fv0 (* fv0 9)) (jump L.test.7)))
        (define L.test.7 (halt fv0))
        (define L.test.8 (jump L.test.6))
        (define L.test.9 (jump L.test.10))
        (define L.test.10 (halt 10))
        (define L.test.11 (jump L.test.9))))
    `(module
      (define L.test.1 (begin (set! fv0 10) (jump L.test.4)))
      (define L.test.4 (jump L.test.2))
      (define L.test.2 (if (< fv0 9) (jump L.test.11) (jump L.test.5)))
      (define L.test.5 (if (= fv0 9) (jump L.test.8) (jump L.test.3)))
      (define L.test.3 (halt 0))
      ,restb ...)
    (check-b-order
      restb
      `(((define L.test.8 (jump L.test.6))
         (define L.test.6 (begin (set! fv0 (* fv0 9)) (jump L.test.7)))
         (define L.test.7 (halt fv0)))
        ((define L.test.11 (jump L.test.9))
         (define L.test.9 (jump L.test.10))
         (define L.test.10 (halt 10)))
        ((define L.place.1 (halt 4))))))

  ;; multiple jumping to the same label
  (check-match
    (trace-schedule
      '(module
        (define L.test.1 (begin (set! fv0 10) (jump L.test.2)))
        (define L.test.2 (if (< fv0 9) (jump L.test.4) (jump L.test.3)))
        (define L.test.3 (halt 0))
        (define L.test.4 (halt 1))
        (define L.test.5 (jump L.test.4))
        (define L.test.6 (jump L.test.4))
        (define L.test.7 (jump L.test.4))))
    `(module
      (define L.test.1 (begin (set! fv0 10) (jump L.test.2)))
      (define L.test.2 (if (< fv0 9) (jump L.test.4) (jump L.test.3)))
      (define L.test.3 (halt 0))
      (define ,li (jump L.test.4))
      (define L.test.4 (halt 1))
      (define ,lj (jump L.test.4))
      (define ,lk (jump L.test.4)))
    (and (member 'L.test.5 (list li lj lk))
         (member 'L.test.6 (list li lj lk))
         (member 'L.test.7 (list li lj lk))
         (= (set-count (list->set (list li lj lk))) 3)))
  )