#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide link-paren-x64)

(define/contract (link-paren-x64 p)
  (-> paren-x64-v4? paren-x64-rt-v4?)

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (trg? t) (int64? t)))

  (define/contract (trg? t)
    (-> any/c boolean?)
    (or (register? t)
        (label? t)))


  ;; Compiles Paren-x64 v4 to Paren-x64-rt v4 by resolving all labels to their
  ;;  position in the instruction sequence.
  ;;
  ;; labels: dictionary of label to index
  ;; p: paren-x64-v4-p
  ;; -> paren-x64-rt-v4-p
  (define (link-paren-x64-p labels p)
    (match p
      [`(begin ,s ...)
        `(begin
           ,@(map (lambda (s) (link-paren-x64-s labels s)) s))]))

  (define (link-paren-x64-s labels s)
    (match s
      [`(set! ,_ (,_ ,_ ,int32))
        #:when (int32? int32)
        s]
      [`(set! ,_ (,_ ,_ ,_))
        s]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        `(set! ,reg ,(link-paren-x64-triv labels triv))]
      [`(set! ,reg ,_)
        #:when (register? reg)
        s]
      [`(set! ,_ ,int32)
        #:when (int32? int32)
        s]
      [`(set! ,addr ,trg)
        `(set! ,addr ,(link-paren-x64-trg labels trg))]
      [`(compare ,_ ,_)
        s]
      [`(with-label ,_ ,s)
        (link-paren-x64-s labels s)]
      [`(jump ,trg)
        `(jump ,(link-paren-x64-trg labels trg))]
      [`(jump-if ,relop ,label)
        `(jump-if ,relop ,(dict-ref labels label))]))

  ;; labels: dictionary of label to index
  ;; t: paren-x64-v4-trg
  ;; -> paren-x64-rt-v4-trg
  (define (link-paren-x64-trg labels t)
    (match t
      [(? register?) t]
      [(? label?) (dict-ref labels t)]))

  ;; labels: dictionary of label to index
  ;; t: paren-x64-v4-triv
  ;; -> paren-x64-rt-v4-triv
  (define (link-paren-x64-triv labels t)
    (match t
      [(? trg?) (link-paren-x64-trg labels t)]
      [(? int64?) t]))

  #;
  (define (link-paren-x64-opand o)
    (match o
      [(? int64?) (void)]
      [(? register?) (void)]))

  #;
  (define (link-paren-x64-loc l)
    (match l
      [(? register?) (void)]
      [addr (void)]))

  ;; Resolve labels to indexes and add to d.
  ;;
  ;; idx: index of the current instruction
  ;; d: dictionary of label to index
  ;; s: paren-x64-v4-s
  ;; -> dictionary of label to index
  (define/contract (labels->addrs-s d idx s)
    (-> dict? natural-number/c any/c dict?)
    (match s
      [`(with-label ,label ,_)
        (dict-set d label idx)]
      ;; Modified template - compressed other cases
      ;; into one.
      [_ d]))

  ;; Returns a dict of label to index.
  (define/contract (labels->addrs-p p)
    (-> any/c dict?)
    (match p
      [`(begin ,s ...)
        (for/fold ([acc '()])
                  ([s s]
                   [i (in-naturals)])
        (labels->addrs-s acc i s))]))

  (link-paren-x64-p (labels->addrs-p p) p))

(module+ test
  (require rackunit)

  ;; interp-paren-x64-rt-v4 doesn't seem to exist right now...
  #;
  (define (check-42 p)
    (check-equal?
      (interp-paren-x64-rt-v4 (link-paren-x64 p))
      42))


  ;; A temporary test
  (check-equal?
    (link-paren-x64
      '(begin
         (with-label L.a.1 (set! rax 42))
         (compare rax 42)
         (jump-if != L.a.1)
         (jump L.b.1)
         (with-label L.b.1 (set! rsi L.c.1))
         (with-label L.c.1 (set! rax 42))))
    '(begin
       (set! rax 42)
       (compare rax 42)
       (jump-if != 0)
       (jump 4)
       (set! rsi 5)
       (set! rax 42)))

  )

