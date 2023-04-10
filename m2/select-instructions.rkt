#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide select-instructions)

;; Milestone 4 Exercise 17
;; Milestone 5 Exercise 5
;; Milestone 6 Exercise 6
;; Milestone 7 Exercise 7
;; Milestone 8 Exercise 9
;;
;; Selects appropriate sequences of abstract assembly instructions to
;; implement the operations of the source language.
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v8? asm-alloc-lang-v8?)

  ;; Selects appropriate sequence of abstract assembly instructions
  ;; to implement the operations in the given procedure represented
  ;; by label, info, and tail.
  ;;
  ;; tail: imp-cmf-lang-v8-tail
  ;; -> asm-alloc-lang-v8-tail
  (define/contract (select-instructions-proc label info tail)
    (-> label? info? any/c any/c)
    `(define
       ,label
       ,info
       ,(select-instructions-tail tail)))

  ;; imp-cmf-lang-v8-p asm-alloc-lang-v8-p
  (define (select-instructions-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
           ,info
           ,@(map select-instructions-proc labels infos tails)
           ,(select-instructions-tail tail))]))

  ;; imp-cmf-lang-v8-pred asm-alloc-lang-v8-pred
  (define (select-instructions-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(select-instructions-pred p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map select-instructions-effect es)
           ,(select-instructions-pred p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(select-instructions-pred p1)
           ,(select-instructions-pred p2)
           ,(select-instructions-pred p3))]
      [`(,r ,o1 ,o2)
        ;; (relop opand opand) -> (relop loc opand)
        (select-instructions-opand
          o1
          (lambda (l) `(,r ,l ,o2)))]))

  ;; imp-cmf-lang-v8-tail asm-tail-lang-v8-tail
  (define (select-instructions-tail t)
    (match t
      [`(begin ,es ... ,t)
        `(begin
           ,@(map select-instructions-effect es)
           ,(select-instructions-tail t))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(select-instructions-pred p)
           ,(select-instructions-tail t1)
           ,(select-instructions-tail t2))]
      [`(jump ,_ ,_ ...)
        t]))


  ;; v: imp-cmf-lang-v8-value
  ;; f: ((or/c asm-alloc-lang-v8-triv (alloc index) (mref loc index)) -> asm-alloc-lang-v8-effect)
  ;; -> asm-alloc-lang-v8-effect
  (define (select-instructions-value v f)
    (match v
      ;; (mref ,loc ,opand)
      [`(mref ,_ ,_)
        (f v)]
      ;; (alloc opand)
      [`(alloc ,_)
        (f v)]
      [`(,b ,o1 ,o2)
        (define tmp (fresh 'tmp-v))
        (make-begin
          `((set! ,tmp ,o1)
            (set! ,tmp (,b ,tmp ,o2)))
          (f tmp))]
      ;; triv
      [_ (f v)]))

  ;; imp-cmf-lang-v8-effect asm-effect-lang-v8-effect
  (define (select-instructions-effect e)
    (match e
      [`(set! ,l ,v)
        ;; (set! loc value) -> (set! loc triv)
        (select-instructions-value
          v
          (lambda (t) `(set! ,l ,t)))]
      ;; (mset! ,loc ,opand ,triv)
      [`(mset! ,_ ,_ ,_) e]
      ;; modified template - removed tail effect
      [`(begin ,es ...)
        `(begin ,@(map select-instructions-effect es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(select-instructions-pred p)
           ,(select-instructions-effect e1)
           ,(select-instructions-effect e2))]
      [`(return-point ,l ,t)
        `(return-point ,l ,(select-instructions-tail t))]))

  ;; o: imp-cmf-lang-v8-opand
  ;; f: (-> asm-alloc-lang-v8-loc asm-alloc-lang-v8-pred)
  ;; -> asm-alloc-lang-v8-pred
  (define (select-instructions-opand o f)
    (match o
      [(? int64?)
       (define tmp (fresh 'tmp-o))
       (make-begin
         `((set! ,tmp ,o))
         (f tmp))]
      [(? aloc?) (f o)]))
  
  ;; not used
  #;
  (define (select-instructions-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  ;; not used
  #;
  (define (select-instructions-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  ;; not used
  #;
  (define (select-instructions-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  ;; not used
  #;
  (define (select-instructions-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]
      ['bitwise-ior (void)]
      ['bitwise-and (void)]
      ['bitwise-xor (void)]
      ['arithmetic-shift-right (void)]))

  ;; not used
  #;
  (define (select-instructions-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (select-instructions-p p))

(module+ test
  (require rackunit)

  (define-check (check-42p p)
    (interp-asm-alloc-lang-v8 (select-instructions p))
    42)

  ;; e: imp-cmf-lang-v8-effect
  (define-check (check-42 e)
    (check-equal?
      (interp-asm-alloc-lang-v8 (select-instructions (boilerplate e)))
      42))

  ;; wrap e with boilerplate
  ;;
  ;; e: imp-cmf-lang-v8-effect
  (define/contract (boilerplate e)
    (-> any/c imp-cmf-lang-v8?)
    `(module
       ((new-frames ()))
       (begin (set! tmp-ra.1 r15)
         (begin
           ,e
           (jump tmp-ra.1 rbp rax)))))

  (define rax (current-return-value-register))

  ;; basic
  (check-42 `(set! ,rax 42))
  (check-42 `(set! ,rax (+ 40 2)))
  (check-42 `(set! ,rax (* 21 2)))
  (check-42 `(set! ,rax (- 50 8)))
  
  ;; relop with 2 int64s
  (check-42
     `(if (= 1 1)
        (set! ,rax 42)
        (set! ,rax 0)))
  ;; relop with 2 int64s inside not
  (check-42
     `(if (not (= 1 1))
        (set! ,rax 0)
        (set! ,rax 42)))

  ;; relop with int64, triv
  (check-42
    `(begin
       (set! x.1 1)
       (if (= 1 x.1)
         (set! ,rax 42)
         (set! ,rax 0))))

  ;; relop with triv, int64
  ;; (no changes needed)
  (check-42
    `(begin
       (set! x.1 1)
       (if (= x.1 1)
         (set! ,rax 42)
         (set! ,rax 0))))

  ;; lots of ifs
  (check-42
    `(begin
       (if
         (begin
           (set! x.2 10)
           (> x.2 1))
         (set! x.1 42)
         (set! x.1 2))
       (if
         (if (false) (true) (false))
         (set! ,rax 0)
         (if (not (true))
           (set! ,rax 0)
           (set! ,rax x.1)))))

  ;; M2 tests
  ; simple
  (check-42 `(set! ,rax 42))

  ; basic binop
  (check-42 `(set! ,rax (+ 40 2)))

  ; basic set and return
  (check-42 `(begin (set! x.1 42) (set! ,rax x.1)))

  ; add from two alocs and return result
  (check-42
   `(begin
      (set! x.1 40)
      (set! x.2 2)
      (set! ,rax (+ x.1 x.2))))

  ; nested begins
  (check-42
    `(begin
       (begin
         (set! x.1 40)
         (set! x.2 2)
         (begin
           (set! x.1 (+ x.1 x.2))))
       (set! ,rax x.1)))


  ;; nothing to change here
  (check-42
    `(begin
       (set! x.2 21)
       (set! x.2 (* x.2 2))
       (set! ,rax x.2)))

  (check-42
    `(begin
       (set! x.2 21)
       (set! z.3 (* x.2 2))
       (set! ,rax z.3)))

  ;; M5 tests
  (check-42p
    '(module
       ((new-frames ()))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! tmp-ra.1 r15)
           (begin (begin (set! rax 42) (jump tmp-ra.1 rbp rax)))))
       (begin
         (set! tmp-ra.2 r15)
         (begin (set! r15 tmp-ra.2) (jump L.foo.1 rbp r15)))))

  (check-42p
    '(module
       ((new-frames ()))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! tmp-ra.1 r15)
           (begin (begin (set! rax (+ 40 2)) (jump tmp-ra.1 rbp rax)))))
       (begin
         (set! tmp-ra.2 r15)
         (begin (set! r15 tmp-ra.2) (jump L.foo.1 rbp r15)))))

  (check-42p
    '(module
       ((new-frames ()))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! tmp-ra.2 r15)
           (begin
             (begin
               (set! x.1 (+ 40 2))
               (begin (set! rax x.1) (jump tmp-ra.2 rbp rax))))))
       (begin
         (set! tmp-ra.3 r15)
         (begin (set! r15 tmp-ra.3) (jump L.foo.1 rbp r15)))))

  (check-42p
    '(module
       ((new-frames ()))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! tmp-ra.1 r15)
           (begin
             (if (> 10 2)
               (begin (set! rax 40) (jump tmp-ra.1 rbp rax))
               (begin (set! rax 2) (jump tmp-ra.1 rbp rax))))))
       (begin
         (set! tmp-ra.2 r15)
         (begin (set! r15 tmp-ra.2) (jump L.foo.1 rbp r15)))))


  ;; M6 tests
  (check-42p
    '(module
       ((new-frames ()))
       (define L.bar.1
         ((new-frames ()))
         (begin
           (set! tmp-ra.6 r15)
           (begin
             (set! x.2 rdi)
             (set! y.1 rsi)
             (begin (set! rax (+ x.2 y.1)) (jump tmp-ra.6 rbp rax)))))
       (define L.foo.2
         ((new-frames (() ())))
         (begin
           (set! tmp-ra.7 r15)
           (begin
             (set! z.3 rdi)
             (begin
               (if (= z.3 0)
                 (begin
                   (return-point
                    L.rp.3
                    (begin
                      (set! rsi 2)
                      (set! rdi 40)
                      (set! r15 L.rp.3)
                      (jump L.bar.1 rbp r15 rdi rsi)))
                   (set! x.4 rax))
                 (begin
                   (return-point
                    L.rp.4
                    (begin
                      (set! rsi 2)
                      (set! rdi 1)
                      (set! r15 L.rp.4)
                      (jump L.bar.1 rbp r15 rdi rsi)))
                   (set! x.4 rax)))
               (begin (set! rax x.4) (jump tmp-ra.7 rbp rax))))))
       (begin
         (set! tmp-ra.8 r15)
         (begin
           (set! x.5 0)
           (begin (set! rdi x.5) (set! r15 tmp-ra.8) (jump L.foo.2 rbp r15 rdi))))))

  (check-42p
    '(module
       ((new-frames (())))
       (define L.foo.1
         ((new-frames ()))
         (begin (set! tmp-ra.2 r15) (set! rax 50) (jump tmp-ra.2 rbp rax)))
       (begin
         (set! tmp-ra.3 r15)
         (return-point L.rp.2 (begin (set! r15 L.rp.2) (jump L.foo.1 rbp r15)))
         (set! x.1 rax)
         (if (begin (set! tmp.4 1) (= tmp.4 2))
           (begin (set! rax 0) (jump tmp-ra.3 rbp rax))
           (begin (set! rax x.1) (set! rax (- rax 8)) (jump tmp-ra.3 rbp rax))))))

  ;; M8 test
  (check-42p
    '(module
       ((new-frames
         ((nfv.110 nfv.111)
          (nfv.108 nfv.109)
          (nfv.105 nfv.106 nfv.107)
          (nfv.102 nfv.103 nfv.104)
          (nfv.101))))
       (define L.+.8
         ((new-frames ()))
         (begin
           (set! tmp-ra.92 r15)
           (begin
             (set! tmp.21 fv0)
             (set! tmp.22 fv1)
             (if (begin
                   (if (begin (set! tmp.57 (bitwise-and tmp.22 7)) (= tmp.57 0))
                     (set! tmp.56 14)
                     (set! tmp.56 6))
                   (!= tmp.56 6))
               (if (begin
                     (if (begin (set! tmp.59 (bitwise-and tmp.21 7)) (= tmp.59 0))
                       (set! tmp.58 14)
                       (set! tmp.58 6))
                     (!= tmp.58 6))
                 (begin (set! rax (+ tmp.21 tmp.22)) (jump tmp-ra.92 rbp rax))
                 (begin (set! rax 574) (jump tmp-ra.92 rbp rax)))
               (begin (set! rax 574) (jump tmp-ra.92 rbp rax))))))
       (define L.unsafe-vector-ref.3
         ((new-frames ()))
         (begin
           (set! tmp-ra.93 r15)
           (begin
             (set! tmp.16 fv0)
             (set! tmp.17 fv1)
             (if (begin
                   (if (begin (set! tmp.61 (mref tmp.16 -3)) (< tmp.17 tmp.61))
                     (set! tmp.60 14)
                     (set! tmp.60 6))
                   (!= tmp.60 6))
               (if (begin
                     (if (>= tmp.17 0) (set! tmp.62 14) (set! tmp.62 6))
                     (!= tmp.62 6))
                 (begin
                   (begin
                     (begin
                       (set! tmp.65 (arithmetic-shift-right tmp.17 3))
                       (set! tmp.64 (* tmp.65 8)))
                     (set! tmp.63 (+ tmp.64 5)))
                   (begin (set! rax (mref tmp.16 tmp.63)) (jump tmp-ra.93 rbp rax)))
                 (begin (set! rax 2878) (jump tmp-ra.93 rbp rax)))
               (begin (set! rax 2878) (jump tmp-ra.93 rbp rax))))))
       (define L.vector-ref.7
         ((new-frames ()))
         (begin
           (set! tmp-ra.94 r15)
           (begin
             (set! tmp.38 fv0)
             (set! tmp.39 fv1)
             (if (begin
                   (if (begin (set! tmp.67 (bitwise-and tmp.39 7)) (= tmp.67 0))
                     (set! tmp.66 14)
                     (set! tmp.66 6))
                   (!= tmp.66 6))
               (if (begin
                     (if (begin (set! tmp.69 (bitwise-and tmp.38 7)) (= tmp.69 3))
                       (set! tmp.68 14)
                       (set! tmp.68 6))
                     (!= tmp.68 6))
                 (begin
                   (set! fv1 tmp.39)
                   (set! fv0 tmp.38)
                   (set! r15 tmp-ra.94)
                   (jump L.unsafe-vector-ref.3 rbp r15 fv0 fv1))
                 (begin (set! rax 2878) (jump tmp-ra.94 rbp rax)))
               (begin (set! rax 2878) (jump tmp-ra.94 rbp rax))))))
       (define L.unsafe-vector-set!.2
         ((new-frames ()))
         (begin
           (set! tmp-ra.95 r15)
           (begin
             (set! tmp.11 fv0)
             (set! tmp.12 fv1)
             (set! tmp.13 fv2)
             (if (begin
                   (if (begin (set! tmp.71 (mref tmp.11 -3)) (< tmp.12 tmp.71))
                     (set! tmp.70 14)
                     (set! tmp.70 6))
                   (!= tmp.70 6))
               (if (begin
                     (if (>= tmp.12 0) (set! tmp.72 14) (set! tmp.72 6))
                     (!= tmp.72 6))
                 (begin
                   (begin
                     (begin
                       (begin
                         (set! tmp.75 (arithmetic-shift-right tmp.12 3))
                         (set! tmp.74 (* tmp.75 8)))
                       (set! tmp.73 (+ tmp.74 5)))
                     (mset! tmp.11 tmp.73 tmp.13))
                   (begin (set! rax 30) (jump tmp-ra.95 rbp rax)))
                 (begin (set! rax 2622) (jump tmp-ra.95 rbp rax)))
               (begin (set! rax 2622) (jump tmp-ra.95 rbp rax))))))
       (define L.vector-set!.6
         ((new-frames ()))
         (begin
           (set! tmp-ra.96 r15)
           (begin
             (set! tmp.35 fv0)
             (set! tmp.36 fv1)
             (set! tmp.37 fv2)
             (if (begin
                   (if (begin (set! tmp.77 (bitwise-and tmp.36 7)) (= tmp.77 0))
                     (set! tmp.76 14)
                     (set! tmp.76 6))
                   (!= tmp.76 6))
               (if (begin
                     (if (begin (set! tmp.79 (bitwise-and tmp.35 7)) (= tmp.79 3))
                       (set! tmp.78 14)
                       (set! tmp.78 6))
                     (!= tmp.78 6))
                 (begin
                   (set! fv2 tmp.37)
                   (set! fv1 tmp.36)
                   (set! fv0 tmp.35)
                   (set! r15 tmp-ra.96)
                   (jump L.unsafe-vector-set!.2 rbp r15 fv0 fv1 fv2))
                 (begin (set! rax 2622) (jump tmp-ra.96 rbp rax)))
               (begin (set! rax 2622) (jump tmp-ra.96 rbp rax))))))
       (define L.vector-init-loop.4
         ((new-frames ()))
         (begin
           (set! tmp-ra.97 r15)
           (begin
             (set! len.8 fv0)
             (set! i.10 fv1)
             (set! vec.9 fv2)
             (if (begin
                   (if (= len.8 i.10) (set! tmp.80 14) (set! tmp.80 6))
                   (!= tmp.80 6))
               (begin (set! rax vec.9) (jump tmp-ra.97 rbp rax))
               (begin
                 (begin
                   (begin
                     (begin
                       (set! tmp.83 (arithmetic-shift-right i.10 3))
                       (set! tmp.82 (* tmp.83 8)))
                     (set! tmp.81 (+ tmp.82 5)))
                   (mset! vec.9 tmp.81 0))
                 (begin
                   (set! tmp.84 (+ i.10 8))
                   (begin
                     (set! fv2 vec.9)
                     (set! fv1 tmp.84)
                     (set! fv0 len.8)
                     (set! r15 tmp-ra.97)
                     (jump L.vector-init-loop.4 rbp r15 fv0 fv1 fv2))))))))
       (define L.make-init-vector.1
         ((new-frames ()))
         (begin
           (set! tmp-ra.98 r15)
           (begin
             (set! tmp.6 fv0)
             (if (begin
                   (if (>= tmp.6 0) (set! tmp.85 14) (set! tmp.85 6))
                   (!= tmp.85 6))
               (begin
                 (begin
                   (begin
                     (begin
                       (begin
                         (begin
                           (set! tmp.89 (arithmetic-shift-right tmp.6 3))
                           (set! tmp.88 (+ 1 tmp.89)))
                         (set! tmp.87 (* tmp.88 8)))
                       (set! tmp.86 (alloc tmp.87)))
                     (set! tmp.55 (+ tmp.86 3)))
                   (begin (mset! tmp.55 -3 tmp.6) (set! tmp.7 tmp.55)))
                 (begin
                   (set! fv2 tmp.7)
                   (set! fv1 0)
                   (set! fv0 tmp.6)
                   (set! r15 tmp-ra.98)
                   (jump L.vector-init-loop.4 rbp r15 fv0 fv1 fv2)))
               (begin (set! rax 3134) (jump tmp-ra.98 rbp rax))))))
       (define L.make-vector.5
         ((new-frames ()))
         (begin
           (set! tmp-ra.99 r15)
           (begin
             (set! tmp.33 fv0)
             (if (begin
                   (if (begin (set! tmp.91 (bitwise-and tmp.33 7)) (= tmp.91 0))
                     (set! tmp.90 14)
                     (set! tmp.90 6))
                   (!= tmp.90 6))
               (begin
                 (set! fv0 tmp.33)
                 (set! r15 tmp-ra.99)
                 (jump L.make-init-vector.1 rbp r15 fv0))
               (begin (set! rax 2110) (jump tmp-ra.99 rbp rax))))))
       (begin
         (set! tmp-ra.100 r15)
         (begin
           (begin
             (return-point
              L.rp.9
              (begin
                (set! nfv.101 16)
                (set! r15 L.rp.9)
                (jump L.make-vector.5 rbp r15 nfv.101)))
             (set! v.1 rax))
           (begin
             (begin
               (return-point
                L.rp.10
                (begin
                  (set! nfv.104 8)
                  (set! nfv.103 0)
                  (set! nfv.102 v.1)
                  (set! r15 L.rp.10)
                  (jump L.vector-set!.6 rbp r15 nfv.102 nfv.103 nfv.104)))
               (set! x.3 rax))
             (begin
               (return-point
                L.rp.11
                (begin
                  (set! nfv.107 328)
                  (set! nfv.106 8)
                  (set! nfv.105 v.1)
                  (set! r15 L.rp.11)
                  (jump L.vector-set!.6 rbp r15 nfv.105 nfv.106 nfv.107)))
               (set! y.2 rax))
             (begin
               (begin
                 (return-point
                  L.rp.12
                  (begin
                    (set! nfv.109 0)
                    (set! nfv.108 v.1)
                    (set! r15 L.rp.12)
                    (jump L.vector-ref.7 rbp r15 nfv.108 nfv.109)))
                 (set! one.5 rax))
               (begin
                 (return-point
                  L.rp.13
                  (begin
                    (set! nfv.111 8)
                    (set! nfv.110 v.1)
                    (set! r15 L.rp.13)
                    (jump L.vector-ref.7 rbp r15 nfv.110 nfv.111)))
                 (set! two.4 rax))
               (begin
                 (set! fv1 two.4)
                 (set! fv0 one.5)
                 (set! r15 tmp-ra.100)
                 (jump L.+.8 rbp r15 fv0 fv1))))))))
  )
