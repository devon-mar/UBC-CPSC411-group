#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(provide select-instructions)

;; Milestone 4 Exercise 17
;; Milestone 5 Exercise 5
;; Milestone 6 Exercise 6
;; Milestone 7 Exercise 7
;;
;; Compiles Imp-cmf-lang v7 to Asm-pred-lang v7, selecting appropriate
;; sequences of abstract assembly instructions to implement the operations of
;; the source language.
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v7? asm-pred-lang-v7?)

  ;; Selects appropriate sequence of abstract assembly instructions
  ;; to implement the operations in the given procedure represented
  ;; by label, info, and tail.
  ;;
  ;; tail: imp-cmf-lang-v7-tail
  ;; -> asm-pred-lang-v7-tail
  (define/contract (select-instructions-proc label info tail)
    (-> label? info? any/c any/c)
    `(define
       ,label
       ,info
       ,(select-instructions-tail tail)))

  ;; imp-cmf-lang-v7-p asm-pred-lang-v7-p
  (define (select-instructions-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
           ,info
           ,@(map select-instructions-proc labels infos tails)
           ,(select-instructions-tail tail))]))

  ;; imp-cmf-lang-v7-pred asm-pred-lang-v7-pred
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

  ;; imp-cmf-lang-v7-tail asm-tail-lang-v7-tail
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


  ;; v: imp-cmf-lang-v7-value
  ;; f: (asm-pred-lang-v7-triv -> asm-pred-lang-v7-effect)
  ;; -> asm-pred-lang-v7-effect
  (define (select-instructions-value v f)
    (match v
      [`(,b ,o1 ,o2)
        (define tmp (fresh 'tmp-v))
        (make-begin
          `((set! ,tmp ,o1)
            (set! ,tmp (,b ,tmp ,o2)))
          (f tmp))]
      ;; triv
      [_ (f v)]))

  ;; imp-cmf-lang-v7-effect asm-effect-lang-v7-effect
  (define (select-instructions-effect e)
    (match e
      [`(set! ,l ,v)
        ;; (set! loc value) -> (set! loc triv)
        (select-instructions-value
          v
          (lambda (t) `(set! ,l ,t)))]
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

  ;; o: imp-cmf-lang-v7-opand
  ;; f: (-> asm-pred-lang-v7-loc asm-pred-lang-v7-pred)
  ;; -> asm-pred-lang-v7-pred
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
    (interp-asm-pred-lang-v7 (select-instructions p))
    42)

  ;; e: imp-cmf-lang-v7-effect
  (define-check (check-42 e)
    (check-equal?
      (interp-asm-pred-lang-v7 (select-instructions (boilerplate e)))
      42))

  ;; wrap e with boilerplate
  ;;
  ;; e: imp-cmf-lang-v7-effect
  (define/contract (boilerplate e)
    (-> any/c imp-cmf-lang-v7?)
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
  )
