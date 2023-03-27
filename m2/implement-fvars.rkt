#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6)

(provide implement-fvars)

;; Milestone 2 Exercise 11
;; Milestone 4 Exercise 4
;; Milestone 6 Exercise 17
;;
;; Reifies fvars into displacement mode operands.
(define/contract (implement-fvars p)
  (-> nested-asm-lang-fvars-v6? nested-asm-lang-v6?)

  ;; nested-asm-lang-fvars-v6-p nested-asm-lang-v6-p
  (define (implement-fvars-p p)
    (match p
      [`(module (define ,labels ,tails) ... ,tail)
        `(module
           ,@(map implement-fvars-proc labels tails)
           ,(implement-fvars-tail tail))]))

  ;; tail: nested-asm-lang-fvars-v6-tail
  ;; -> nested-asm-lang-v6-tail
  (define/contract (implement-fvars-proc label tail)
    (-> label? any/c any/c)
    `(define
       ,label
       ,(implement-fvars-tail tail)))

  ;; nested-asm-lang-fvars-v6-pred nested-asm-lang-v6-pred
  (define (implement-fvars-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(implement-fvars-pred p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map implement-fvars-effect es)
           ,(implement-fvars-pred p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(implement-fvars-pred p1)
           ,(implement-fvars-pred p2)
           ,(implement-fvars-pred p3))]
      [`(,r ,o1 ,o2)
        `(,r
          ,(implement-fvars-opand o1)
          ,(implement-fvars-opand o2))]))

  ;; nested-asm-lang-fvars-v6-tail nested-asm-lang-v6-tail
  (define (implement-fvars-tail t)
    (match t
      [`(jump ,trg)
        `(jump ,(implement-fvars-trg trg))]
      [`(begin ,es ... ,t)
        `(begin
           ,@(map implement-fvars-effect es)
           ,(implement-fvars-tail t))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(implement-fvars-pred p)
           ,(implement-fvars-tail t1)
           ,(implement-fvars-tail t2))]))

  ;; nested-asm-lang-fvars-v6-effect nested-asm-lang-v6-effect
  (define (implement-fvars-effect e)
    (match e
      [`(set! ,l (,b ,l ,o))
        (define loc (implement-fvars-loc l))
        `(set! ,loc (,b ,loc ,(implement-fvars-opand o)))]
      [`(set! ,l ,t)
        `(set!
           ,(implement-fvars-loc l)
           ,(implement-fvars-triv t))]
      ;; modified template - removed tail effect
      [`(begin ,es ...)
        `(begin ,@(map implement-fvars-effect es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(implement-fvars-pred p)
           ,(implement-fvars-effect e1)
           ,(implement-fvars-effect e2))]
      [`(return-point ,l ,t)
        `(return-point
           ,l
           ,(implement-fvars-tail t))]))

  ;; nested-asm-lang-fvars-v6-opand nested-asm-lang-v6-opand
  (define (implement-fvars-opand o)
    (match o
      [(? int64?) o]
      [_ (implement-fvars-loc o)]))

  (define (implement-fvars-triv t)
    (match t
      [(? label?) t]
      [_ (implement-fvars-opand t)]))

  ;; Return the displacement mode operand for fvar f.
  (define/contract (implement-fvars-fvar f)
    (-> fvar? any/c)
    `(,(current-frame-base-pointer-register) - ,(* 8 (fvar->index f))))

  ;; nested-asm-lang-fvars-v6-loc nested-asm-lang-v6-loc
  (define (implement-fvars-loc l)
    (match l
      [(? register?) l]
      [(? fvar?) (implement-fvars-fvar l)]))

  ;; nested-asm-lang-fvars-v6-trg nested-asm-lang-v6-trg
  (define (implement-fvars-trg t)
    (match t
      [(? label?) t]
      [_ (implement-fvars-loc t)]))

  (implement-fvars-p p))

(module+ test
  (require rackunit)

  (define/contract (fv idx)
    (-> exact-nonnegative-integer? any/c)
    `(,(current-frame-base-pointer-register) - ,(* 8 idx)))

  (check-equal?
    (implement-fvars
      `(module
         (define
           L.foo.1
           ;; tail/(begin effect ... tail)
           (begin
             (set! fv5 L.bar.1)
             ;; effect/(set! loc triv)
             (set! fv1 123)
             ;; effect/(set! loc_1 (binop loc_1 opand))
             (set! fv1 (- fv1 fv2))
             ;; tail/(jump trg)
             (jump fv0)))
         (define L.bar.1
           (begin
             ;; tail/(if pred tail tail)
             ;; pred/(if pred pred pred)
             ;; pred/(relop loc opand)
             (if (if (= fv0 fv1) (= fv2 fv3) (= fv3 fv4))
               (jump fv0)
               (jump L.test.1))))
         (begin
           ;; effect/(begin effect ... effect)
           (begin
             (set! fv2 123)
             (set! fv3 456)
             (set! fv4 789))
           (jump fv3))))
    `(module
       (define
         L.foo.1
         (begin
           (set! ,(fv 5) L.bar.1)
           (set! ,(fv 1) 123)
           (set! ,(fv 1) (- ,(fv 1) ,(fv 2)))
           (jump ,(fv 0))))
       (define L.bar.1
         (begin
           (if (if (= ,(fv 0) ,(fv 1)) (= ,(fv 2) ,(fv 3)) (= ,(fv 3) ,(fv 4)))
             (jump ,(fv 0))
             (jump L.test.1))))
       (begin
         (begin
           (set! ,(fv 2) 123)
           (set! ,(fv 3) 456)
           (set! ,(fv 4) 789))
         (jump ,(fv 3)))))

  (check-equal?
    (implement-fvars
      `(module
         (begin
           ;; effect/(if pred effect effect)
           ;; pred/(true)
           ;; pred/(false)
           ;; pred/(not pred)
           ;; pred/(begin effect ... pred)
           (if (begin
                 (set! fv0 fv1)
                 (set! fv2 fv3)
                 (if (true) (false) (not (= fv0 fv1))))
             (set! r8 10)
             (set! r8 12))
           ;; effect/(return-point label tail)
           (return-point L.rt.1 (jump fv4))
           (jump r15))))
    `(module
       (begin
         ;; effect/(if pred effect effect)
         ;; pred/(true)
         ;; pred/(false)
         ;; pred/(not pred)
         ;; pred/(begin effect ... pred)
         (if (begin
               (set! ,(fv 0) ,(fv 1))
               (set! ,(fv 2) ,(fv 3))
               (if (true) (false) (not (= ,(fv 0) ,(fv 1)))))
           (set! r8 10)
           (set! r8 12))
         ;; effect/(return-point label tail)
         (return-point L.rt.1 (jump ,(fv 4)))
         (jump r15))))
  )
