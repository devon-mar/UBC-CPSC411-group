#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide implement-fvars)

;; Milestone 2 Exercise 11
;; Milestone 4 Exercise 4
;; Milestone 6 Exercise 17
;; Milestone 7 Exercise 7
;; Milestone 8 Exercise 11
;;
;; Reifies fvars into displacement mode operands.
(define/contract (implement-fvars p)
  (-> nested-asm-lang-fvars-v8? nested-asm-lang-v8?)

  (define offset 0)

  ;; nested-asm-lang-fvars-v8-p -> nested-asm-lang-v8-p
  (define (implement-fvars-p p)
    (match p
      [`(module (define ,labels ,tails) ... ,tail)
        (set! offset 0)
        (define new-tail (implement-fvars-tail tail))
        `(module
           ,@(map implement-fvars-proc labels tails)
           ,new-tail)]))

  ;; tail: nested-asm-lang-fvars-v8-tail
  ;; -> nested-asm-lang-v8-tail
  (define/contract (implement-fvars-proc label tail)
    (-> label? any/c any/c)
    (set! offset 0)
    `(define
       ,label
       ,(implement-fvars-tail tail)))

  ;; nested-asm-lang-fvars-v8-pred -> nested-asm-lang-v8-pred
  (define (implement-fvars-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,pred)
        `(not ,(implement-fvars-pred pred))]
      [`(begin ,effects ... ,pred)
        `(begin
           ,@(map implement-fvars-effect effects)
           ,(implement-fvars-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
        `(if
           ,(implement-fvars-pred pred1)
           ,(implement-fvars-pred pred2)
           ,(implement-fvars-pred pred3))]
      [`(,relop ,opand1 ,opand2)
        `(,relop
          ,(implement-fvars-opand opand1)
          ,(implement-fvars-opand opand2))]))

  ;; nested-asm-lang-fvars-v8-tail -> nested-asm-lang-v8-tail
  (define (implement-fvars-tail t)
    (match t
      [`(jump ,trg)
        `(jump ,(implement-fvars-trg trg))]
      [`(begin ,effects ... ,tail)
        `(begin
           ,@(map implement-fvars-effect effects)
           ,(implement-fvars-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
        `(if
           ,(implement-fvars-pred pred)
           ,(implement-fvars-tail tail1)
           ,(implement-fvars-tail tail2))]))

  ;; nested-asm-lang-fvars-v8-effect -> nested-asm-lang-v8-effect
  (define (implement-fvars-effect e)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,index))
       `(set!
         ,(implement-fvars-loc loc1)
         (mref
           ,(implement-fvars-loc loc2)
           ,(implement-fvars-index index)))]
      [`(set! ,loc (,binop ,loc ,opand))
        (define nloc (implement-fvars-loc loc))
        (begin0
        `(set! ,nloc (,binop ,nloc ,(implement-fvars-opand opand)))
        (when (and (equal? loc (current-frame-base-pointer-register)) (number? opand))
        ;; Assumption: bitwise operations will not be used on the frame base pointer register
          (set! offset ((match binop ['- -] ['+ +]) offset opand))))]
      [`(set! ,loc ,triv)
        `(set!
           ,(implement-fvars-loc loc)
           ,(implement-fvars-triv triv))]
      [`(mset! ,loc ,index ,triv)
       `(mset!
         ,(implement-fvars-loc loc)
         ,(implement-fvars-index index)
         ,(implement-fvars-triv triv))]
      [`(begin ,effects ... ,effectt)
        `(begin
           ,@(map implement-fvars-effect (append effects (list effectt))))]
      [`(if ,pred ,effect1 ,effect2)
        `(if
           ,(implement-fvars-pred pred)
           ,(implement-fvars-effect effect1)
           ,(implement-fvars-effect effect2))]
      [`(return-point ,label ,tail)
        `(return-point
           ,label
           ,(implement-fvars-tail tail))]))

  ;; nested-asm-lang-fvars-v8-opand -> nested-asm-lang-v8-opand
  (define (implement-fvars-opand o)
    (match o
      [(? int64?) o]
      [loc (implement-fvars-loc loc)]))

  (define (implement-fvars-triv t)
    (match t
      [(? label?) t]
      [opand (implement-fvars-opand opand)]))

  ;; Return the displacement mode operand for fvar f.
  ;; fvar -> addr
  (define/contract (implement-fvars-fvar f)
    (-> fvar? any/c)
    (define doffset
      (+ (* (fvar->index f) (current-word-size-bytes)) offset))
    `(,(current-frame-base-pointer-register) - ,doffset))

  ;; nested-asm-lang-fvars-v8-loc -> nested-asm-lang-v8-loc
  (define (implement-fvars-loc l)
    (match l
      [(? register?) l]
      [(? fvar?) (implement-fvars-fvar l)]))

  ;; nested-asm-lang-fvars-v8-trg -> nested-asm-lang-v8-trg
  (define (implement-fvars-trg t)
    (match t
      [(? label?) t]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v8-index -> nested-asm-lang-v8-index
  (define (implement-fvars-index i)
    (match i
      [(? int64?) i]
      [loc (implement-fvars-loc loc)]))

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

  ;; https://piazza.com/class/lcquw2vc2cs73i/post/339
  (check-equal?
    (implement-fvars
      '(module
         (define L.swap.1
           (begin
             (set! fv4 r15)
             (set! r14 fv0)
             (set! r15 fv1)
             (if (< r15 r14)
                 (begin
                   (set! rax r14)
                   (jump fv4))
                 (begin
                   (begin
                     (set! rbp (- rbp 40))
                     (return-point L.rp.1
                       (begin
                         (set! fv5 r15)
                         (set! fv6 r14)
                         (set! r15 L.rp.1)
                         (jump L.swap.1)))
                     (set! rbp (+ rbp 40)))
                   (jump fv4)))))
         (begin
           (set! fv1 7)
           (set! fv0 4)
           (jump L.swap.1))))
    '(module
       (define L.swap.1
         (begin
           (set! (rbp - 32) r15)
           (set! r14 (rbp - 0))
           (set! r15 (rbp - 8))
           (if (< r15 r14)
             (begin (set! rax r14) (jump (rbp - 32)))
             (begin
               (begin
                 (set! rbp (- rbp 40))
                 (return-point
                  L.rp.1
                  (begin
                    (set! (rbp - 0) r15)
                    (set! (rbp - 8) r14)
                    (set! r15 L.rp.1)
                    (jump L.swap.1)))
                 (set! rbp (+ rbp 40)))
               (jump (rbp - 32))))))
       (begin (set! (rbp - 8) 7) (set! (rbp - 0) 4) (jump L.swap.1))))

  ;; Identity on bitwise operations
  (check-equal?
    (implement-fvars
      `(module
         (begin
           (set! rax 2)
           (set! rax (arithmetic-shift-right rax 1))
           (set! rax (bitwise-and rax 1))
           (set! rax (bitwise-ior rax 2))
           (set! rax (bitwise-xor rax rax))
           (jump r15))))
    `(module
         (begin
           (set! rax 2)
           (set! rax (arithmetic-shift-right rax 1))
           (set! rax (bitwise-and rax 1))
           (set! rax (bitwise-ior rax 2))
           (set! rax (bitwise-xor rax rax))
           (jump r15))))

  ;; Check that the compiled program interprets to 42
  (define-check (check-42 p)
    (check-equal?
      (interp-nested-asm-lang-v8 (implement-fvars p))
      42))

  ;; mref mset
  (check-42
    '(module
      (define L.test.1
        (begin
          (set! rcx (mref r12 rcx))
          (set! rax (* rax rcx))
          (jump done)))
      (begin
        (set! rcx 64)
        (set! rdx -1)
        (set! fv0 r12)
        (set! fv1 16)
        (set! fv2 -92)
        (set! fv3 24)
        (mset! fv0 fv1 fv2)
        (mset! r12 fv3 L.test.1)
        (mset! r12 32 -134)
        (mset! r12 rcx rdx)
        (set! fv4 (mref fv0 fv1))
        (set! rax (mref r12 32))
        (set! rax (- rax fv4))
        (set! rdx (mref r12 fv3))
        (jump rdx))))

  )
