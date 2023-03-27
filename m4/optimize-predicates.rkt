#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6)

(provide optimize-predicates)

;; Milestone 4 Exercise 11
;; Milestone 5 Exercise 12
;; Milestone 6 Exercise 15
;;
;; Optimize Nested-asm-lang-fvars programs by analyzing and simplifying predicates.
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-fvars-v6? nested-asm-lang-fvars-v6?)

  ;; env loc triv -> void
  ;; Update loc to triv in env if it is decided (integer),
  ;; otherwise remove key
  (define (env-update env loc triv)
    (if (integer? triv)
        (set-box! env (dict-set (unbox env) loc triv))
        (set-box! env (dict-remove (unbox env) loc))))

  ;; relop -> procedure
  (define (symbol->relop relop)
    (match relop
      [`>= >=]
      [`<= <=]
      [`< <]
      [`> >]
      [`= =]
      [`!= (lambda (x y) (not (= x y)))]))

  ;; binop -> procedure
  (define (binop->procedure binop)
    (match binop
      [`+ x64-add]
      [`* x64-mul]
      [`- x64-sub]))

  ;; opand env -> opand
  (define (convert-opand opand env)
    (match opand
      [(? integer?) opand]
      [(or (? register?) (? fvar?)) ;; loc
       (dict-ref (unbox env) opand opand)]))

  ;; triv env -> triv
  (define (convert-triv triv env)
    (match triv
      [(? label?) triv]
      [opand (convert-opand opand env)]))

  ;; tail env -> tail
  (define (convert-tail t env)
    (match t
      [`(jump ,_) t]
      [`(begin ,effects ... ,tail)
       (define new-effects (convert-effect-list effects env))
       `(begin
         ,@new-effects
         ,(convert-tail tail env))]
      [`(if ,pred ,tail1 ,tail2)
       (convert-if
         pred
         (lambda (e) (convert-tail tail1 e))
         (lambda (e) (convert-tail tail2 e))
         env)]))

  ;; pred (env -> tail) (env -> tail) env -> tail
  ;; pred (env -> effect) (env -> effect) env -> effect
  ;; Convert an (if pred t1 t2) where
  ;; tfn1 creates t1 and tfn2 creates t2
  (define (convert-if pred tfn1 tfn2 env)
    (match pred
      [`(true) ;; base case
       (tfn1 env)]
      [`(false) ;; base case
       (tfn2 env)]
      [`(not ,nested-pred)
       (convert-if nested-pred tfn2 tfn1 env)]
      [`(begin ,effects ... ,nested-pred)
       (define new-effects (convert-effect-list effects env))
       (define new-tail (convert-if nested-pred tfn1 tfn2 env))
       `(begin ,@new-effects ,new-tail)]
      [`(,relop ,loc ,opand) ;; base case
       (convert-relop relop loc opand tfn1 tfn2 env)]
      [`(if ,pred ,pred1 ,pred2)
       (convert-if
         pred
         (lambda (e) (convert-if pred1 tfn1 tfn2 e))
         (lambda (e) (convert-if pred2 tfn1 tfn2 e))
         env)]))

  ;; relop loc opand (env -> tail) (env -> tail) env -> tail
  ;; relop loc opand (env -> effect) (env -> effect) env -> effect
  ;; Convert an (if (relop loc opand) t1 t2) where
  ;; tfn1 creates t1 and tfn2 creates t2
  (define (convert-relop relop loc opand tfn1 tfn2 env)
    (define a1 (convert-triv loc env))
    (define a2 (convert-opand opand env))
    (if (and (integer? a1) (integer? a2))
        (if ((symbol->relop relop) a1 a2)
            (tfn1 env)
            (tfn2 env))
        (let ([env1 (box (unbox env))]
              [env2 (box (unbox env))])
          ;; interpret both branches with env copies, merge envs after
          (begin0
            `(if (,relop ,loc ,opand) ,(tfn1 env1) ,(tfn2 env2))
            (set-box! env (set-intersect (unbox env1) (unbox env2)))))))

  ;; (effect ...) env -> (effect ...)
  (define (convert-effect-list effect-list env)
    (for/fold ([new-effect-list '()]) ([e effect-list])
      (define new-effect (convert-effect e env))
      (append new-effect-list (list new-effect))))

  ;; effect env -> effect
  (define (convert-effect e env)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
       (env-update env loc_1 (convert-binop binop loc_1 opand env))
       `(set! ,loc_1 (,binop ,loc_1 ,opand))]
      [`(set! ,loc ,triv)
       (env-update env loc (convert-triv triv env))
       `(set! ,loc ,triv)]
      [`(begin ,effects ...)
       `(begin ,@(convert-effect-list effects env))]
      [`(if ,pred ,effect1 ,effect2)
       (convert-if
         pred
         (lambda (e) (convert-effect effect1 e))
         (lambda (e) (convert-effect effect2 e))
         env)]
      [`(return-point ,label ,tail)
       (begin0
        `(return-point ,label ,(convert-tail tail env))
         ;; clear env after return
         (set-box! env '()))]))

  ;; binop loc triv env -> symbol
  ;; binop loc triv env -> integer
  (define (convert-binop binop loc opand env)
    (define interp-loc (convert-triv loc env))
    (define interp-opand (convert-opand opand env))
    (if (and (integer? interp-loc) (integer? interp-opand))
        ((binop->procedure binop) interp-loc interp-opand)
        `(,binop ,loc ,opand)))

   ;; (define label tail) -> (define label tail)
   (define (convert-block block)
     (match block
       [`(define ,label ,tail)
         ;; Begin with clear env since source of jump could be anywhere.
         (define env (box '()))
         `(define ,label ,(convert-tail tail env))]))

  ;; ((define label tail) ...)
  (define (convert-block-list block-list)
    (map convert-block block-list))

  ;; p -> p
  (define (convert-p p)
    (match p
      [`(module ,block-list ... ,tail)
        ;; env is box(dict(loc, int)) (we use box to support set-intersect)
        (define env (box '()))
        `(module ,@(convert-block-list block-list),(convert-tail tail env))]))

  (convert-p p))

(module+ test
  (require rackunit)

  ;; check that optimized program is as expected and interprets to the same value
  ;; p p -> void
  (define-check (check-equal-interp? orig expected)
    (check-equal? (optimize-predicates orig) expected)
    (check-equal?
      (interp-nested-asm-lang-fvars-v6 (optimize-predicates orig))
      (interp-nested-asm-lang-fvars-v6 orig)))

  ;; check that optimization did not change the program
  ;; p -> void
  (define-check (check-no-change? program)
    (check-equal? (optimize-predicates program) program))


  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (> fv1 0)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (< fv1 0)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 2) (jump done)))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (< fv1 ,(max-int 64))
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
      `(module
        (begin
          (set! fv1 1)
          (begin (set! rax 1) (jump done)))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 2)
          (set! fv1 (+ fv1 3))
          (if (= fv1 5)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 2)
        (set! fv1 (+ fv1 3))
        (begin (set! rax 1) (jump done)))))

  ;; Test >= relop equal
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (>= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; Test >= relop greater than
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (>= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; Test <= relop equal
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (<= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; Test <= relop less than
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 0)
          (if (<= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 0)
        (begin (set! rax 1) (jump done)))))

  ;; Test != relop
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (!= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 2) (jump done)))))

  ;; Test = relop
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (= fv1 1)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; true
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (true)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; false
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (false)
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 2) (jump done)))))

  ;; not false
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (not (false))
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; not true
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (not (true))
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 2) (jump done)))))

  ;; double not
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (not (not (true)))
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; Begin effects pred
  (check-equal-interp?
    `(module
      (begin
        (set! fv1 1)
        (if (begin
              (set! fv0 1)
              (set! fv2 2)
              (< fv0 fv2))
            (begin (set! rax 1) (jump done))
            (begin (set! rax 2) (jump done)))))
    `(module
      (begin
        (set! fv1 1)
        (begin
          (set! fv0 1)
          (set! fv2 2)
          (begin (set! rax 1) (jump done))))))

  ;; check (if pred pred pred)
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (if (true) (true) (false))
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (begin (set! rax 1) (jump done)))))

  ;; check (if pred pred pred)
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (if (true) (> rsp rax) (false))
              (begin (set! rax 1) (jump done))
              (begin (set! rax 2) (jump done))))))
    `(module
      (begin
        (set! fv1 1)
        (if (> rsp rax)
            (begin (set! rax 1) (jump done))
            (begin (set! rax 2) (jump done))))))

  ;; check (set! loc (+ loc opand)) is identity
  (check-no-change?
    `(module
      (begin
        (set! fv1 1)
        (set! fv1 (+ fv1 2))
        (set! rax fv1)
        (jump done))))

  ;; check nested begin is identity
  (check-no-change?
    `(module
      (begin
        (begin
          (set! fv1 1)
          (set! fv1 (+ fv1 2)))
        (set! rax fv1)
        (jump done))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (begin
            (set! fv1 1)
            (if (true) (set! fv1 (+ fv1 2)) (set! fv1 0)))
          (set! rax fv1)
          (jump done))))
    `(module
      (begin
        (begin
          (set! fv1 1)
          (set! fv1 (+ fv1 2)))
        (set! rax fv1)
        (jump done))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (begin
            (set! fv1 1)
            (if (false) (set! fv1 (+ fv1 2)) (set! fv1 0)))
          (set! rax fv1)
          (jump done))))
    `(module
      (begin
        (begin
          (set! fv1 1)
          (set! fv1 0))
        (set! rax fv1)
        (jump done))))

  ;; Optimize predicate in block and jump in tail
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.1
          (if (true)
              (begin (set! rax 42) (jump done))
              (begin (set! rax 2) (jump done))))
        (jump L.block.1)))
    `(module
      (define L.block.1 (begin (set! rax 42) (jump done)))
      (jump L.block.1)))

  ;; Handle jump in block
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.2 (jump L.block.1))
        (define L.block.1
          (if (true)
              (begin (set! rax 42) (jump done))
              (begin (set! rax 2) (jump done))))
        (jump L.block.2)))
    `(module
      (define L.block.2 (jump L.block.1))
      (define L.block.1 (begin (set! rax 42) (jump done)))
      (jump L.block.2)))

  ;; (set! loc label) place label into location and jump
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.1
          (if (true)
              (begin (set! rax 42) (jump done))
              (begin (set! rax 2) (jump done))))
        (begin
          (set! rax L.block.1)
          (jump rax))))
    `(module
      (define L.block.1 (begin (set! rax 42) (jump done)))
        (begin
          (set! rax L.block.1)
          (jump rax))))

  ;; Handle relops inside of jump
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.1
          (begin
            (set! rax 5)
            (if (= rax 5)
                (begin (set! rax 42) (jump done))
                (begin (set! rax 2) (jump done)))))
        (jump L.block.1)))
    `(module
      (define L.block.1
        (begin
          (set! rax 5)
          (begin (set! rax 42) (jump done))))
      (jump L.block.1)))

  ;; Binop with unknown values is identity
  (check-no-change?
    `(module
      (begin
        (set! rax (+ rax 1))
        (jump done))))

  ;; int64 is bound
  (check-equal-interp?
    '(module
      (begin
        (set! rax 9223372036854775807)
        (set! rax (+ rax 1))
        (if (> rax 9223372036854775807)
            (begin (set! rax 2) (jump done))
            (jump done))))
    '(module
      (begin
        (set! rax 9223372036854775807)
        (set! rax (+ rax 1))
        (jump done))))

  ;; check set inside of pred evaluation
  (check-equal-interp?
    '(module
      (begin
        (set! fv1 9)
        (if (begin (set! fv2 10) (= fv1 9))
            (if (= fv2 10)
                (begin (set! rax 22) (jump done))
                (begin (set! rax 17) (jump done)))
            (begin (set! rax fv1) (jump done)))))
    '(module
      (begin
        (set! fv1 9)
        (begin
          (set! fv2 10)
          (begin (set! rax 22) (jump done))))))

  ;; check set inside of pred inside of if tail pred evaluation
  (check-equal-interp?
    '(module
      (begin
        (set! fv1 9)
        (set! fv2 2)
        (if (if (begin (set! fv1 fv2) (= fv1 9))
                (begin (set! fv2 9) (false))
                (begin (set! fv2 fv1) (= fv2 2)))
            (begin (set! rax 2) (jump done))
            (begin (set! rax 3) (jump done)))))
    '(module
      (begin
        (set! fv1 9)
        (set! fv2 2)
        (begin
          (set! fv1 fv2)
          (begin
            (set! fv2 fv1)
            (begin (set! rax 2) (jump done)))))))

  ;; check set inside of pred inside of if effect pred evaluation
  (check-equal-interp?
    '(module
      (begin
        (set! fv1 9)
        (set! fv2 2)
        (if (if (begin (set! fv1 fv2) (= fv1 9))
                (begin (set! fv2 9) (false))
                (begin (set! fv2 fv1) (= fv2 2)))
            (set! fv3 fv2)
            (set! fv3 11))
        (if (= fv1 2)
            (begin (set! rax 3) (jump done))
            (begin (set! rax 15) (jump done)))))
    '(module
      (begin
        (set! fv1 9)
        (set! fv2 2)
        (begin
          (set! fv1 fv2)
          (begin
            (set! fv2 fv1)
            (set! fv3 fv2)))
        (begin (set! rax 3) (jump done)))))

  ;; Environment following undecided branch is intersect
  (check-equal-interp?
    '(module
      (define L.test.1
        (begin
          (if (= fv0 2)
              (set! fv1 3)
              (set! fv1 3))
          (if (= fv1 3)
              (set! rax 2)
              (set! rax 8))
          (jump done)))
      (begin
        (set! fv0 2)
        (jump L.test.1)))
    '(module
      (define L.test.1
        (begin
          (if (= fv0 2)
              (set! fv1 3)
              (set! fv1 3))
          (set! rax 2)
          (jump done)))
      (begin
        (set! fv0 2)
        (jump L.test.1))))

  ;; Value following undecided set! is undefined
  (check-no-change?
    '(module
      (define L.test.1
        (begin
          (set! fv0 9)
          (set! fv0 (+ fv0 fv1))
          (if (= fv1 9)
              (set! rax 1)
              (set! rax 2))
          (jump done)))
      (define L.test.2
        (begin
          (set! fv0 12)
          (set! fv0 fv1)
          (if (= fv1 12)
              (set! rax 3)
              (set! rax 4))
          (jump done)))
      (begin
        (jump done))))

  ;; Value is not carried to another branch in undecided if
  (check-no-change?
    '(module
      (define L.test.1
        (if (= fv1 4)
            (begin
              (set! fv1 9)
              (if (= fv2 10)
                  (set! rax 1)
                  (set! rax 2))
              (jump done))
            (begin
              (set! fv2 10)
              (if (= fv1 9)
                  (set! rax 3)
                  (set! rax 4))
              (jump done))))
      (begin
        (jump done))))

  ;; subtract
  (check-equal-interp?
    '(module
      (begin
        (set! rax 10)
        (set! rax (- rax 18))
        (if (<= rax -8) (set! rax 42) (set! rax (- rax 2)))
        (jump done)))
    '(module
      (begin
        (set! rax 10)
        (set! rax (- rax 18))
        (set! rax 42)
        (jump done))))

  ;; return-point
  (check-equal-interp?
    '(module
      (define L.test.1
        (begin
          (set! rdx r15)
          (set! rax 20)
          (jump r15)))
      (begin
        (set! fv0 r15)
        (set! rax 2)
        (return-point L.rp.1
          (begin
            (set! r15 L.rp.1)
            (if (= rax 2)
                (jump L.test.1)
                (jump done))))
        (set! rax (+ rax 5))
        (if (< rax 7) (set! rax 11) (set! rax 13))
        (jump fv0)))
    '(module
      (define L.test.1
        (begin
          (set! rdx r15)
          (set! rax 20)
          (jump r15)))
      (begin
        (set! fv0 r15)
        (set! rax 2)
        (return-point L.rp.1
          (begin
            (set! r15 L.rp.1)
            (jump L.test.1)))
        (set! rax (+ rax 5))
        (if (< rax 7) (set! rax 11) (set! rax 13))
        (jump fv0))))
  )
