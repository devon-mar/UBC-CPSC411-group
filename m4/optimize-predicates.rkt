#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide optimize-predicates)

;; Milestone 4 Exercise 11
;; Milestone 5 Exercise 12
;; Milestone 6 Exercise 15
;;
;; Optimize Nested-asm-lang programs by analyzing and simplifying predicates.
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-v5? nested-asm-lang-v5?)

  ;; env loc triv -> void
  ;; Update loc to triv if it is decided (integer), otherwise remove key
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
      [`* x64-mul]))

  ;; triv env -> triv
  (define (convert-triv triv env)
    (match triv
      [(? integer?) triv]
      [(? symbol?) (dict-ref (unbox env) triv triv)]))

  ;; tail env -> tail
  (define (convert-tail t env)
    (match t
      [`(halt ,triv) t]
      [`(jump ,trg) t]
      [`(begin ,effects ... ,tail)
       (define new-effects (convert-effect-list effects env))
       `(begin
         ,@new-effects
         ,(convert-tail tail env))]
      [`(if ,pred ,tail1 ,tail2)
       (convert-if-tail
         pred
         (lambda (e) (convert-tail tail1 e))
         (lambda (e) (convert-tail tail2 e))
         env)]))

  ;; pred (env -> tail) (env -> tail) env -> tail
  (define (convert-if-tail pred tfn1 tfn2 env)
    (match pred
      [`(true) ;; base case
       (tfn1 env)]
      [`(false) ;; base case
       (tfn2 env)]
      [`(not ,nested-pred)
       (convert-if-tail nested-pred tfn2 tfn1 env)]
      [`(begin ,effects ... ,nested-pred)
       (define new-effects (convert-effect-list effects env))
       (define new-tail (convert-if-tail nested-pred tfn1 tfn2 env))
       `(begin ,@new-effects ,new-tail)]
      [`(,relop ,loc ,triv) ;; base case
       (convert-relop relop loc triv tfn1 tfn2 env)]
      [`(if ,pred ,pred1 ,pred2)
       (convert-if-tail
         pred
         (lambda (e) (convert-if-tail pred1 tfn1 tfn2 e))
         (lambda (e) (convert-if-tail pred2 tfn1 tfn2 e))
         env)]))

  ;; pred (effect -> effect env) (effect -> effect env) env -> effect
  (define (convert-if-effect pred efn1 efn2 env)
    (match pred
      [`(true) ;; base case
       (efn1 env)]
      [`(false) ;; base case
       (efn2 env)]
      [`(not ,nested-pred)
       (convert-if-effect nested-pred efn2 efn1 env)]
      [`(begin ,effects ... ,nested-pred)
       (define new-effects (convert-effect-list effects env))
       (define new-effect-t (convert-if-effect nested-pred efn1 efn2 env))
       `(begin ,@new-effects ,new-effect-t)]
      [`(,relop ,loc ,triv) ;; base case
       (convert-relop relop loc triv efn1 efn2 env)]
      [`(if ,pred ,pred1 ,pred2)
       (convert-if-effect
         pred
         (lambda (e) (convert-if-effect pred1 efn1 efn2 e))
         (lambda (e) (convert-if-effect pred2 efn1 efn2 e))
         env)]))

  ;; relop loc triv (env -> tail) (env -> tail) env -> tail
  ;; relop loc triv (env -> effect) (env -> effect) env -> effect
  (define (convert-relop relop loc triv tfn1 tfn2 env)
    (define a1 (convert-triv loc env))
    (define a2 (convert-triv triv env))
    (if (and (integer? a1) (integer? a2))
        (if ((symbol->relop relop) a1 a2)
            (tfn1 env)
            (tfn2 env))
        (let ([env1 (box (unbox env))] [env2 (box (unbox env))])
          (begin0
            `(if (,relop ,loc ,triv) ,(tfn1 env1) ,(tfn2 env2))
            (set-box! env (set-intersect (unbox env1) (unbox env2)))))))

  ;; (effect ...) env -> (effect ...)
  (define (convert-effect-list effect-list env)
    (for/fold ([new-effect-list '()]) ([e effect-list])
      (define new-effect (convert-effect e env))
      (append new-effect-list (list new-effect))))

  ;; effect env -> (values effect env)
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
       (convert-if-effect
         pred
         (lambda (e) (convert-effect effect1 e))
         (lambda (e) (convert-effect effect2 e))
         env)]))

  ;; binop loc triv env -> symbol
  ;; binop loc triv env -> integer
  (define (convert-binop binop loc opand env)
    (define interp-loc (convert-triv loc env))
    (define interp-opand (convert-triv opand env))
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
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (> fv1 0) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (< fv1 0) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 2))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (< fv1 ,(max-int 64)) (halt 1) (halt 2)))))
      `(module
        (begin
          (set! fv1 1)
          (halt 1))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 2)
          (set! fv1 (+ fv1 3))
          (if (= fv1 5) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 2)
        (set! fv1 (+ fv1 3))
        (halt 1))))

  ;; Test >= relop equal
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (>= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; Test >= relop greater than
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (>= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; Test <= relop equal
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (<= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; Test <= relop less than
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 0)
          (if (<= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 0)
        (halt 1))))

  ;; Test != relop
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (!= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 2))))

  ;; Test = relop
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (= fv1 1) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; true
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (true) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; false
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (false) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 2))))

  ;; not false
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (not (false)) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; not true
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (not (true)) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 2))))

  ;; double not
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (not (not (true))) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  (define if-begin-test
    `(module (begin
               (set! fv1 1)
               (if (begin
                     (set! fv0 1)
                     (set! fv2 2)
                     (< fv0 fv2))
                   (halt 1)
                   (halt 2)))))
  ;; Begin effects pred
  (check-equal?
    (optimize-predicates if-begin-test)
    `(module
      (begin
        (set! fv1 1)
        (begin
          (set! fv0 1)
          (set! fv2 2)
          (halt 1)))))
  (check-equal? (interp-nested-asm-lang-v5 if-begin-test)
                (interp-nested-asm-lang-v5 (optimize-predicates if-begin-test)))

  ;; check (if pred pred pred)
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (if (true) (true) (false)) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (halt 1))))

  ;; check (if pred pred pred)
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (if (if (true) (> rsp rax) (false)) (halt 1) (halt 2)))))
    `(module
      (begin
        (set! fv1 1)
        (if (> rsp rax) (halt 1) (halt 2)))))

  ;; check (set! loc (+ loc opand)) is identity
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! fv1 1)
          (set! fv1 (+ fv1 2))
          (halt fv1))))
    `(module
      (begin
        (set! fv1 1)
        (set! fv1 (+ fv1 2))
        (halt fv1))))

  ;; check nested begin is identity
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (begin
            (set! fv1 1)
            (set! fv1 (+ fv1 2)))
          (halt fv1))))
    `(module
      (begin
        (begin
          (set! fv1 1)
          (set! fv1 (+ fv1 2)))
        (halt fv1))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (begin
            (set! fv1 1)
            (if (true) (set! fv1 (+ fv1 2)) (set! fv1 0)))
          (halt fv1))))
    `(module
      (begin
        (begin
          (set! fv1 1)
          (set! fv1 (+ fv1 2)))
        (halt fv1))))

  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (begin
            (set! fv1 1)
            (if (false) (set! fv1 (+ fv1 2)) (set! fv1 0)))
          (halt fv1))))
    `(module
      (begin
        (begin
          (set! fv1 1)
          (set! fv1 0))
        (halt fv1))))

  ;; Optimize predicate in block and jump in tail
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.1 (if (true) (halt 42) (halt 2)))
        (jump L.block.1)))
    `(module
      (define L.block.1 (halt 42))
      (jump L.block.1)))

  ;; Handle jump in block
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.2 (jump L.block.1))
        (define L.block.1 (if (true) (halt 42) (halt 2)))
        (jump L.block.2)))
    `(module
      (define L.block.2 (jump L.block.1))
      (define L.block.1 (halt 42))
      (jump L.block.2)))

  ;; (set! loc label) place label into location and jump
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.1 (if (true) (halt 42) (halt 2)))
        (begin
          (set! rax L.block.1)
          (jump rax))))
    `(module
      (define L.block.1 (halt 42))
        (begin
          (set! rax L.block.1)
          (jump rax))))

  ;; Handle relops inside of jump
  (check-equal?
    (optimize-predicates
      `(module
        (define L.block.1 (begin (set! rax 5) (if (= rax 5) (halt 42) (halt 2))))
        (jump L.block.1)))
    `(module
      (define L.block.1 (begin (set! rax 5) (halt 42)))
      (jump L.block.1)))

  ;; Binop with unknown values is identity
  (check-equal?
    (optimize-predicates
      `(module
        (begin
          (set! rax (+ rax 1))
          (halt rax))))
    `(module
      (begin
        (set! rax (+ rax 1))
        (halt rax))))

  ;; int64 is bound
  (check-equal?
    (optimize-predicates
      '(module
        (begin
          (set! rax 9223372036854775807)
          (set! rax (+ rax 1))
          (if (> rax 9223372036854775807)
              (begin (set! rax 2) (jump done))
              (jump done)))))
    '(module
      (begin
        (set! rax 9223372036854775807)
        (set! rax (+ rax 1))
        (jump done))))

  ;; check set inside of pred evaluation
  (check-equal?
    (optimize-predicates
      '(module
        (begin
          (set! fv1 9)
          (if (begin (set! fv2 10) (= fv1 9))
              (if (= fv2 10) (halt fv2) (halt 17))
              (halt fv1)))))
    '(module
      (begin
        (set! fv1 9)
        (begin (set! fv2 10) (halt fv2)))))

  ;; check set inside of pred inside of if tail pred evaluation
  (check-equal?
    (optimize-predicates
      '(module
        (begin
          (set! fv1 9)
          (set! fv2 2)
          (if (if (begin (set! fv1 fv2) (= fv1 9))
                  (begin (set! fv2 9) (false))
                  (begin (set! fv2 fv1) (= fv2 2)))
              (halt 2)
              (halt 3)))))
    '(module
      (begin
        (set! fv1 9)
        (set! fv2 2)
        (begin
          (set! fv1 fv2)
          (begin
            (set! fv2 fv1)
            (halt 2))))))

  ;; check set inside of pred inside of if effect pred evaluation
  (check-equal?
    (optimize-predicates
      '(module
        (begin
          (set! fv1 9)
          (set! fv2 2)
          (if (if (begin (set! fv1 fv2) (= fv1 9))
                  (begin (set! fv2 9) (false))
                  (begin (set! fv2 fv1) (= fv2 2)))
              (set! fv3 fv2)
              (set! fv3 11))
          (if (= fv1 2) (halt 3) (halt 15)))))
    '(module
      (begin
        (set! fv1 9)
        (set! fv2 2)
        (begin
          (set! fv1 fv2)
          (begin
            (set! fv2 fv1)
            (set! fv3 fv2)))
        (halt 3))))

  ;; Environment following undecided branch is intersect
  (check-equal?
    (optimize-predicates
      '(module
        (define L.test.1
          (begin
            (if (= fv0 2)
                (set! fv1 3)
                (set! fv1 3))
            (if (= fv1 3) (halt 2) (halt 8))))
        (begin
          (halt 3))))
    '(module
      (define L.test.1
        (begin
          (if (= fv0 2)
              (set! fv1 3)
              (set! fv1 3))
          (halt 2)))
      (begin
        (halt 3))))

  ;; Value following undecided set! is undefined
  (check-equal?
    (optimize-predicates
      '(module
        (define L.test.1
          (begin
            (set! fv0 9)
            (set! fv0 (+ fv0 fv1))
            (if (= fv1 9) (halt 1) (halt 2))))
        (define L.test.2
          (begin
            (set! fv0 12)
            (set! fv0 fv1)
            (if (= fv1 12) (halt 3) (halt 4))))
        (begin
          (halt 3))))
    '(module
      (define L.test.1
        (begin
          (set! fv0 9)
          (set! fv0 (+ fv0 fv1))
          (if (= fv1 9) (halt 1) (halt 2))))
      (define L.test.2
        (begin
          (set! fv0 12)
          (set! fv0 fv1)
          (if (= fv1 12) (halt 3) (halt 4))))
      (begin
        (halt 3))))

  ;; Value is not carried to another branch in undecided if
  (check-equal?
    (optimize-predicates
      '(module
        (define L.test.1
          (if (= fv1 4)
              (begin (set! fv1 9) (if (= fv2 10) (halt 1) (halt 2)))
              (begin (set! fv2 10) (if (= fv1 9) (halt 3) (halt 4)))))
        (begin
          (halt 4))))
    '(module
      (define L.test.1
        (if (= fv1 4)
            (begin (set! fv1 9) (if (= fv2 10) (halt 1) (halt 2)))
            (begin (set! fv2 10) (if (= fv1 9) (halt 3) (halt 4)))))
      (begin
        (halt 4))))
  )
