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

  ;; triv dict -> triv
  (define (convert-triv triv env)
    (match triv
      [(? integer?) triv]
      [(? symbol?) (if (dict-has-key? env triv) (dict-ref env triv) triv)]))

  ;; tail dict -> tail
  (define (convert-tail t env)
    (match t
      [`(halt ,triv) t]
      [`(jump ,trg) t]
      [`(begin ,effects ... ,tail)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       `(begin
         ,@new-effects
         ,(convert-tail tail new-env))]
      [`(if ,pred ,tail1 ,tail2)
       (convert-if-tail
         pred
         (lambda (e) (convert-tail tail1 e))
         (lambda (e) (convert-tail tail2 e))
         env)]))

  ;; pred (dict -> tail) (dict -> tail) dict -> tail
  (define (convert-if-tail pred tfn1 tfn2 env)
    (match pred
      [`(true) ;; base case
       (tfn1 env)]
      [`(false) ;; base case
       (tfn2 env)]
      [`(not ,nested-pred)
       (convert-if-tail nested-pred tfn2 tfn1 env)]
      [`(begin ,effects ... ,nested-pred)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       `(begin
         ,@new-effects
         ,(convert-if-tail nested-pred tfn1 tfn2 new-env))]
      [`(,relop ,loc ,triv) ;; base case
       (convert-relop
         relop
         (convert-triv loc env)
         (convert-triv triv env)
         (lambda () (tfn1 env))
         (lambda () (tfn2 env))
         (lambda ()
           `(if ,pred ,(tfn1 env) ,(tfn2 env))))]
      [`(if ,pred ,pred1 ,pred2)
       (convert-if-tail
         pred
         (lambda (e) (convert-if-tail pred1 tfn1 tfn2 e))
         (lambda (e) (convert-if-tail pred2 tfn1 tfn2 e))
         env)]))

  ;; pred (effect -> effect dict) (effect -> effect dict) dict -> effect dict
  (define (convert-if-effect pred efn1 efn2 env)
    (match pred
      [`(true) ;; base case
       (efn1 env)]
      [`(false) ;; base case
       (efn2 env)]
      [`(not ,nested-pred)
       (convert-if-effect nested-pred efn2 efn1 env)]
      [`(begin ,effects ... ,nested-pred)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       (define-values (new-effect-t new-env-t) (convert-if-effect nested-pred efn1 efn2 new-env))
       (values
         `(begin
           ,@new-effects
           ,new-effect-t)
         new-env-t)]
      [`(,relop ,loc ,triv) ;; base case
       (convert-relop
         relop
         (convert-triv loc env)
         (convert-triv triv env)
         (lambda () (efn1 env))
         (lambda () (efn2 env))
         (lambda ()
           (define-values (new-effect1 new-env1) (efn1 env))
           (define-values (new-effect2 new-env2) (efn2 env))
           (values `(if ,pred ,new-effect1 ,new-effect2)
                   (set-intersect new-env1 new-env2))))]
      [`(if ,pred ,pred1 ,pred2)
       (convert-if-effect
         pred
         (lambda (e) (convert-if-effect pred1 efn1 efn2 e))
         (lambda (e) (convert-if-effect pred2 efn1 efn2 e))
         env)]))

  ;; relop triv triv (-> tail) (-> tail) (-> tail) -> tail
  ;; relop triv triv (-> effect dict) (-> effect dict) (-> effect dict) -> effect dict
  (define (convert-relop relop a1 a2 tfn1 tfn2 tfn-if)
    (if (and (integer? a1) (integer? a2))
        (if ((symbol->relop relop) a1 a2)
            (tfn1)
            (tfn2))
        (tfn-if)))

  ;; (effect ...) dict -> (values (effect ...) dict)
  (define (convert-effect-list effect-list env)
    (for/fold ([new-effect-list '()] [intermediate-env env]) ([e effect-list])
      (define-values (new-effect new-env) (convert-effect e intermediate-env))
      (values (append new-effect-list (list new-effect)) new-env)))

  ;; dict loc triv -> dict
  ;; Add loc to triv if it is decided (integer), otherwise remove key
  (define (env-add env loc triv)
    (if (integer? triv)
        (dict-set env loc triv)
        (dict-remove env loc)))

  ;; effect dict -> (values effect dict)
  (define (convert-effect e env)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
       (define binop-evaluation (convert-binop binop loc_1 opand env))
       (values `(set! ,loc_1 (,binop ,loc_1 ,opand))
               (env-add env loc_1 binop-evaluation))]
      [`(set! ,loc ,triv)
       (values `(set! ,loc ,triv)
               (env-add env loc (convert-triv triv env)))]
      [`(begin
          ,effects ...)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       (values `(begin
                  ,@new-effects)
               new-env)]
      [`(if ,pred ,effect1 ,effect2)
       (convert-if-effect
         pred
         (lambda (e) (convert-effect effect1 e))
         (lambda (e) (convert-effect effect2 e))
         env)]))

  ;; binop loc triv dict -> symbol
  ;; binop loc triv dict -> integer
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
         (define env '())
         `(define ,label ,(convert-tail tail env))]))

  ;; ((define label tail) ...)
  (define (convert-block-list block-list)
    (map convert-block block-list))

  ;; p -> p
  (define (convert-p p)
    (match p
      [`(module ,block-list ... ,tail)
        (define env '())
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
  )
