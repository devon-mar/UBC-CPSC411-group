#lang racket

(require cpsc411/compiler-lib
         cpsc411/graph-lib
         cpsc411/langs/v4)
(provide optimize-predicates)

;; Exercise #11
;; nested-asm-lang-v4? -> nested-asm-lang-v4?
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-v4? nested-asm-lang-v4?)
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
      [`+ +]
      [`* *]))

  ;; triv dict -> triv
  (define (convert-triv triv env)
    (match triv
      [(? integer?) triv]
      [(? symbol?) (if (dict-has-key? env triv) (dict-ref env triv) triv)]))

  ;; tail dict -> tail
  (define (convert-tail t env)
    (match t
      [`(halt ,triv) `(halt ,triv)]
      [`(begin
          ,effects ...
          ,tail)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       `(begin
          ,@new-effects
          ,(convert-tail tail new-env))]
      [`(if ,pred ,tail1 ,tail2)
       (convert-if pred (convert-tail tail1 env) (convert-tail tail2 env) env)]))

  ;; pred tail tail dict -> tail
  ;; pred effect effect dict -> effect
  (define (convert-if pred t1 t2 env)
    (match pred
      [`(true) t1]
      [`(false) t2]
      [`(not ,nested-pred) (convert-if nested-pred t2 t1 env)]
      [`(begin
          ,effects ...
          ,nested-pred)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       `(begin
          ,@new-effects
          ,(convert-if nested-pred t1 t2 new-env))]
      [`(,relop ,loc ,triv)
       (convert-relop relop (convert-triv loc env) (convert-triv triv env) t1 t2)]
      [`(if ,pred ,pred1 ,pred2)
       (convert-if pred (convert-if pred1 t1 t2 env) (convert-if pred2 t1 t2 env) env)]))

  ;; relop triv triv tail tail -> tail
  ;; relop triv triv effect effect -> effect
  (define (convert-relop relop a1 a2 tail1 tail2)
    (define (abstract-relop relop)
      (if (and (integer? a1) (integer? a2))
          (if ((symbol->relop relop) a1 a2) tail1 tail2)
          `(if (,relop ,a1 ,a2) ,tail1 ,tail2)))
    (abstract-relop relop))

  ;; (effect ...) dict -> (values (effect ...) dict)
  (define (convert-effect-list effect-list env)
    (for/fold ([new-effect-list '()] [intermediate-env env]) ([e effect-list])
      (define-values (new-effect new-env) (convert-effect e intermediate-env))
      (values (append new-effect-list (list new-effect)) new-env)))

  ;; effect dict -> (values effect dict)
  (define (convert-effect e env)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
       (values `(set! ,loc_1 (,binop ,loc_1 ,opand))
               (dict-set env loc_1 (convert-binop binop loc_1 opand env)))]
      [`(set! ,loc ,triv) (values `(set! ,loc ,triv) (dict-set env loc (convert-triv triv env)))]
      [`(begin
          ,effects ...)
       (define-values (new-effects new-env) (convert-effect-list effects env))
       (values `(begin
                  ,@new-effects)
               new-env)]
      [`(if ,pred ,effect1 ,effect2)
       (match-define-values (new-effect1 _) (convert-effect effect1 env))
       (match-define-values (new-effect2 _) (convert-effect effect2 env))
       (values (convert-if pred new-effect1 new-effect2 env) env)]))

  ;; binop loc triv dict -> symbol | integer
  (define (convert-binop binop loc opand env)
    (define interp-loc (convert-triv loc env))
    (define interp-opand (convert-triv opand env))
    (if (and (integer? interp-loc) (integer? interp-opand))
        ((binop->procedure binop) interp-loc interp-opand)
        (`(,binop ,loc ,opand))))

  (match p
    [`(module ,tail)
     (define env '())
     `(module ,(convert-tail tail env))]))

(module+ test
  (require rackunit)
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (> fv1 0) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (< fv1 0) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 2))))

  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (< fv1 ,(max-int 64)) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 2)
               (set! fv1 (+ fv1 3))
               (if (= fv1 5) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 2)
               (set! fv1 (+ fv1 3))
               (halt 1))))

  ;; Test >= relop equal
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (>= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))
  
  ;; Test >= relop greater than
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (>= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))
        
  ;; Test <= relop equal
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (<= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  ;; Test <= relop less than
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 0)
               (if (<= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 0)
               (halt 1))))

  ;; Test != relop
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (!= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 2))))
  
  ;; Test = relop
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (= fv1 1) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))
        
  ;; true
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (true) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  ;; false 
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (false) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 2))))

  ;; not false
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (not (false)) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  ;; not true
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (not (true)) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 2))))


  ;; double not
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (not (not (true))) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))
  
  (define if-begin-test 
    `(module 
        (begin
          (set! fv1 1)
          (if 
            (begin (set! fv0 1) (set! fv2 2) (< fv0 fv2))
            (halt 1)
            (halt 2)))))
  ;; Begin effects pred
  (check-equal?
    (optimize-predicates 
      if-begin-test)
    `(module 
        (begin
          (set! fv1 1)
          (begin 
            (set! fv0 1)
            (set! fv2 2) 
            (halt 1)))))
  (check-equal?
    (interp-nested-asm-lang-v4 if-begin-test)
    (interp-nested-asm-lang-v4 (optimize-predicates if-begin-test)))
  

  ;; check (if pred pred pred)
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (if (true) (true) (false)) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (halt 1))))

  ;; check (if pred pred pred)
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (if (if (true) (> rsp rax) (false)) (halt 1) (halt 2)))))
    `(module (begin
               (set! fv1 1)
               (if (> rsp rax) (halt 1) (halt 2)))))

  ;; check (set! loc (+ loc opand)) is identity
  (check-equal?
    (optimize-predicates `(module (begin
               (set! fv1 1)
               (set! fv1 (+ fv1 2))
               (halt fv1))))
    `(module (begin
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
      (optimize-predicates `(module (begin
               (begin
                (set! fv1 1)
                (if (true) (set! fv1 (+ fv1 2)) (set! fv1 0)))
               (halt fv1))))

      `(module (begin
               (begin
                (set! fv1 1)
                (set! fv1 (+ fv1 2)))
               (halt fv1))))

      (check-equal?
      (optimize-predicates `(module (begin
               (begin
                (set! fv1 1)
                (if (false) (set! fv1 (+ fv1 2)) (set! fv1 0)))
               (halt fv1))))

      `(module (begin
               (begin
                (set! fv1 1)
                (set! fv1 0))
               (halt fv1))))

)
