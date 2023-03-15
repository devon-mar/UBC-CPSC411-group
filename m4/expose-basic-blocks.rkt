#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide expose-basic-blocks)

;; Milestone 4 Exercise 10
;; Milestone 5 Exercise 13
;;
;; Compile the nested-asm-lang-v5 to block-pred-lang-v5, eliminating all
;; nested expressions by generating fresh basic blocks and jumps.
(define/contract (expose-basic-blocks p)
  (-> nested-asm-lang-v5? block-pred-lang-v5?)

  (define blocks (box '()))

  ;; Don't need this - make-begin in cpsc411lib
  ;;
  ;; If tail is not a begin, introduce a begin
  ;; with effect and tail. Otherwise add effect
  ;; as the first effect in tail.
  ;;
  ;; A begin will always be returned.
  ;;
  ;; block-pred-lang-v5-effect block-pred-lang-v5-tail -> block-pred-lang-v5-tail
  #;
  (define (cons-effect-tail effect tail)
    (match tail
      [`(begin ,_ ... ,_)
        ;; TODO is this faster than just using ` with @??
        ;; ` is statically compiled to cons
        ;; so it may not be slower.
        #;
        `(begin
           ,effect
           ,@tail)
        (cons
          'begin
          (cons
            effect
            (cdr tail)))]
      [_ `(begin ,effect ,tail)]))


  ;; Adds a basic block with the the label (fresh-label label) and
  ;; tail to the mutable state. Returns a block-pred-lang-v5 jump
  ;; instruction to the new label.
  ;;
  ;; label: symbol
  ;; tail: block-pred-lang-v5-tail
  ;; -> block-pred-lang-v5-tail
  (define/contract (add-block! label tail)
    (-> symbol? any/c any/c)
    (define actual-label (fresh-label label))
    (set-box!
      blocks
      (cons
        `(define ,actual-label ,tail)
        (unbox blocks)))
    `(jump ,actual-label))

  (define (expose-basic-blocks-p p)
    (match p
      [`(module ,block-list ... ,tail)
        `(module
           (define
             ,(fresh-label 'main)
             ,(expose-basic-blocks-tail tail))
           ,@(expose-basic-blocks-asm-lang-block-list block-list)
           ,@(unbox blocks))]))


  ;; Note: By Piazza @137 @92, we may assume labels in the provided program  will not be generated
  ;; by fresh-labels and so we do not need to reinvoke fresh-label.
  ;; ((define label nested-asm-lang-v5-tail) ...) -> ((define label block-pred-lang-v5-tail) ...)
  (define (expose-basic-blocks-asm-lang-block block)
    (match block
      [`(define ,label ,tail)
        `(define ,label ,(expose-basic-blocks-tail tail))]))

  ;; ((define label nested-asm-lang-v5-tail) ...) -> ((define label block-pred-lang-v5-tail) ...)
  (define (expose-basic-blocks-asm-lang-block-list block-list)
    (map expose-basic-blocks-asm-lang-block block-list))

  ;; nested-asm-lang-v5-pred nested-asm-lang-v5-tail nested-asm-lang-v5-tail -> block-pred-lang-v5-tail
  (define (expose-basic-blocks-pred p tail1 tail2)
    (match p
      [`(true)
        `(if ,p
           ,tail1
           ,tail2)]
      [`(false)
        `(if ,p
           ,tail1
           ,tail2)]
      [`(not ,pred)
        (expose-basic-blocks-pred pred tail2 tail1)]
      [`(begin ,effects ... ,pred)
        (expose-basic-blocks-effects
          effects
          (expose-basic-blocks-pred
            pred
            tail1
            tail2))]
      [`(if ,p1 ,p2 ,p3)
        (expose-basic-blocks-pred
          p1
          (add-block! 'pt (expose-basic-blocks-pred p2 tail1 tail2))
          (add-block! 'pf (expose-basic-blocks-pred p3 tail1 tail2)))]
      [`(,_relop ,_loc ,_triv)
        `(if ,p
           ,tail1
           ,tail2)]))

  ;; nested-asm-lang-v5-tail -> block-pred-lang-v5-tail
  (define (expose-basic-blocks-tail t)
    (match t
      [`(halt ,_triv)
        t]
      [`(jump ,trg)
        ;; As stated in note above and By Piazza @137 @92, we assume trg label will be fresh and unused
        t]
      [`(begin ,effects ... ,tail)
        (expose-basic-blocks-effects
          effects
          (expose-basic-blocks-tail tail))]
      [`(if ,pred ,t1 ,t2)
        (expose-basic-blocks-pred
          pred
          (add-block! 't (expose-basic-blocks-tail t1))
          (add-block! 'f (expose-basic-blocks-tail t2)))]))

  ;; nested-asm-lang-v5-effect nested-asm-lang-v5-tail -> block-pred-lang-v5-tail
  (define (expose-basic-blocks-effect e tail)
    (match e
      [`(set! ,_ (,_ ,_ ,_))
        (make-begin (list e) tail)]
      [`(set! ,_ ,_)
        (make-begin (list e) tail)]
      [`(begin ,effects ... ,effect)
        (for/foldr ([acc-tail (expose-basic-blocks-effect effect tail)])
                   ([e effects])
          (expose-basic-blocks-effect e acc-tail))]
      [`(if ,pred ,e1 ,e2)
        (define jump-join (add-block! 'j tail))
        (expose-basic-blocks-pred
          pred
          (add-block!
            'et
            (expose-basic-blocks-effect e1 jump-join))
          (add-block!
            'ef
            (expose-basic-blocks-effect e2 jump-join)))]))

  ;; (listof nested-asm-lang-v5-effect) nested-asm-lang-v5-tail
  ;; -> block-pred-lang-v5-tail
  (define (expose-basic-blocks-effects effects tail)
    (match effects
      ['() tail]
      [(cons effect effects)
       (expose-basic-blocks-effect
         effect
         (expose-basic-blocks-effects effects tail))]))

  #;
  (define (expose-basic-blocks-triv t)
    (match t
      [(? int64?)
       (void)]
      [loc
        (void)]))

  #;
  (define (expose-basic-blocks-loc l)
    (match l
      [(? register?)
       (void)]
      [(? fvar?)
       (void)]))

  (expose-basic-blocks-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-block-pred-lang-v5 (expose-basic-blocks p))
      42))

  ;; single halt
  (check-42
    '(module (halt 42)))


  ;; tail/if
  (check-42
    '(module
       (if (true)
         (halt 42)
         (halt 0))))

  ;; pred/if
  (check-42
    '(module
       (if (if (true) (false) (true))
         (halt 0)
         (halt 42))))
  (check-42
    '(module
       (if (if (false) (false) (true))
         (halt 42)
         (halt 0))))

  ;; pred/not
  (check-42
    '(module
       (if (not (if (false) (false) (true)))
         (halt 0)
         (halt 42))))

  ;; 0 effects in begin
  (check-42
    '(module
       (begin
         (halt 42))))

  ;; tail/if nested
  (check-42
    '(module
       (if (true)
         (if (false)
           (halt 1)
           (halt 42))
         (halt 0))))

  ;; some effects with no ifs
  (check-42
    '(module
       (begin
         (set! r8 0)
         (set! r9 21)
         (set! r8 (+ r8 r9))
         (set! r12 2)
         (set! r8 (* r8 r12))
         (halt r8))))

  ;; nested effect/begin
  (check-42
    '(module
       (begin
         (begin (set! r8 42))
         (halt r8))))
  ;; nested effect/begin 2
  (check-42
    '(module
       (begin
         (begin (begin (begin (set! r8 42))))
         (halt r8))))

  ;; same thing as above but with nested begins
  (check-42
    '(module
       (begin
         (set! r8 0)
         (begin
           (set! r9 21)
           (begin
             (begin (set! r8 (+ r8 r9)))))
         (set! r12 2)
         (set! r8 (* r8 r12))
         (halt r8))))

  ;; pred/begin
  (check-42
    '(module
       (if (begin (begin (set! r8 0) (= r8 0)))
         (halt 42)
         (halt 0))))

  ;; effect/if
  (check-42
    '(module
       (begin
         (if (true)
           (set! r8 42)
           (set! r8 41))
         (halt r8))))

  ;; iiiiiffffssss
  (check-42
    '(module
       (begin
         (begin
           (if (if (true) (true) (false))
             (if (true) (set! r8 42) (set! r8 40))
             (set! r8 41)))
         (if (if (true) (false) (true)) (halt 2) (halt r8)))))

  ;; effect/begin multiple where
  ;; order of effects matters
  (check-42
    '(module
       (begin
         (begin
           (set! r8 10)
           (set! r9 r8)
           (set! rsi 2)
           (set! rsi (* rsi 16))
           (set! r9 (+ r9 rsi)))
         (set! rcx r9)
         (halt rcx))))

  ;; Check blocks are placed correct order. Tail is evaluated first
  (check-42
    '(module (define L.main.1 (halt 2)) (halt 42)))

  ;; Check correct handling of multiple blocks defined in p with jumps
  (check-42
    '(module
      (define L.first.1
        (begin 
          (set! rax 42)
          (jump L.second.1)))
      (define L.second.1
        (begin
          (set! rsp rax)
          (halt rsp)))
      (jump L.first.1)))
    
  ;; Block ordering doesn't matter
  (check-42
    '(module
      (define L.second.1
          (begin
            (set! rsp rax)
            (halt rsp)))
      (define L.first.1
        (begin 
          (set! rax 42)
          (jump L.second.1)))
      (jump L.first.1)))


  ;; Pred with jump as tail
  (check-42
    '(module
      (define L.second.1
            (halt 42))
      (define L.first.1
        (begin 
          (halt 2)))
      (if (true) (jump L.second.1) (jump L.first.1))))

  ;; pred with complex tail blocks
  (check-42
    '(module
      (define L.haltrax.1
            (halt rax))
      (if (true) (begin (set! rax 42) (jump L.haltrax.1)) (begin (set! rax 2) (jump L.haltrax.1)))))

  ;; Ensure nested blocks inside asm-lang blocks are unboxed
  (check-42
    '(module
      (define L.haltrax.1
        (halt rax))
      (define L.nested.1
        (if (true) (begin (set! rax 42) (jump L.haltrax.1)) (begin (set! rax 2) (jump L.haltrax.1))))
      (jump L.nested.1)))
      
  ;; Case where jump trg is loc and set! loc label
  (check-42
    '(module
      (define L.haltrax.1
        (halt rax))
      (begin
        (set! rax 42)
        (set! rsp L.haltrax.1)
        (jump rsp)))))

