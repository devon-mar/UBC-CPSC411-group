#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide replace-locations)

;; Milestone 2 Exercise 8
;; Milestone 4 Exercise 12
;; Milestone 5 Exercise 11
;; Milestone 6 Exercise 14
;; Milestone 7 Exercise 7
;; Milestone 8 Exercise 11
;;
;; Compiles Asm-pred-lang v8/assignments to Nested-asm-lang-fvars v8 by
;; replacing all abstract location with physical locations using the
;; assignment described in the assignment info field.
(define/contract (replace-locations p)
  (-> asm-pred-lang-v8/assignments? nested-asm-lang-fvars-v8?)

  ;; asm-pred-lang-v8/assignments-p -> nested-asm-lang-fvars-v8-p
  (define (replace-locations-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
           ,@(map replace-locations-proc labels infos tails)
           ,(replace-locations-tail (info-ref info 'assignment) tail))]))

  ;; tail: asm-pred-lang-v8/assignments-tail
  ;; -> nested-asm-lang-fvars-v8-proc
  (define/contract (replace-locations-proc label info tail)
    (-> label? info? any/c any/c)
    `(define
       ,label
       ,(replace-locations-tail (info-ref info 'assignment) tail)))

  ;; assignment asm-pred-lang-v8/assignments-pred -> nested-asm-lang-fvars-v8-pred
  (define (replace-locations-pred as p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(replace-locations-pred as p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect as e)) es)
           ,(replace-locations-pred as p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(replace-locations-pred as p1)
           ,(replace-locations-pred as p2)
           ,(replace-locations-pred as p3))]
      [`(,r ,l ,o)
        `(,r
          ,(replace-locations-loc as l)
          ,(replace-locations-opand as o))]))

  ;; assignment asm-pred-lang-v8/assignments-tail -> nested-asm-lang-fvars-v8-tail
  (define (replace-locations-tail as t)
    (match t
      [`(jump ,trg ,_loc ...)
        `(jump ,(replace-locations-trg as trg))]
      [`(begin ,es ... ,t)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect as e)) es)
           ,(replace-locations-tail as t))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(replace-locations-pred as p)
           ,(replace-locations-tail as t1)
           ,(replace-locations-tail as t2))]))

  ;; assignment asm-pred-lang-v8/assignments-effect -> nested-asm-lang-fvars-v8-effect
  (define (replace-locations-effect as e)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,idx))
        `(set! ,(replace-locations-loc as loc1)
           (mref ,(replace-locations-loc as loc2)
                 ,(replace-locations-index as idx)))]
      [`(set! ,loc (,binop ,loc ,opand))
        `(set!
           ,(replace-locations-loc as loc)
           (,binop ,(replace-locations-loc as loc)
                   ,(replace-locations-opand as opand)))]
      [`(set! ,loc ,triv)
        `(set! ,(replace-locations-loc as loc) ,(replace-locations-triv as triv))]
      ;; modified template - removed tail effect
      [`(begin ,es ...)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect as e)) es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(replace-locations-pred as p)
           ,(replace-locations-effect as e1)
           ,(replace-locations-effect as e2))]
      [`(return-point ,label ,tail)
        `(return-point ,label ,(replace-locations-tail as tail))]
      [`(mset! ,loc ,index ,triv)
        `(mset!
           ,(replace-locations-loc as loc)
           ,(replace-locations-index as index)
           ,(replace-locations-triv as triv))]))

  ;; assignment asm-pred-lang-v8/assignments-index -> nested-asm-lang-fvars-v8-index
  (define (replace-locations-index as o)
    (match o
      [(? int64?) o]
      [_ (replace-locations-loc as o)]))

  ;; assignment asm-pred-lang-v8/assignments-opand -> nested-asm-lang-fvars-v8-opand
  (define (replace-locations-opand as o)
    (match o
      [(? int64?) o]
      [_ (replace-locations-loc as o)]))

  ;; assignment asm-pred-lang-v8/assignments-triv -> nested-asm-lang-fvars-v8-triv
  (define (replace-locations-triv as t)
    (match t
      [(? label?) t]
      [_ (replace-locations-opand as t)]))

  ;; assignment asm-pred-lang-v8/assignments-loc -> nested-asm-lang-fvars-v8-loc
  (define (replace-locations-loc as l)
    (-> info? any/c (or/c register? fvar?))
    (match l
      [(? aloc?) (info-ref as l)]
      [_ l]))

  ;; assignment asm-pred-lang-v8/assignments-trg -> nested-asm-lang-fvars-v8-trg
  (define (replace-locations-trg as t)
    (match t
      [(? label?) t]
      [_ (replace-locations-loc as t)]))

  ;; not used
  #;
  (define (replace-locations-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]
      ['bitwise-and (void)]
      ['bitwise-ior (void)]
      ['bitwise-xor (void)]
      ['arithmetic-shift-right (void)]))

  ;; not used
  #;
  (define (replace-locations-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (replace-locations-p p))

(module+ test
  (require rackunit)

  ;; tail/(jump trg loc ...)
  ;; tail/(begin effect ... tail)
  (define-syntax-rule (check-42 as es)
    (check-42p
      `(module
         ((assignment ((tmp-ra.1 r15) ,@as)))
         (begin
           (set! tmp-ra.1 r15)
           (begin
             ,@es
             (jump tmp-ra.1 rbp rax))))))

  ;; as: (listof (list/c aloc? rloc"))
  ;; es: (listof effect)
  (define-check (check-42p p)
    (check-equal?
      (interp-nested-asm-lang-fvars-v8 (replace-locations p))
      42))

  (define rax (current-return-value-register))

  (check-42
    `((x.1 ,rax))
    `((set! x.1 42)))

  (check-42
    `((x.1 ,rax) (a.1 fv0) (b.1 r8) (c.1 r9))
    `((set! b.1 10)
      ;; effect/(set! loc triv)
      (set! a.1 b.1)
      (set! a.1 10)
      (set! c.1 10)
      ;; effect/(if pred effect effect)
      ;; pred/(relop loc opand)
      (if (= a.1 10)
          (set! x.1 42)
          (set! x.1 0))))

  (check-42
    `((a.1 r8) (b.1 fv0))
    `((set! a.1 5)
      (set! b.1 5)
      ;; pred/(true)
      ;; pred/(false)
      ;; pred/(not pred)
      ;; pred/(if pred pred pred)
      (if (if (= a.1 b.1) (if (= a.1 b.1) (true) (false)) (if (not (!= a.1 b.1)) (true) (false)))
        (set! ,rax 42)
        (set! ,rax 0))))

  (check-42
    `((a.1 r8) (b.1 r9))
    ;; pred/(begin effect ... pred)
    `((set! a.1 5)
      (set! b.1 5)
      ;; effect/(set! loc_1 (binop loc_1 opand))
      (if (begin (set! a.1 (+ a.1 b.1)) (set! b.1 10) (= a.1 b.1))
        ;; effect/(begin effect ... effect)
        (begin
          (set! ,rax 7)
          (set! ,rax (* ,rax 6)))
        (set! ,rax 0))))

  #;
  (parameterize ([current-parameter-registers '()])
    (pretty-display
     ((compose
       assign-frame-variables
       assign-registers
       allocate-frames
       assign-call-undead-variables
       conflict-analysis
       undead-analysis
       uncover-locals
       select-instructions
       impose-calling-conventions
       normalize-bind
       sequentialize-let
       uniquify)
      '(module
         (define foo (lambda () 50))
         (let ([x (call foo)])
           (if (= 1 2)
             0
             (- x 8)))))))

  (check-42p
    '(module
       ((locals ())
        (conflicts
         ((tmp.4 (rbp tmp-ra.3 x.1))
          (tmp-ra.3 (x.1 rbp tmp.4 rax))
          (x.1 (rbp tmp-ra.3 tmp.4))
          (rax (rbp tmp-ra.3))
          (rbp (x.1 r15 tmp-ra.3 tmp.4 rax))
          (r15 (rbp))))
        (assignment ((tmp-ra.3 fv0) (tmp.4 r15) (x.1 r14))))
       (define L.foo.1
         ((locals ())
          (conflicts
           ((tmp-ra.2 (rax rbp)) (rbp (rax tmp-ra.2)) (rax (rbp tmp-ra.2))))
          (assignment ((tmp-ra.2 r15))))
         (begin (set! tmp-ra.2 r15) (set! rax 50) (jump tmp-ra.2 rbp rax)))
       (begin
         (set! tmp-ra.3 r15)
         (begin
           (set! rbp (- rbp 8))
           ;; effect/(return-point label tail)
           (return-point L.rp.2 (begin (set! r15 L.rp.2) (jump L.foo.1 rbp r15)))
           (set! rbp (+ rbp 8)))
         (set! x.1 rax)
         ;; tail/(if pred tail tail)
         (if (begin (set! tmp.4 1) (= tmp.4 2))
           (begin (set! rax 0) (jump tmp-ra.3 rbp rax))
           (begin (set! rax x.1) (set! rax (- rax 8)) (jump tmp-ra.3 rbp rax))))))

  ;; M8 test
  (check-42p
    `(module
       ((assignment ((x.1 rsi) (y.1 rax) (idx.1 rcx) (val.1 r13))))
       (begin
         (set! val.1 42)
         (set! idx.1 0)
         (set! x.1 ,(current-heap-base-pointer-register))
         (mset! x.1 0 0)
         (mset! x.1 idx.1 val.1)
         (set! y.1 (mref x.1 idx.1))
         (jump ,(current-return-address-register) ,(current-frame-base-pointer-register) ,(current-return-value-register)))))
  )
