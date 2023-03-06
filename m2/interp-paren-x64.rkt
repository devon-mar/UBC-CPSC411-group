#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4

  "../m4/link-paren-x64.rkt")

(provide interp-paren-x64)

;; Milestone 4 Exercise 3
;;
;; Interpret the Paren-x64 v4 program p as a value, returning the final value of rax.
(define/contract (interp-paren-x64 p)
  (-> paren-x64-v4? int64?)

  (define relops '(< <= = >= > !=))

  ;; Dict of relop to result of last compare (boolean).
  (define flags (box '()))

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (trg? t) (int64? t)))

  (define/contract (trg? t)
    (-> any/c boolean?)
    (or (register? t)
        (natural? t)))


  (define (interp-paren-x64-p p)
    (match p
      [`(begin ,s ...)
        (eval-program '() 0 s)]))

  ;; pc is the index of statement s in the program.
  ;;
  ;; dict-of(loc -> int64) natural statement -> (values new-env new-pc)
  (define (interp-paren-x64-s env pc s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (values
          (dict-set env reg ((interp-paren-x64-binop binop) (dict-ref env reg) int32))
          (add1 pc))]
      [`(set! ,reg (,binop ,reg ,loc))
        (values
          (dict-set env reg ((interp-paren-x64-binop binop) (dict-ref env reg) (interp-paren-x64-loc env loc)))
          (add1 pc))]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (values
          (dict-set env reg (interp-paren-x64-triv env triv))
          (add1 pc))]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (values
          (dict-set env reg (interp-paren-x64-loc env loc))
          (add1 pc))]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (values
          (dict-set env addr int32)
          (add1 pc))]
      [`(set! ,addr ,trg)
        (values
          (dict-set env addr (interp-paren-x64-trg env trg))
          (add1 pc))]
      [`(jump ,trg)
        (values
          env
          (interp-paren-x64-trg env trg))]
      [`(compare ,reg ,opand)
        (define a (dict-ref env reg))
        (define b (interp-paren-x64-opand env opand))
        (set-box!
          flags
          (map
            (lambda (relop) (cons relop ((interp-paren-x64-relop relop) a b)))
            relops))
        (values env (add1 pc))]
      [`(jump-if ,relop ,pc-addr)
        (values
          env
          (if (dict-ref (unbox flags) relop)
            pc-addr
            (add1 pc)))]))

  ;; Evaluates t.
  ;; env: dict?
  ;; t: paren-x64-rt-trg
  ;; -> int64?
  (define/contract (interp-paren-x64-trg env t)
    (-> dict? any/c int64?)
    (match t
      [(? register?) (dict-ref env t)]
      [(? natural?) t]))

  ;; Evaluates t.
  ;; env: dict?
  ;; t: paren-x64-rt-triv
  ;; -> int64?
  (define/contract (interp-paren-x64-triv env t)
    (-> dict? any/c int64?)
    (match t
      [(? int64?) t]
      [(? trg?) (interp-paren-x64-trg env t)]))

  ;; Evaluates o.
  ;; env: dict?
  ;; t: paren-x64-rt-opand
  ;; -> int64?
  (define/contract (interp-paren-x64-opand env o)
    (-> dict? any/c int64?)
    (match o
      [(? int64?) o]
      [(? register?) (dict-ref env o)]))

  ;; Evaluates loc.
  ;; env: dict?
  ;; t: paren-x64-rt-loc
  ;; -> int64?
  (define/contract (interp-paren-x64-loc env l)
    (-> dict? any/c int64?)
    (match l
      [(? register?) (dict-ref env l)]
      ;; addr
      [_ (dict-ref env l)]))

  ;; Returns the corresponding procedure for b.
  (define/contract (interp-paren-x64-binop b)
    (-> symbol? procedure?)
    (match b
      ['* x64-mul]
      ['+ x64-add]))

  ;; Returns the corresponding procedure for relop.
  (define/contract (interp-paren-x64-relop relop)
    (-> symbol? procedure?)
    (match relop
      ['< <]
      ['<= <=]
      ['= =]
      ['>= >=]
      ['> >]
      ;; TODO: Is there a built in function??
      ['!= (lambda (x y) (not (= x y)))]))

  ;; dict-of(loc -> int64) Natural (listof statement) statement -> int64
  ;; Runs statement `s`, which is expected to be the `pc`th instruction of
  ;; `los`, modifying the environment and incrementing the program counter,
  ;; before executing the next instruction in `los`.
  (define/contract (eval-statement env pc los s)
    (-> dict? natural? list? any/c int64?)
    (define-values (new-env new-pc) (interp-paren-x64-s env pc s))
    (eval-program new-env new-pc los))

  ;; dict-of(loc -> int64) Natural (listof statements) -> int64
  ;; Runs the program represented by `los` starting from instruction number
  ;; indicated by the program counter `pc`, represented as a natural number.
  ;; Program is finished when `pc` reaches the final instruction of `los`.
  (define (eval-program env pc los)
    (-> dict? natural? list? int64?)
    (if (= pc (length los))
        (dict-ref env 'rax)
        (eval-statement env pc los (list-ref los pc))))

  (interp-paren-x64-p (link-paren-x64 p)))

(module+ test
  (require rackunit)

  (define/contract (fbp x)
    (-> dispoffset? any/c)
    `(,(current-frame-base-pointer-register) - ,x))

  (define (check-42 p)
    (check-equal?
      (interp-paren-x64 p)
      42))

  ;; (set! reg triv/int64)
  (check-42 '(begin (set! rax 42)))

	(check-42
    `(begin
       ;; (set! addr int32)
       (set! ,(fbp 0) 42)
       ;; (set! reg loc/addr)
       (set! rax ,(fbp 0))))
		

  (check-42
    `(begin
       (set! r8 42)
       ;; (set! reg loc/reg)
       ;; (set! reg triv/trg/reg)
       (set! r9 r8)
       ;; (set! addr trg/reg)
       (set! ,(fbp 0) r9)
       (set! rax ,(fbp 0))))

  (check-42
    `(begin
       (set! rax 0)
       ;; (set! reg_1 (binop reg_1 int32))
       (set! rax (+ rax 2))
       (set! r8 20)
       ;; (set! reg_1 (binop reg_1 loc/reg))
       (set! rax (+ rax r8))
       (set! ,(fbp 8) 20)
       ;; (set! reg_1 (binop reg_1 loc/addr))
       (set! rax (+ rax ,(fbp 8)))))

  ;; multiplication
  (check-42
    `(begin
       (set! rax 21)
       (set! rax (* rax 2))))

  (check-42
    '(begin
       (set! rax 0)
       ;; (jump trg/label)
       (jump L.a.1)
       (set! rax (+ rax 1))
       ;; (with-label label s)
       (with-label L.a.1 (set! rax (+ rax 42)))))

  (check-42
    '(begin
       (set! rax 0)
       ;; (set! reg triv/trg/label)
       (set! r8 L.a.1)
       ;; (jump trg/reg)
       (jump r8)
       (set! rax (+ rax 1))
       (with-label L.a.1 (set! rax (+ rax 42)))))

  (check-42
    `(begin
       (set! rax 0)
       ;; (set! addr trg/label)
       (set! ,(fbp 0) L.a.1)
       (set! r8 ,(fbp 0))
       (jump r8)
       (set! rax (+ rax 1))
       (with-label L.a.1 (set! rax (+ rax 42)))))

  (check-42
    `(begin
       (set! rax 0)
       (set! r8 20)
       ;; (compare reg opand/int64)
       (compare r8 20)
       ;; (jump-if relop/= label)/true
       (jump-if = L.a.1)
       (set! rax (+ rax 500))
       (with-label L.a.1 (set! rax (+ rax 41)))
       (set! r15 19)
       ;; (compare reg opand/reg)
       (compare r8 r15)
       ;; (jump-if relop/= label)/false
       (jump-if = L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (set! rax rax))))

  (check-42
    `(begin
       (set! rax 0)
       (set! r8 20)
       (compare r8 40)
       ;; (jump-if relop/< label)/true
       (jump-if < L.a.1)
       (set! rax (+ rax 500))
       (with-label L.a.1 (set! rax (+ rax 41)))
       (set! r15 10)
       (compare r8 r15)
       ;; (jump-if relop/< label)/false
       (jump-if < L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (set! rax rax))))

  (check-42
    `(begin
       (set! rax 0)
       (set! r8 20)
       (compare r8 20)
       ;; (jump-if relop/<= label)/true
       (jump-if <= L.a.1)
       (set! rax (+ rax 500))
       (with-label L.a.1 (set! rax (+ rax 41)))
       (set! r15 10)
       (compare r8 r15)
       ;; (jump-if relop/<= label)/false
       (jump-if <= L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (set! rax rax))))

  (check-42
    `(begin
       (set! rax 0)
       (set! r8 20)
       (compare r8 10)
       ;; (jump-if relop/>= label)/true
       (jump-if >= L.a.1)
       (set! rax (+ rax 500))
       (with-label L.a.1 (set! rax (+ rax 41)))
       (set! r15 21)
       (compare r8 r15)
       ;; (jump-if relop/>= label)/false
       (jump-if >= L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (set! rax rax))))

  (check-42
    `(begin
       (set! rax 0)
       (set! r8 20)
       (compare r8 10)
       ;; (jump-if relop/> label)/true
       (jump-if > L.a.1)
       (set! rax (+ rax 500))
       (with-label L.a.1 (set! rax (+ rax 41)))
       (set! r15 40)
       (compare r8 r15)
       ;; (jump-if relop/> label)/false
       (jump-if > L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (set! rax rax))))

  (check-42
    `(begin
       (set! rax 0)
       (set! r8 20)
       (compare r8 19)
       ;; (jump-if relop/!= label)/true
       (jump-if != L.a.1)
       (set! rax (+ rax 500))
       (with-label L.a.1 (set! rax (+ rax 41)))
       (set! r15 20)
       (compare r8 r15)
       ;; (jump-if relop/!= label)/false
       (jump-if != L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (set! rax rax))))

  )
