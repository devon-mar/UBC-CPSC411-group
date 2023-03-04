#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

;; Milestone 2 Exercise 12
;; Milestone 4 Exercise 1
;;
;; Compile the Paren-x64 v4 program into a valid sequence of x64 instructions,
;; represented as a string.
(define/contract (generate-x64 p)
  (-> paren-x64-v4? string?)

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (trg? t) (int64? t)))

  (define/contract (trg? t)
    (-> any/c boolean?)
    (or (register? t)
        (label? t)))

  ;; Returns the appropriate x64 instruction name for the binop b.
  (define/contract (binop->ins b)
    (-> symbol? string?)
    (match b
      ['* "imul"]
      ['+ "add"]))


  ;; paren-x64-v4-p -> string?
  (define (generate-x64-p p)
    (match p
      [`(begin ,s ...)
        (foldr
          (lambda (s acc) (string-append s "\n" acc))
          ""
          (map generate-x64-s s))]))

  ;; paren-x64-v4-s -> string?
  (define (generate-x64-s s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (format "~a ~a, ~a" (binop->ins binop) reg int32)]
      [`(set! ,reg (,binop ,reg ,loc))
        (format "~a ~a, ~a" (binop->ins binop) reg (generate-x64-loc loc))]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (format "mov ~a, ~a" reg triv)]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (format "mov ~a, ~a" reg (generate-x64-loc loc))]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (format "mov ~a, ~a" (addr->x64 addr) int32)]
      [`(set! ,addr ,trg)
        (format "mov ~a, ~a" (addr->x64 addr) trg)]
      [`(with-label ,label ,s)
        (format "~a:~n~a" label (generate-x64-s s))]
      [`(jump ,trg)
        (format "jmp ~a" trg)]
      [`(compare ,reg ,opand)
        (format "cmp ~a, ~a" reg opand)]
      [`(jump-if ,relop ,label)
        (format "~a ~a" (relop->x64-jmp relop) label)]))


  #;
  (define (generate-x64-triv t)
    (match t
      [(? trg?) (void)]
      [(? int64?) (void)]))

  #;
  (define (generate-x64-opand o)
    (match o
      [(? int64?) (void)]
      [(? register?) (void)]))

  ;; Returns the appropriate x64 jump
  ;; instructions for the relop r.
  ;;
  ;; paren-x64-v4-relop -> string?
  (define/contract (relop->x64-jmp r)
    (-> symbol? string)
    (match r
      ['< "jl"]
      ['<= "jle"]
      ['= "je"]
      ['>= "jge"]
      ['> "jg"]
      ['!= "jne"]))

  ;; Returns the displacement mode operand
  ;; for the given addr
  ;;
  ;; Displacement mode operand -> x64 displacement mode operand string
  (define (addr->x64 a)
    (match a
      [`(,fbp - ,dispoffset)
        (format "QWORD [~a - ~a]" fbp dispoffset)]))

  ;; Returns the appropriate register or displacement mode operand for loc.
  ;;
  ;; paren-x64-v4-loc -> string? or register?
  (define (generate-x64-loc l)
    (match l
      [(? register?) l]
      ;; addr
      [_ (addr->x64 l)]))

  (generate-x64-p p))

(module+ test
  (require
    rackunit
    cpsc411/2c-run-time)

  (current-pass-list
    (list
      generate-x64
      wrap-x64-run-time
      wrap-x64-boilerplate))

  (define (check-42 p)
    (check-equal?
      (execute p)
      42))

  (define/contract (fbp x)
    (-> dispoffset? any/c)
    `(,(current-frame-base-pointer-register) - ,x))

  ;; New M3 tests:

  (check-42
    `(begin
       ;; (set! addr int32)
       (set! ,(fbp 0) 42)
       ;; (set! reg loc/addr)
       (set! rax ,(fbp 0))))

  (check-42
    `(begin
       (set! r11 42)
       ;; (set! reg loc/reg)
       ;; (set! reg triv/trg/reg) -- unreachable
       (set! r10 r11)
       ;; (set! addr trg/reg)
       (set! ,(fbp 8) r10)
       (set! rax ,(fbp 8))))


  (check-42
    `(begin
       ;; (set! addr trg/label)
       (set! ,(fbp 0) L.a.1)
       (set! r9 ,(fbp 0))
       ;; (jump trg/reg)
       (jump r9)
       (with-label L.b.1 (set! rax 40))
       ;; (jump trg/label)
       (jump L.c.1)
       ;; (with-label label s)
       ;; (set! reg triv/trg/label)
       (with-label L.a.1 (set! r9 L.b.1))
       (jump L.b.1)
       ;; (set! reg_1 (binop/+ reg_1 int32))
       (with-label L.c.1 (set! rax (+ rax 2)))))

  ;; @258
  #;
  (define big-num (add1 (max-int 32)))
  #;
  (check-42
    `(begin
       (set! rax 40)
       ;; (set! reg triv/int64)
       (set! rsi ,big-num)
       ;; (compare reg opand/int64)
       (compare rsi ,big-num)
       ;; (jump-if relop/= label)
       (jump-if = L.a.1)
       (set! rax (+ rax 1))
       (with-label L.a.1 (set! rax (+ rax 2)))))

  (check-42
    `(begin
       (set! r13 2)
       (set! ,(fbp 0) 2)
       (set! ,(fbp 8) 3)
       (set! r9 2)
       ;; (set! reg_1 (binop/+ reg_1 loc/addr))
       (set! r9 (+ r9 ,(fbp 8)))
       ;; (set! reg_1 (binop/* reg_1 loc/addr))
       (set! r9 (* r9 ,(fbp 0)))
       (set! rax 8)
       ;; (set! reg_1 (binop/* reg_1 loc/reg))
       (set! rax (* rax r13))
       ;; (set! reg_1 (binop/* reg_1 int32))
       (set! rax (* rax 2))
       ;; (set! reg_1 (binop/+ reg_1 loc/reg))
       (set! rax (+ rax r9))))

  (check-42
    '(begin
       (set! rax 0)
       (set! r12 2)
       (set! r9 4)
       ;; (compare reg opand/int64)
       (compare r9 4)
       ;; (jump-if relop/= label)
       (jump-if = L.a.1)
       (set! rax (+ rax 1))
       ;; (compare reg opand/reg)
       (with-label L.a.1 (compare r9 r12))
       ;; (jump-if relop/> label)
       (jump-if > L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (compare r9 r12))
       (jump-if != L.c.1)
       (set! rax (+ rax 1))
       (with-label L.c.1 (compare r12 r9))
       ;; (jump-if relop/< label)
       (jump-if < L.d.1)
       (set! rax (+ rax 1))
       (with-label L.d.1 (set! rax (+ rax 42)))))

  (check-42
    '(begin
       (set! rax 21)
       (set! r9 4)
       (set! r12 2)
       (compare r12 r9)
       ;; (jump-if relop/<= label)
       (jump-if <= L.a.1)
       (set! rax (+ rax 1))
       (with-label L.a.1 (compare r9 4))
       (jump-if <= L.b.1)
       (set! rax (+ rax 1))
       (with-label L.b.1 (compare r9 r12))
       ;; (jump-if relop/!= label)
       (jump-if != L.c.1)
       (set! rax (+ rax 1))
       (with-label L.c.1 (compare r9 r12))
       ;; (jump-if relop/>= label)
       (jump-if >= L.d.1)
       (set! rax (+ rax 1))
       (with-label L.d.1 (compare r9 4))
       (jump-if >= L.e.1)
       (set! rax (+ rax 1))
       (with-label L.e.1 (set! rax (* rax 2)))))


  ;; M2 tests

  ; no statements
  (check-equal?
    (generate-x64 '(begin))
    "")

  ; simplest program
  (check-42
    '(begin (set! rax 42)))

  ; register to register move
  (check-42
    '(begin
       (set! rsi 42)
       (set! rax rsi)))

  ; register to addr
  (check-42
    '(begin
       (set! rsi 42)
       (set! (rbp - 0) rsi)
       (set! rax (rbp - 0))))

  ; addr <- int32
  ; reg <- addr
  (check-42
    '(begin
       (set! (rbp - 0) 42)
       (set! rax (rbp - 0))))

  ; binop, lhs int32
  (check-42
    '(begin
       (set! rax 40)
       (set! rax (+ rax 2))))
  ; binop, lhs loc(reg)
  (check-42
    '(begin
       (set! rsi 2)
       (set! rax 40)
       (set! rax (+ rax rsi))))

  ; binop mul
  (check-42
    '(begin
       (set! rax 21)
       (set! rax (* rax 2))))
  )
