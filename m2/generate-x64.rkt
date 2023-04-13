#lang racket
(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide generate-x64)

;; Milestone 2 Exercise 12
;; Milestone 4 Exercise 1
;; Milestone 6 Exercise 19
;; Milestone 7 Exercise 9
;; Milestone 7 Exercise 14
;;
;; Compile the Paren-x64 v8 program into a valid sequence of x64 instructions,
;; represented as a string.
(define/contract (generate-x64 p)
  (-> paren-x64-v8? string?)

  (define/contract (trg? t)
    (-> any/c boolean?)
    (or (register? t) (label? t)))

  (define/contract (triv? t)
    (-> any/c boolean?)
    (or (trg? t) (int64? t)))

  ;; paren-x64-v8-p -> string?
  (define (generate-x64-p p)
    (match p
      [`(begin ,s ...)
        (foldr
          (lambda (s acc) (string-append s "\n" acc))
          ""
          (map generate-x64-s s))]))

  ;; paren-x64-v8-s -> string?
  (define/contract (generate-x64-s s)
    (-> any/c string?)
    (match s
      [`(set! ,reg (,b ,reg ,int32))
        #:when (int32? int32)
        (format "~a ~a, ~a" (binop->ins b) reg int32)]
      [`(set! ,reg (,b ,reg ,loc))
        (format "~a ~a, ~a" (binop->ins b) reg (generate-x64-loc loc))]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (format "mov ~a, ~a" reg (generate-x64-triv triv))]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (format "mov ~a, ~a" reg (generate-x64-loc loc))]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (format "mov ~a, ~a" (addr->x64 addr) int32)]
      [`(set! ,addr ,trg)
        (format "mov ~a, ~a" (addr->x64 addr) (generate-x64-trg trg))]
      [`(with-label ,label ,s)
        (format "~a:~n~a" (sanitize-label label) (generate-x64-s s))]
      [`(jump ,trg)
        (format "jmp ~a" (generate-x64-trg trg))]
      [`(compare ,reg ,opand)
        (format "cmp ~a, ~a" reg opand)]
      [`(jump-if ,relop ,label)
        (format "~a ~a" (relop->x64-jmp relop) (sanitize-label label))]))

  ;; paren-x64-v8-trg -> string
  ;; If the trg is a label, santize it. Otherwise return t
  (define (generate-x64-trg t)
    (match t
      [(? register?)
        t]
      [(? label?)
       (sanitize-label t)]))

  ;; paren-x64-v8-triv -> string
  (define (generate-x64-triv t)
    (match t
      [(? int64?)
       t]
      [_ 
        (generate-x64-trg t)]))

  ;; not used
  #;
  (define (generate-x64-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? register?)
       (void)]))

  ;; paren-x64-v8-loc -> string?
  (define/contract (generate-x64-loc l)
    (-> any/c string?)
    (match l
      [(? register?) (symbol->string l)]
      [_ (addr->x64 l)]))

  ;; Returns the displacement mode operand for the given addr
  ;;
  ;; paren-x64-v8-addr -> x64 displacement mode operand string
  (define (addr->x64 a)
    (match a
      [`(,fbp - ,dispoffset)
        (format "QWORD [~a - ~a]" fbp dispoffset)]
      [`(,reg + ,int32)
        #:when (and (register? reg) (int32? int32))
        (format "QWORD [~a + ~a]" reg int32)]
      [`(,r1 + ,r2)
        #:when (and (register? r1) (register? r2))
        (format "QWORD [~a + ~a]" r1 r2)]))

  ;; paren-x64-v8-binop -> string?
  (define/contract (binop->ins b)
    (-> symbol? string?)
    (match b
      ['* "imul"]
      ['+ "add"]
      ['- "sub"]
      ['bitwise-and "and"]
 	 	  ['bitwise-ior "or"]
 	 	  ['bitwise-xor "xor"]
 	 	  ['arithmetic-shift-right "sar"]))

  ;; Returns the appropriate x64 jump instructions for the relop r.
  ;;
  ;; paren-x64-v8-relop -> string?
  (define/contract (relop->x64-jmp r)
    (-> symbol? string?)
    (match r
      ['< "jl"]
      ['<= "jle"]
      ['= "je"]
      ['>= "jge"]
      ['> "jg"]
      ['!= "jne"]))

  (generate-x64-p p))

(module+ test
  (require
    rackunit
    cpsc411/ptr-run-time)

  (define rax (current-return-value-register))
  (define hbp (current-heap-base-pointer-register))

  (define/contract (fixnum i)
    (-> int61? int64?)
    (bitwise-ior (arithmetic-shift i (current-fixnum-shift)) (current-fixnum-tag)))

  (define fn42 (fixnum 42))

  (current-pass-list
    (list
      generate-x64
      wrap-x64-run-time
      wrap-x64-boilerplate))

  (define-check (check-42 p)
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
       (set! ,(fbp 0) ,fn42)
       ;; (set! reg loc/addr)
       (set! ,rax ,(fbp 0))))

  (check-42
    `(begin
       (set! r11 ,fn42)
       ;; (set! reg loc/reg)
       ;; (set! reg triv/trg/reg) -- unreachable
       (set! r10 r11)
       ;; (set! addr trg/reg)
       (set! ,(fbp 8) r10)
       (set! ,rax ,(fbp 8))))

  (check-42
    `(begin
       ;; (set! addr trg/label)
       (set! ,(fbp 0) L.a.1)
       (set! r9 ,(fbp 0))
       ;; (jump trg/reg)
       (jump r9)
       (with-label L.b.1 (set! rax ,(fixnum 40)))
       ;; (jump trg/label)
       (jump L.c.1)
       ;; (with-label label s)
       ;; (set! reg triv/trg/label)
       (with-label L.a.1 (set! r9 L.b.1))
       (jump L.b.1)
       ;; (set! reg_1 (binop/+ reg_1 int32))
       (with-label L.c.1 (set! ,rax (+ ,rax ,(fixnum 2))))))

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
       (set! r13 ,(fixnum 2))
       (set! ,(fbp 0) ,(fixnum 2))
       (set! ,(fbp 8) ,(fixnum 3))
       (set! r9 ,(fixnum 2))
       ;; (set! reg_1 (binop/+ reg_1 loc/addr))
       (set! r9 (+ r9 ,(fbp 8)))
       ;; (set! reg_1 (binop/* reg_1 loc/addr))
       (set! r9 (* r9 ,(fbp 0)))
       (set! ,rax ,(fixnum 8))
       ;; (set! reg_1 (binop/* reg_1 loc/reg))
       (set! ,rax (* ,rax r13))
       ;; (set! reg_1 (binop/* reg_1 int32))
       (set! ,rax (* ,rax 2))
       ;; (set! reg_1 (binop/+ reg_1 loc/reg))
       (set! ,rax (+ ,rax r9))
       ;; I didn't bother shifting for other things...
       ;; and this seems to work.
       (set! ,rax (arithmetic-shift-right rax 3))))

  (check-42
    `(begin
       (set! rax ,(fixnum 0))
       (set! r12 ,(fixnum 2))
       (set! r9 ,(fixnum 4))
       ;; (compare reg opand/int64)
       (compare r9 ,(fixnum 4))
       ;; (jump-if relop/= label)
       (jump-if = L.a.1)
       (set! rax (+ rax ,(fixnum 1)))
       ;; (compare reg opand/reg)
       (with-label L.a.1 (compare r9 r12))
       ;; (jump-if relop/> label)
       (jump-if > L.b.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.b.1 (compare r9 r12))
       (jump-if != L.c.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.c.1 (compare r12 r9))
       ;; (jump-if relop/< label)
       (jump-if < L.d.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.d.1 (set! ,rax (+ ,rax ,fn42)))))

  (check-42
    `(begin
       (set! rax ,(fixnum 21))
       (set! r9 ,(fixnum 4))
       (set! r12 ,(fixnum 2))
       (compare r12 r9)
       ;; (jump-if relop/<= label)
       (jump-if <= L.a.1)
       (set! rax (+ rax 1))
       (with-label L.a.1 (compare r9 ,(fixnum 4)))
       (jump-if <= L.b.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.b.1 (compare r9 r12))
       ;; (jump-if relop/!= label)
       (jump-if != L.c.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.c.1 (compare r9 r12))
       ;; (jump-if relop/>= label)
       (jump-if >= L.d.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.d.1 (compare r9 ,(fixnum 4)))
       (jump-if >= L.e.1)
       (set! rax (+ rax ,(fixnum 1)))
       (with-label L.e.1 (set! ,rax (* ,rax 2)))))


  ;; M2 tests

  ; no statements
  (check-equal?
    (generate-x64 '(begin))
    "")

  ; simplest program
  (check-42
    `(begin (set! ,rax ,fn42)))

  ; register to register move
  (check-42
    `(begin
       (set! rsi ,fn42)
       (set! ,rax rsi)))

  ; register to addr
  (check-42
    `(begin
       (set! rsi ,fn42)
       (set! ,(fbp 0) rsi)
       (set! ,rax ,(fbp 0))))

  ; addr <- int32
  ; reg <- addr
  (check-42
    `(begin
       (set! ,(fbp 0) ,fn42)
       (set! ,rax ,(fbp 0))))

  ; binop, lhs int32
  (check-42
    `(begin
       (set! ,rax ,(fixnum 40))
       (set! ,rax (+ ,rax ,(fixnum 2)))))
  ; binop, lhs loc(reg)
  (check-42
    `(begin
       (set! rsi ,(fixnum 2))
       (set! ,rax ,(fixnum 40))
       (set! ,rax (+ ,rax rsi))))

  ; binop mul
  (check-42
    `(begin
       (set! ,rax ,(fixnum 21))
       (set! ,rax (* ,rax ,2))))

  ;; M6 test
  ; binop sub
  (check-42
    `(begin
       (set! ,rax ,(fixnum 50))
       (set! ,rax (- ,rax ,(fixnum 8)))))

  ;; M7 test
  ;; Test xor
  (check-42
    `(begin
       (set! ,rax ,(fixnum 50))
       (set! ,rax (bitwise-xor rax rax))
       (set! ,rax (+ ,rax ,fn42))))
  
  ;; Test arithmetic-shift-right
  (check-42
    `(begin
       (set! ,rax ,(fixnum 32))
       (set! ,rax (* ,rax 32))
       (set! ,rax (arithmetic-shift-right ,rax 5))
       (set! ,rax (+ ,rax ,(fixnum 10)))))
  
  ;; Test ior
  (check-42
    `(begin
       (set! ,rax ,(fixnum 10))
       (set! ,rax (bitwise-ior ,rax ,(fixnum 32)))))

  ;; Test and
  (define 32-ones (max-int 32))
  (check-42
    `(begin
       (set! ,rax ,(fixnum 42))
       (set! ,rax (bitwise-and ,rax ,32-ones))))

  ;; Should not throw error if the label is correctly formatted
  (check-42
    `(begin
       (with-label L.$!!@#*main.2 (set! rbp L.$!!@#*main.2))
       (set! ,rax ,(fixnum 42))))

  ;; M8 tests
  (check-42
    `(begin
       (set! (,hbp + 8) ,(fixnum 42))
       (set! ,rax (,hbp + 8))))
  (check-42
    `(begin
       (set! r8 8)
       (set! (,hbp + r8) ,(fixnum 42))
       (set! ,rax (,hbp + r8))))
)
