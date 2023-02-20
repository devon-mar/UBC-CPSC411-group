#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v2)

(provide generate-x64)

;; Milestone 2 Exercise 12
;;
;; Compile the Paren-x64 v2 program into a valid sequence of x64 instructions,
;; represented as a string.
(define/contract (generate-x64 p)
  (-> paren-x64-v2? string?)

  ;; Returns true if t is a triv.
  (define (triv? t)
    (-> any/c boolean?)
    (or (register? t) (int64? t)))

  ;; paren-x64-v2-p -> x64-string
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
        (foldr (lambda (s acc) (string-append s "\n" acc)) "" (map statement->x64 s))]))

  ;; Returns the appropriate x64 instruction as a string.
  ;;
  ;; paren-x64-v2-s -> x64-string?
  (define (statement->x64 s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (format "~a ~a, ~a" (binop->ins binop) reg int32)]
      [`(set! ,reg (,binop ,reg ,loc))
        (format "~a ~a, ~a" (binop->ins binop) reg (loc->x64 loc))]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (format "mov ~a, ~a" reg triv)]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (format "mov ~a, ~a" reg (loc->x64 loc))]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (format "mov ~a, ~a" (addr->x64 addr) int32)]
      [`(set! ,addr ,reg)
        (format "mov ~a, ~a" (addr->x64 addr) reg)]))

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
  ;; paren-x64-v2-loc -> string? or register?
  (define (loc->x64 loc)
    (match loc
      [(? register?) loc]
      [addr (addr->x64 addr)]))

  ;; Returns the appropriate x64 instruction name for the binop b.
  (define/contract (binop->ins b)
    (-> symbol? string?)
    (match b
      ['* "imul"]
      ['+ "add"]))

  (program->x64 p))

(module+ test
  (require rackunit)

  (current-pass-list
    (list
      generate-x64
      wrap-x64-run-time
      wrap-x64-boilerplate))

  (define (check-42 p)
    (check-equal?
      (execute p)
      42))

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
       (set! rax (* rax 2)))))
