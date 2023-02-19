#lang racket

(provide interp-paren-x64)

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

;; Milestone 2 Exercise 16
;;
;; Interprets the Paren-x64 v2 program,
;; returning the final value as an exit code in the range 0–255.
(define/contract (interp-paren-x64 p)
  (-> paren-x64-v2? int64?)

  (define (triv? t)
    (-> any/c boolean?)
    (or (register? t) (int64? t)))

  ; Environment (List-of (paren-x64-v2 Statements)) -> Integer
  (define (eval-instruction-sequence env sls)
    (if (empty? sls)
        (dict-ref env 'rax)
        (eval-instruction-sequence
          (eval-statement env (car sls))
          (cdr sls))))

  ; Environment Statement -> Environment
  (define (eval-statement env s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (int32? int32)
        (dict-set env reg ((eval-binop binop) (dict-ref env reg) int32))]
      [`(set! ,reg (,binop ,reg ,loc))
        (dict-set env reg ((eval-binop binop) (dict-ref env reg) (dict-ref env loc)))]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (dict-set env reg (eval-triv env triv))]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (dict-set env reg (dict-ref env loc))]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (dict-set env addr int32)]
      [`(set! ,addr ,reg)
        (dict-set env addr (dict-ref env reg))]))

  ; (Paren-x64-v2 binop) -> procedure?
  (define (eval-binop b)
    (match b
      ['* x64-mul]
      ['+ x64-add]))

  ; Environment (Paren-x64-v2 triv) -> Integer
  (define (eval-triv regfile t)
    (match t
      [(? register?) (dict-ref regfile t)]
      ;; int64
      [_ t]))

  (define (eval-p p)
    (match p
      [`(begin ,s ...)
        (eval-instruction-sequence '() s)]))

  (eval-p p))

(module+ test
  (require rackunit)

  (define (check-42 p)
    (check-equal?
      (interp-paren-x64 p)
      42))
  
  ;; simple
  (check-42 '(begin (set! rax 42)))

  ;; reg to reg move
  (check-42
    '(begin
       (set! rsi 42)
       (set! rax rsi)))

  ;; imm32 to addr
  ;; addr to reg
  (check-42
    '(begin
       (set! (rbp - 0) 42)
       (set! rax (rbp - 0))))


  ;; mul
  (check-42
    '(begin
       (set! rax 21)
       (set! rax (* rax 2))))

  ;; add
  (check-42
    '(begin
       (set! rax 40)
       (set! rax (+ rax 2))))

  ;; add from addr to reg
  (check-42
    '(begin
       (set! rax 2)
       (set! (rbp - 8) 40)
       (set! rax (+ rax (rbp - 8)))))
  ;; add from reg to reg
  (check-42
    '(begin
       (set! rax 2)
       (set! rsi 40)
       (set! rax (+ rax rsi))))

  ;; reg to addr
  (check-42
    '(begin
       (set! rsi 42)
       (set! (rbp - 0) rsi)
       (set! rax (rbp - 0))))
  )
