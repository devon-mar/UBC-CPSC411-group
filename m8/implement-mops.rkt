#lang racket

(require
  cpsc411/langs/v8)

(provide implement-mops)

;; Milestone 8 Exercise 13
;;
;; Compiles mops to instructions on pointers with index and displacement-mode operands.
(define/contract (implement-mops p)
  (-> paren-x64-mops-v8? paren-x64-v8?)

  ;; paren-x64-mops-v8-p -> paren-x64-v8-p
  (define (implement-mops-p p)
    (match p
      [`(begin ,s ...)
       `(begin ,@(map implement-mops-s s))]))

  ;; paren-x64-mops-v8-reg paren-x64-mops-v8-index
  ;; -> paren-x64-v8-addr
  ;; Convert reg + index into index mode operand
  (define (reg-index->addr reg index)
    `(,reg + ,index))

  ;; paren-x64-mops-v8-s -> paren-x64-v8-s
  (define (implement-mops-s s)
    (match s
      [`(set! ,reg1 (mref ,reg2 ,index))
       `(set! ,reg1 ,(reg-index->addr reg2 index))]
      ;; For:
      ;; (set! reg_1 (binop reg_1 int32))
      ;; (set! reg_1 (binop reg_1 loc))
      [`(set! ,_reg1 (,_binop ,_reg1 ,_)) s]
      ;; For:
      ;; (set! addr int32)
      ;; (set! addr trg)
      ;; (set! reg loc)
      ;; (set! reg triv)
      [`(set! ,_ ,_) s]
      ;; For:
      ;; (mset! reg_1 index int32)
      ;; (mset! reg_1 index trg)
      [`(mset! ,reg ,index ,int32-or-trg)
       `(set! ,(reg-index->addr reg index) ,int32-or-trg)]
      [`(with-label ,label ,st)
       `(with-label ,label ,(implement-mops-s st))]
      [`(jump ,_trg) s]
      [`(compare ,_reg ,_opand) s]
      [`(jump-if ,_relop ,_label) s]))

  ;; all other templates removed

  (implement-mops-p p))

(module+ test
  (require rackunit)

  ;; Check that the compiled program interprets to 42
  (define-check (check-42 p)
    (check-equal?
      (interp-paren-x64-v8 (implement-mops p))
      42))

  ;; not mref, mset
  (check-42
    '(begin
       (set! rax 22)
       (set! rax (* rax 2))
       (compare rax 5)
       (jump-if < L.test.1)
       (set! rax (- rax 2))
       (jump L.test.2)
       (with-label L.test.1 (set! rax -1))
       (with-label L.test.2 (jump done))))

  ;; mref, mset
  (check-42
    '(begin
       (set! rdx 32)
       (mset! r12 8 42)
       (mset! r12 rdx done)
       (set! rax (mref r12 8))
       (set! r8 (mref r12 rdx))
       (jump r8)))

  ;; with-label
  (check-42
    '(begin
       (with-label L.test.1 (mset! r12 24 42))
       (with-label L.test.2 (set! rax (mref r12 24)))
       (with-label L.test.3 (jump done))))
  )
