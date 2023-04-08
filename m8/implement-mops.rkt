#lang racket

(require
  cpsc411/compiler-lib
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
  (define (reg-index->addr reg index)
    `(,reg + ,index))

  ;; paren-x64-mops-v8-s -> paren-x64-v8-s
  (define (implement-mops-s s)
    (match s
      [`(set! ,reg1 (mref ,reg2 ,index))
       `(set! ,reg1 ,(reg-index->addr reg2 index))]
      [`(set! ,_ (,_ ,_ ,_)) s]           ;; combined templates
      [`(set! ,_ ,_) s]                   ;; combined templates
      [`(mset! ,reg ,index ,int32-or-trg) ;; combined templates
       `(set! ,(reg-index->addr reg index) ,int32-or-trg)]
      [`(with-label ,_ ,_) s]
      [`(jump ,_) s]
      [`(compare ,_ ,_) s]
      [`(jump-if ,_ ,_) s]))

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
  )
