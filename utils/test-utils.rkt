#lang racket

(require
  rackunit
  "gen-utils.rkt")

(provide
  extract-asm-pred-lang
  check-list-equiv?)

;; p -> info tail (List-of label) (List-of info) (List-of tail)
;; Extract fields from a Asm-Pred-Lang program
;; Returns info of the main tail and main tail & labels, infos, tails of each proc
(define (extract-asm-pred-lang p)
  (match p
    [`(module ,main-info (define ,proc-labels ,proc-infos ,proc-tails) ... ,main-tail)
      (values main-info main-tail proc-labels proc-infos proc-tails)]))

;; list list -> void
;; Check if list contains the same elements
(define-binary-check (check-list-equiv? list1 list2)
  (list-equiv? list1 list2))

(module+ test
  (define-values (main-info main-tail proc-labels proc-infos proc-tails)
    (extract-asm-pred-lang
      '(module ((locals ()))
        (define L.test.1 ((locals (x.1))) (halt 1))
        (define L.test.2 ((locals (x.2))) (halt 2))
        (halt 0))))
  (check-equal? main-info '((locals ())))
  (check-equal? main-tail '(halt 0))
  (check-equal? proc-labels '(L.test.1 L.test.2))
  (check-equal? proc-infos '(((locals (x.1))) ((locals (x.2)))))
  (check-equal? proc-tails '((halt 1) (halt 2)))

  (check-list-equiv? '(1 3 2) '(3 2 1))
  )
