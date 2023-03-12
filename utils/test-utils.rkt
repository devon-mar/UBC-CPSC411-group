#lang racket

(provide
  extract-asm-pred-lang)

;; p -> info tail (List-of label) (List-of info) (List-of tail)
;; Extract fields from a Asm-Pred-Lang program
;; Returns info of the main tail and main tail & labels, infos, tails of each proc
(define (extract-asm-pred-lang p)
  (match p
    [`(module ,main-info (define ,proc-labels ,proc-infos ,proc-tails) ... ,main-tail)
      (values main-info main-tail proc-labels proc-infos proc-tails)]))