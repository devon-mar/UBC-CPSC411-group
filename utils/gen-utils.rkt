#lang racket

(provide append-e)

;; General util functions

;; List, Any -> List
;; Append element to end of list
(define (append-e l e)
  (append l (list e)))

(module+ test
  (require rackunit)
  (check-equal? (append-e '() 1) '(1))
  (check-equal? (append-e '(a b c) 'd) '(a b c d))
  (check-equal? (append-e '(a b c) '(d e f)) '(a b c (d e f)))
  )