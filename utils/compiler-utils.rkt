#lang racket

;; compiler utility functions
(provide
  relop?)

;; Return true if r is a relop, otherwise false
;; any -> boolean
(define (relop? r)
  (and (member r '(< <= = >= > !=)) #t))

(module+ test
  (require rackunit)
  (for ([r '(< <= = >= > !=)])
    (check-true (relop? r)))
  (for ([n (list 1 'a '(<) '+ =)])
    (check-false (relop? n)))
  )