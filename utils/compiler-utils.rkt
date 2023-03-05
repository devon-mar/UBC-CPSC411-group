#lang racket

;; compiler utility functions
(provide
  relop?)

;; Return true if r is a relop, otherwise false
;; any -> boolean
(define (relop? r)
  (and (member r '(< <= = >= > !=)) #t))