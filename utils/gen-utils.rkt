#lang racket

(provide append-e)

;; General util functions

;; List, Any -> List
;; Append element to end of list
(define (append-e l e)
  (append l (list e)))