#lang racket

(require cpsc411/compiler-lib)

;; compiler utility functions
(provide
  relop?
  undead-set/rloc?)

;; Return true if r is a relop, otherwise false
;; any -> boolean
(define (relop? r)
  (and (member r '(< <= = >= > !=)) #t))

;; True when input is an undead-set that might contain physical locations or abstract locations,
;; and #f otherwise.
;; any -> boolean
(define undead-set/rloc? (_undead-set? (or/c aloc? fvar? register?)))

(module+ test
  (require rackunit)
  (for ([r '(< <= = >= > !=)])
    (check-true (relop? r)))
  (for ([n (list 1 'a '(<) '+ =)])
    (check-false (relop? n)))

  (check-true (undead-set/rloc? '()))
  (check-false (undead-set/rloc? '(L.test.1)))
  (check-false (undead-set/rloc? '(x.1 (x.2))))
  (check-true (undead-set/rloc? '(x.1)))
  (check-true (undead-set/rloc? '(r14)))
  (check-true (undead-set/rloc? '(fv0)))
  (check-true (undead-set/rloc? '(x.1 y.2 r14 rax rbp fv2 fv13)))
  )
