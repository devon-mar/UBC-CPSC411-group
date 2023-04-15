#lang racket

(provide
  append-e
  list-equiv?
  dict-equal?
  or-checks
  and-checks)

;; General util functions

;; List, Any -> List
;; Append element to end of list
(define (append-e l e)
  (append l (list e)))

;; list list -> boolean
;; Check if list contains the same elements
(define/contract (list-equiv? list1 list2)
  (-> list? list? boolean?)
  (empty? (set-symmetric-difference list1 list2)))

;; Dict(K V) Dict(K V) ((V V) -> boolean) -> boolean
;; Check if two dictionaries contain the same data
;; val-equal? is used to check if the data is the same
(define (dict-equal? dict1 dict2 [val-equal? equal?])
  (and (= (dict-count dict1) (dict-count dict2))
       (list-equiv? (dict-keys dict1) (dict-keys dict2))
       (for/and ([k (dict-keys dict1)])
         (val-equal? (dict-ref dict1 k) (dict-ref dict2 k)))))

;; (any -> boolean) ...  -> (any -> boolean)
;; Combine the checks to create one check such that it returns true
;; iff one of the checks in check-list passes with value
(define (or-checks . check-list)
  (lambda (value) (ormap (lambda (check) (check value)) check-list)))

(module+ test
  (require rackunit)
  ;; append-e tests
  (check-equal? (append-e '() 1) '(1))
  (check-equal? (append-e '(a b c) 'd) '(a b c d))
  (check-equal? (append-e '(a b c) '(d e f)) '(a b c (d e f)))

  ;; list-equiv? tests
  (check-true (list-equiv? '() '()))
  (check-false (list-equiv? '() '(1)))
  (check-false (list-equiv? '(2) '(1)))
  ;; swapped ordering
  (check-true (list-equiv? '(1 2 3) '(2 3 1)))
  ;; swapped ordering with list
  (check-true (list-equiv? '(1 (1 2) 3) '((1 2) 3 1)))
  ;; swapped ordering in inner list
  (check-false (list-equiv? '(1 (2 1) 3) '((1 2) 3 1)))

  ;; dict-equal? tests
  (check-true (dict-equal? '() '()))
  (check-true (dict-equal? '((a 1)) '((a 1))))
  ;; missing key
  (check-false (dict-equal? '((b 2 3) (c 4)) '((c 4) (a 1) (b 2 3))))
  ;; same key but swapped inner ordering
  (check-false (dict-equal? '((b 3 2) (c 4) (a 1)) '((c 4) (a 1) (b 2 3))))
  ;; same key but same inner ordering
  (check-true (dict-equal? '((b 2 3) (c 4) (a 1)) '((c 4) (a 1) (b 2 3))))

  ;; dict-equal? with val-equal?
  (check-true (dict-equal? '((b 3 2 1) (c 4 5) (a 1)) '((c 5 4) (a 1) (b 2 1 3)) list-equiv?))

  ;; Helpers
  ;; any -> true
  ;; Always return true
  (define truefn (lambda (x) #t))
  ;; any -> false
  ;; Always return false
  (define falsefn (lambda (x) #f))

  ;; or-checks tests
  (check-false ((or-checks) 5))
  (check-true ((or-checks integer?) 5))
  (check-false ((or-checks string?) 10))
  ;; all trues
  (check-true ((or-checks truefn integer? truefn) 1))
  ;; one true, many false
  (check-true ((or-checks falsefn falsefn falsefn truefn falsefn falsefn) "hi"))
  ;; many true, one false
  (check-true ((or-checks string? string? string? string? string? integer?) "world"))
  ;; all falses
  (check-false ((or-checks falsefn string? falsefn list?) 5))
  )
