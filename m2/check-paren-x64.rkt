#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide check-paren-x64)

(define (loc? loc)
  (-> any/c boolean?)
  (or (register? loc) (addr? loc)))

(define (addr? a)
  (-> any/c boolean?)
  (match a
    [`(,fbp - ,dispoffset)
      #:when (and
        (frame-base-pointer-register? fbp)
        (dispoffset? dispoffset))
      #t]
    [_ #f]))


(define (triv? t)
  (-> any/c boolean?)
  (or (register? t) (int64? t)))

(define (binop? b)
  (-> any/c boolean?)
  (and 
    (member b '(* +))
    #t))

;; Milestone 2 Exercise 15
;;
;; Takes an arbitrary value and either returns it,
;; if it is valid Paren-x64 v2 program,
;; or raises an error with a descriptive error message. 
(define/contract (check-paren-x64-syntax p)
  (-> any/c paren-x64-v2?)

  (define (check-paren-x64-p p)
    (match p
      [`(begin ,ss ...)
        (for ([s ss]) (check-paren-x64-s s))
        p]))

  (define (check-paren-x64-s s)
    (match s
      [`(set! ,reg (,binop ,reg ,int32))
        #:when (and
                 (int32? int32)
                 (binop? binop)
                 (register? reg))
        (void)]
      [`(set! ,reg (,binop ,reg ,loc))
        #:when (and
                 (register? reg)
                 (binop? binop)
                 (loc? loc))
        (void)]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (void)]
      [`(set! ,reg ,loc)
        #:when (and (register? reg) (loc? loc))
        (void)]
      [`(set! ,addr ,int32)
        #:when (and (addr? addr) (int32? int32))
        (void)]
      [`(set! ,addr ,reg)
        #:when (and (addr? addr) (register? reg))
        (void)]))

  (check-paren-x64-p p))

(define/contract (check-paren-x64-init p)
  (-> paren-x64-v2? paren-x64-v2?)

  (define rax (current-return-value-register))

  (define (check-paren-x64-init-addr a)
    (match a
      [`(,fbp - ,dispoffset)
        #:when (and (frame-base-pointer-register? fbp) (dispoffset? dispoffset))
        (void)]))

  (define (check-paren-x64-init-p p ctx)
    (match p
      [`(begin ,s ...)
        (if (set-member? (foldl check-paren-x64-init-s '() s) rax)
          p
          (error "Return value register not initialized at end"))]))

  (define (check-paren-x64-init-triv t ctx)
    (match t
      [(? int64?) (void)]
      [reg
        (if (set-member? ctx reg)
          (void)
          (error "Register use as triv before initialization: " reg))]))

  (define (check-paren-x64-init-loc loc ctx)
    (match loc
      [(? register?)
       (if (set-member? ctx loc)
         (void)
         (error "Register use as loc before initialization: " loc))]
      ;; addr
      [_ (check-paren-x64-init-addr loc)]))

  (define (check-paren-x64-init-s s ctx)
    (match s
      [`(set! ,reg (,_ ,reg ,int32))
        #:when (int32? int32)
        (if (set-member? ctx reg)
          s
          (error "Register ~a not initialized: " reg))]
      [`(set! ,reg (,_ ,reg ,loc))
        (check-paren-x64-init-loc loc ctx)
        (if (set-member? ctx reg)
          s
          (error "Register not initialized: " reg))]
      [`(set! ,reg ,triv)
        #:when (and (register? reg) (triv? triv))
        (check-paren-x64-init-triv triv ctx)
        (set-add ctx reg)]
      [`(set! ,reg ,loc)
        #:when (register? reg)
        (check-paren-x64-init-loc loc ctx)
        (set-add ctx reg)]
      [`(set! ,addr ,int32)
        #:when (int32? int32)
        (check-paren-x64-init-addr addr)
        s]
      [`(set! ,addr ,reg)
        (check-paren-x64-init-addr addr)
        (if (set-member? ctx reg)
          s
          (error "Register not initialized: " reg))]))

  (check-paren-x64-init-p p '()))

(define/contract (check-paren-x64 p)
  (-> any/c paren-x64-v2?)

  (check-paren-x64-init (check-paren-x64-syntax p)))

(module+ test
  (require rackunit)

  (define fbp (current-frame-base-pointer-register))

  (check-true
    (addr? `(,fbp - 0)))
  (check-true
    (addr? `(,fbp - 8)))
  (check-false
    (addr? `(,fbp - 1)))
  (check-false
    (addr? 42))
  (check-false
    (addr? '42))

  (define rax (current-return-value-register))

  (define (check-valid p)
    (check-equal?
      (check-paren-x64 p)
      p))

  (define (check-invalid p)
    (check-exn
      exn:fail?
      (lambda () (check-paren-x64 p))))

  ;; simple
  (check-valid
    `(begin (set! ,rax 42)))
  (check-invalid
    '(begin (set! rsi 42)))

  (check-valid
    `(begin
       (set! rsi 42)
       (set! ,rax rsi)))
  (check-invalid
    `(begin
       (set! ,rax rsi)))

  (check-valid
    `(begin
       (set! rsi 42)
       (set! ,rax 0)
       (set! ,rax (+ rax rsi))))
  (check-invalid
    `(begin
       (set! ,rax 0)
       (set! ,rax (+ rax rsi))))

  (check-valid
    `(begin
       (set! ,rax 42)
       (set! (,fbp - 0) ,rax)))
  (check-invalid
    `(begin
       (set! ,rax 42)
       (set! (,fbp - 0) rsi)))

  (check-valid
    `(begin
       (set! ,rax 0)
       (set! ,rax (+ ,rax 42))))
  (check-invalid
    `(begin
       (set! ,rax (+ ,rax 42))))

  (check-valid
    `(begin
       (set! (,fbp - 0) 2)
       (set! ,rax 0)
       (set! ,rax (+ ,rax (,fbp - 0)))))
  (check-invalid
    `(begin
       (set! (,fbp - 0) 2)
       (set! ,rax (+ ,rax (,fbp - 0)))))

  (check-valid
    `(begin
       (set! (,fbp - 0) 42)
       (set! ,rax (,fbp - 0))))
  )
