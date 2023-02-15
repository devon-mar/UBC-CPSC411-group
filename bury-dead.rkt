#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  cpsc411/langs/v2-reg-alloc)


;; Milestone 3 - Exercise 6 

;; Removes assignments to unused abstract locations.
;; An assignment to a variable is dead if it is not in the undead-out set for the instruction.
(define (bury-dead p)
  (-> asm-lang-v2/undead? asm-lang-v2/locals?)

  ;; Unused
  #;
  (define (bury-dead-binop b)
    (match b
      ['* (TODO)]
      ['+ (TODO)]))

  ;; Unused
  #;
  (define (bury-dead-triv t)
    (match t
      [(? int64?) (TODO)]
      [aloc (TODO)]))

  ;; tail undead-set-tree? -> tail
  ;; 'Buries' (removes) any assignments to unused alocs.
  (define (bury-dead-tail t ust)
    (match (cons t ust)
      [(cons `(halt ,_) '())
        t]
      [(cons `(begin ,effects ... ,tail) `(,effects-usts ... ,tail-ust))
       `(begin
          ,@(append-map bury-dead-effect effects effects-usts)
          ,(bury-dead-tail tail tail-ust))]))

  ;; Returns a list with a single effect or '() if the assignment
  ;; is to an unused aloc.
  ;; 
  ;; effect undead-set-tree -> (list effect ...)
  (define (bury-dead-effect e ust)
    (match (cons e ust)
      [(cons `(set! ,aloc (,_ ,aloc ,_)) `(,vars ...))
        (if (set-member? vars aloc)
          (list e)
          '())]
      [(cons `(set! ,aloc ,_) `(,vars ...))
        (if (set-member? vars aloc)
          (list e)
          '())]
      ;; Modified template to combine tail effect with other ones.
      [(cons `(begin ,es ...) `(,usts ...))
       (list `(begin ,@(append-map bury-dead-effect es usts)))]))

  ;; Returns a p with assignments to unused alocs removed.
  ;;
  ;; asm-lang-v2/undead? -> asm-lang-v2/locals?
  (define (bury-dead-p p)
    (match p
      [`(module ,info ,tail)
       `(module
          ,(info-remove info 'undead-out)
          ,(bury-dead-tail tail (info-ref info 'undead-out)))]))

  (bury-dead-p p))

(module+ test
  (require rackunit)

  ;; 1. Create a asm-lang-v2/undead? program using locals, ust, and p.
  ;; 2. Checks that the output of bury-dead
  ;; 3. Checks that the output program returns 42 when ran.
  (define (check-42 locals ust p want)
    (define have
      (bury-dead `(module ([locals ,locals] [undead-out ,ust]) ,p)))

    (check-equal?
      have
      `(module
         ([locals ,locals])
         ,want))

    (check-equal?
      (interp-asm-lang-v2/locals have)
      42))

  ;; nothing to do
  (check-42
    '()
    '()
    '(halt 42)
    '(halt 42))

  ;; remove one imm mov
  (check-42
    '(x.1)
    '(() ())
    '(begin
       (set! x.1 42)
       (halt 42))
    '(begin
       (halt 42)))
  ;; no changes needed
  (check-42
    '(x.1)
    '((x.1) ())
    '(begin
       (set! x.1 42)
       (halt x.1))
    '(begin
       (set! x.1 42)
       (halt x.1)))

  ;; remove an aloc->aloc mov
  (check-42
    '(x.1 x.2)
    '((x.2) () ())
    '(begin
       (set! x.2 42)
       (set! x.1 x.2)
       (halt 42))
    '(begin
       (set! x.2 42)
       (halt 42)))

  ;; remove a binop
  (check-42
    '(x.1 x.2)
    '((x.1) (x.2) () ())
    '(begin
       (set! x.1 40)
       (set! x.2 x.1)
       (set! x.2 (+ x.2 2))
       (halt 42))
    '(begin
       (set! x.1 40)
       (set! x.2 x.1)
       (halt 42)))
  ;; leave the binop alone!
  (check-42
    '(x.1 x.2)
    '((x.1) (x.2) (x.2) ())
    '(begin
       (set! x.1 40)
       (set! x.2 x.1)
       (set! x.2 (+ x.2 2))
       (halt x.2))
    '(begin
       (set! x.1 40)
       (set! x.2 x.1)
       (set! x.2 (+ x.2 2))
       (halt x.2)))

  ;; lots of nesting with removals
  (check-42
    '(x.1 x.2 x.3)
    '((()) (() (x.2) (x.2) ()))
    '(begin
       (begin
         (set! x.1 2))
       (begin
         (set! x.3 3)
         (set! x.2 40)
         (set! x.2 (+ 40 2))
         (halt 42)))
    '(begin
       (begin)
       (begin
         (set! x.2 40)
         (set! x.2 (+ 40 2))
         (halt 42))))
  ;; lots of nesting with nothing to remove
  (check-42
    '(x.1 x.2 x.3)
    '(((x.1)) ((x.3 x.1) (x.3 x.1) (x.1) (x.1 x.2) (x.2) (x.2) ()))
    '(begin
       (begin
         (set! x.1 1))
       (begin
         (set! x.3 1)
         (set! x.3 (* x.3 2))
         (set! x.1 (+ x.1 x.3))
         (set! x.2 38)
         (set! x.2 (+ x.2 x.1))
         (set! x.2 (+ x.2 1))
         (halt x.2)))
    '(begin
       (begin
         (set! x.1 1))
       (begin
         (set! x.3 1)
         (set! x.3 (* x.3 2))
         (set! x.1 (+ x.1 x.3))
         (set! x.2 38)
         (set! x.2 (+ x.2 x.1))
         (set! x.2 (+ x.2 1))
         (halt x.2))))
  )
