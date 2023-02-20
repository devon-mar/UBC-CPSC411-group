#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide patch-instructions)

;; Milestone 2 Exercise 10
;;
;; Compiles Para-asm-lang v2 to Paren-x64-fvars v2 by patching instructions
;; that have no x64 analogue into a sequence of instructions.
(define/contract (patch-instructions p)
  (-> para-asm-lang-v2? paren-x64-fvars-v2?)

  ;; for convenience...
  (define tmp0 (car (current-patch-instructions-registers)))
  (define tmp1 (cadr (current-patch-instructions-registers)))

  ;; Returns true if i is an int64 but not an int32
  (define (big-int? i)
    (-> any/c boolean?)
    (and (int64? i) (not (int32? i))))

  ;; para-asm-lang-v2-effect -> (list paren-x64-fvars-v2-effect ...)
  (define (patch-instructions-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
        (cond
          [(and
             (register? loc)
             (big-int? triv))
           ;; have to use a temp register to hold triv
           `((set! ,tmp0 ,triv)
             (set! ,loc (,binop ,loc ,tmp0)))]
          [(register? loc)
           ;; it's just a reg/fvar so we can just return the original
           (list e)]
          ;; from here on loc is a fvar
          ;; so we have to use at least one temp register
          [(big-int? triv)
           `((set! ,tmp1 ,triv)
             (set! ,tmp0 ,loc)
             (set! ,tmp0 (,binop ,tmp0 ,tmp1))
             (set! ,loc ,tmp0))]
          [else
           `((set! ,tmp0 ,loc)
             (set! ,tmp0 (,binop ,tmp0 ,triv))
             (set! ,loc ,tmp0))])]
      [`(set! ,loc ,triv)
        (cond
          ;; paren-x64-fvars-v2 doesn't support imm64 to fvar moves
          ;; nor does paren-x64-fvars-v2 doesn't support fvar to fvar moves
          [(and (fvar? loc)
                (or (big-int? triv)
                    (fvar? triv)))
           `((set! ,tmp0 ,triv)
             (set! ,loc ,tmp0))]
          [else
            (list e)])]))

  ;; paren-asm-lang-v2-p -> paren-x64-fvars-v2-p
  (define (patch-instructions-p p)
    (match p
      [`(begin ,es ... (halt ,triv))
        `(begin
           ,@(append-map patch-instructions-effect es)
           (set! ,(current-return-value-register) ,triv))]))

  (patch-instructions-p p))


(module+ test
  (require rackunit)

  (define large-int (add1 (max-int 32)))

  ;; Returns true if p returns the answer
  ;; to life.
  (define (check-answer-to-life p)
    (check-equal?
      (interp-paren-x64-fvars-v2 (patch-instructions p))
      42))

  ;; return from immediate
  (check-answer-to-life
    '(begin (halt 42)))


  ;; return from register rbx
  (check-answer-to-life
    '(begin
       (set! rbx 42)
       (halt rbx)))

  ;; return from rax
  (check-answer-to-life
    '(begin
       (set! rax 42)
       (halt rax)))

  ;; binop, dest register, src imm
  (check-answer-to-life
   '(begin
      (set! r9 40)
      (set! r9 (+ r9 2))
      (halt r9)))

  ;; halt from fvar
  (check-answer-to-life
    '(begin
       (set! fv1 42)
       (halt fv1)))

  ;; binop with fvar as dest, imm src
  ;; This shouldn't work with interp-paren-x64-fvars-v2
  ;; '(begin (set! fv2 40) (set! fv2 (+ fv2 2)) (set! rax fv2))
  (check-answer-to-life
    '(begin
       (set! fv2 40)
       (set! fv2 (+ fv2 2))
       (halt fv2)))
  ;; the interpreter allows us to add
  ;; when the dest is a fvar...
  ;; so we add a check-match
  (check-match
    (patch-instructions
      '(begin
         (set! fv2 40)
         (set! fv2 (+ fv2 2))
         (halt fv2)))
    `(begin
       (set! fv2 40)
       (set! ,tmp fv2)
       (set! ,tmp (+ ,tmp 2))
       (set! fv2 ,tmp)
       (set! ,rax fv2))
    (and
      (member tmp (current-patch-instructions-registers))
      (eq? rax (current-return-value-register))))

  ;; imm64 to fvar move
  (check-answer-to-life
    `(begin
       (set! fv2 ,large-int)
       (halt 42)))
  ;; the interpreter doesn't seem to catch errors with the above case...
  ;; so we also use a check-match
  (check-match
    (patch-instructions
      `(begin
         (set! fv2 ,large-int)
         (halt 42)))
    `(begin
       (set! ,tmp ,val)
       (set! fv2 ,tmp)
       (set! ,rax 42))
    (and
      (member tmp (current-patch-instructions-registers))
      (eq? rax (current-return-value-register))
      (eq? val large-int)))

  ;; fvar to fvar move
  (check-answer-to-life
    `(begin
       (set! fv1 10)
       (set! fv2 fv1)
       (halt 42)))
  ;; again need a check-match
  (check-match
    (patch-instructions
      `(begin
         (set! fv1 10)
         (set! fv2 fv1)
         (halt 42)))
    `(begin
       (set! fv1 10)
       (set! ,tmp fv1)
       (set! fv2 ,tmp)
       (set! ,rax 42))
    (and
      (member tmp (current-patch-instructions-registers))
      (eq? rax (current-return-value-register))))


  ;; add imm64 to reg
  (check-answer-to-life
    `(begin
       (set! rsi 0)
       (set! rsi (+ rsi ,large-int))
       (halt 42)))
  (check-match
    (patch-instructions
      `(begin
         (set! rsi 0)
         (set! rsi (+ rsi ,large-int))
         (halt 42)))
    `(begin
       (set! rsi 0)
       (set! ,tmp ,val)
       (set! rsi (+ rsi ,tmp))
       (set! ,rax 42))
    (and
      (member tmp (current-patch-instructions-registers))
      (eq? rax (current-return-value-register))))

  ;; add imm64 to fvar
  (check-answer-to-life
    `(begin
       (set! fv0 0)
       (set! fv0 (+ fv0 ,large-int))
       (halt 42)))
  (check-match
    (patch-instructions
      `(begin
         (set! fv0 0)
         (set! fv0 (+ fv0 ,large-int))
         (halt 42)))
    `(begin
       (set! fv0 0)
       (set! ,tmp0 ,val)
       (set! ,tmp1 ,fv0)
       (set! ,tmp1 (+ ,tmp1 ,tmp0))
       (set! fv0 ,tmp1)
       (set! ,rax 42))
    (and
      (member tmp0 (current-patch-instructions-registers))
      (member tmp1 (current-patch-instructions-registers))
      (not (eq? tmp0 tmp1))
      (eq? rax (current-return-value-register))))
  )
