#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  "../utils/gen-utils.rkt")

(provide patch-instructions)

;; Milestone 2 Exercise 10
;; Milestone 4 Exercise 5
;;
;; Compiles Para-asm-lang v4 to Paren-x64-fvars v4 by patching instructions
;; that have no x64 analogue into a sequence of instructions.
(define/contract (patch-instructions p)
  (-> para-asm-lang-v4? paren-x64-fvars-v4?)

  ;; for convenience...
  (define tmp0 (car (current-patch-instructions-registers)))
  (define tmp1 (cadr (current-patch-instructions-registers)))

  ;; Returns true if i is an int64 but not an int32
  (define (big-int? i)
    (-> any/c boolean?)
    (and (int64? i) (not (int32? i))))
  
  ;; relop -> relop
  ;; Flip the relop to make the comparison opposite
  (define (flip-relop r)
    (match r
      ['< '>=]
      ['<= '>]
      ['= '!=]
      ['>= '<]
      ['> '<=]
      ['!= '=]))
  
  ;; (any ... -> (List-of Paren-x64-fvars-v4-s)) (cons (any -> boolean) any) ...
  ;; -> (List-of Paren-x64-fvars-v4-s)
  ;;
  ;; Take a function that takes in parameters to create list of Paren-x64-fvars 's'
  ;; and a pair of checks and values for those parameters.
  ;; If the check is true, the value is replaced by a temp register as input to the function,
  ;; and if false, the value inputted as is.
  ;; 
  ;; Returns list of all additional 'set!' created to assign the temp registers
  ;; and all of the patched 's' created by the function
  ;;
  (define (patch-set fn . cvs)
    (define cpir (current-patch-instructions-registers))
    (define-values (ls params _)
      (for/fold ([ls '()] [params '()] [tmps cpir]) ([cv cvs])
        (define check (car cv))
        (define value (cdr cv))
        (if (check value)
            (values (append-e ls `(set! ,(first tmps) ,value))
                    (append-e params (first tmps))
                    (rest tmps))
            (values ls (append-e params value) tmps))))
    (append ls (apply fn params)))

  ;; para-asm-lang-v4-s -> (list paren-x64-fvars-v4-s ...)
  (define (patch-instructions-s e)
    (match e
      [`(halt ,opand)
       `((set! ,(current-return-value-register) ,opand) (jump done))]
      [`(set! ,loc (,binop ,loc ,triv))
       (cond
         [(register? loc)
          ;; Paren-x64-fvars does not support (set! reg (binop reg int64))
          (patch-set
            (lambda (t) `((set! ,loc (,binop ,loc ,t))))
            (cons big-int? triv))]
         [(fvar? loc)
          ;; Paren-x64-fvars does not support (set! fvar (binop fvar _))
          ;; Additional patch for (set! reg (binop reg int64))
          (patch-set
            (lambda (l t) `((set! ,l (,binop ,l ,t)) (set! ,loc ,l)))
            (cons (lambda (_) #t) loc)
            (cons big-int? triv))])]
      [`(set! ,loc ,triv)
       ;; Paren-x64-fvars does not support (set! fvar int64|fvar)
       (define (requires-move? t)
         (and (fvar? loc)
              (or (big-int? t)
                  (fvar? t))))
       (patch-set
         (lambda (t) `((set! ,loc ,t)))
         (cons requires-move? triv))]
      [`(jump ,trg)
       ;; Paren-x64-fvars does not support (jump fvar)
       (patch-set
        (lambda (t) `((jump ,t)))
        (cons fvar? trg))]
      [`(with-label ,label ,s)
       (define s-list (patch-instructions-s s))
       `((with-label ,label ,(first s-list)) ,@(rest s-list))]
      [`(compare ,loc ,opand)
       ;; Paren-x64-fvars does not support (compare fvar _)
       ;; or (compare _ fvar)
       (patch-set 
         (lambda (l o) `((compare ,l ,o)))
         (cons fvar? loc)
         (cons fvar? opand))]
      [`(jump-if ,relop ,trg)
       ;; Paren-x64-fvars does not support (jump-if _ loc)
       ;; Additional patch for (jump fvar)
       (if (label? trg)
           (list e)
           (let ([tmp-label (fresh-label)])
             `((jump-if ,(flip-relop relop) ,tmp-label)
               ,@(patch-set
                  (lambda (t) `((jump ,t)))
                  (cons fvar? trg))
               (with-label ,tmp-label (set! ,tmp0 ,tmp0)))))]))

  ;; paren-asm-lang-v4-p -> paren-x64-fvars-v4-p
  (define (patch-instructions-p p)
    (match p
      [`(begin ,s ...)
        `(begin
           ,@(append-map patch-instructions-s s))]))

  (patch-instructions-p p))


(module+ test
  (require rackunit)

  (define large-int (add1 (max-int 32)))

  ;; Returns true if p returns the answer
  ;; to life.
  (define (check-answer-to-life p)
    (check-equal?
      (interp-paren-x64-fvars-v4 (patch-instructions p))
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
       (set! ,rax fv2)
       (jump done))
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
       (set! ,rax 42)
       (jump done))
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
       (set! ,rax 42)
       (jump done))
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
       (set! ,rax 42)
       (jump done))
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
       (set! ,tmp1 fv0)
       (set! ,tmp0 ,val)
       (set! ,tmp1 (+ ,tmp1 ,tmp0))
       (set! fv0 ,tmp1)
       (set! ,rax 42)
       (jump done))
    (and
      (member tmp0 (current-patch-instructions-registers))
      (member tmp1 (current-patch-instructions-registers))
      (not (eq? tmp0 tmp1))
      (eq? rax (current-return-value-register))))

  ;; any -> boolean
  ;; check if r is a register in current-patch-instructions-registers
  (define (patch-register? r)
    (and (member r (current-patch-instructions-registers)) #t))

  ;; halt - in different places
  (check-equal?
    (patch-instructions
      '(begin
        (set! fv0 9)
        (set! rdx L.test.3)
        (jump rdx)
        (with-label L.test.1 (with-label L.test.2 (halt rdx)))
        (with-label L.test.3 (halt 10))
        (halt fv0)))
    '(begin
      (set! fv0 9)
      (set! rdx L.test.3)
      (jump rdx)
      (with-label L.test.1 (with-label L.test.2 (set! rax rdx)))
      (jump done)
      (with-label L.test.3 (set! rax 10))
      (jump done)
      (set! rax fv0)
      (jump done)))

  ;; jump w/ reg & label -- no patch
  (check-equal?
    (patch-instructions
      '(begin
        (set! r9 0)
        (with-label L.!!test.1 (jump r9))
        (jump L.!!test.1)
        (halt rbx)))
    '(begin
        (set! r9 0)
        (with-label L.!!test.1 (jump r9))
        (jump L.!!test.1)
        (set! rax rbx)
        (jump done)))
  
  ;; jump w/ fvar -- patch
  (check-match
    (patch-instructions
      '(begin
        (set! fv0 L.!!test.1)
        (with-label L.!!test.1 (jump fv0))
        (halt rbx)))
    `(begin
        (set! fv0 L.!!test.1)
        (with-label L.!!test.1 (set! ,tmp0 fv0))
        (jump ,tmp0)
        (set! rax rbx)
        (jump done))
    (patch-register? tmp0))

  ;; with-label
  (check-match
    (patch-instructions
      '(begin
        (set! fv1 9)
        (with-label L.test.0 (set! rdx 0))
        (with-label L.test.1 (set! fv0 fv1))
        (with-label L.test.2 (halt fv0))))
    `(begin
      (set! fv1 9)
      (with-label L.test.0 (set! rdx 0))
      (with-label L.test.1 (set! ,tmp0 fv1))
      (set! fv0 ,tmp0)
      (with-label L.test.2 (set! rax fv0))
      (jump done))
    (patch-register? tmp0))
  
  ;; compare with reg -- no patch
  (check-equal?
    (patch-instructions
      '(begin
        (set! r13 9)
        (set! r14 10)
        (compare r14 0)
        (compare r13 r14)))
    `(begin
      (set! r13 9)
      (set! r14 10)
      (compare r14 0)
      (compare r13 r14)))
  
  ;; compare with fvar -- patched
  (check-match
    (patch-instructions
      '(begin
        (set! r13 9)
        (set! fv1 7)
        (set! fv2 3)
        (compare fv1 fv2)
        (compare fv1 r13)))
    `(begin
      (set! r13 9)
      (set! fv1 7)
      (set! fv2 3)
      (set! ,tmp0 fv1)
      (set! ,tmp1 fv2)
      (compare ,tmp0 ,tmp1)
      (set! ,tmp2 fv1)
      (compare ,tmp2 r13))
    (and (andmap patch-register? (list tmp0 tmp1 tmp2))
         (not (equal? tmp0 tmp1))))

  ;; jump-if with label - no patch
  (check-equal?
    (patch-instructions
      '(begin
        (set! rcx 0)
        (compare rcx 1)
        (jump-if = done)
        (compare rcx 3)
        (jump-if >= done)
        (halt 5)))
    '(begin
      (set! rcx 0)
      (compare rcx 1)
      (jump-if = done)
      (compare rcx 3)
      (jump-if >= done)
      (set! rax 5)
      (jump done)))

  ;; jump-if with reg & fvar -- patched
  (check-match
    (patch-instructions
      '(begin
        (set! rcx done)
        (set! fv0 done)
        (compare rcx 1)
        (jump-if < rcx)
        (compare rcx 2)
        (jump-if != fv0)
        (set! rbx 0)))
    `(begin
      (set! rcx done)
      (set! fv0 done)
      (compare rcx 1)
      (jump-if >= ,tmpl0)
      (jump rcx)
      (with-label ,tmpl0 (set! ,t0 ,t0))
      (compare rcx 2)
      (jump-if = ,tmpl1)
      (set! ,t1 fv0)
      (jump ,t1)
      (with-label ,tmpl1 (set! ,t2 ,t2))
      (set! rbx 0))
    (and (andmap label? (list tmpl0 tmpl1))
         (not (equal? tmpl0 tmpl1))
         (andmap patch-register? (list t0 t1 t2))))
  )
