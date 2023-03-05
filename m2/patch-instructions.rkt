#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide patch-instructions)

;; Milestone 2 Exercise 10
;; Milestone 4 Exercise 5
;;
;; Compiles Para-asm-lang v4 to Paren-x64-fvars v4 by patching instructions
;; that have no x64 analogue into a sequence of instructions.
(define/contract (patch-instructions p)
  (-> para-asm-lang-v4? paren-x64-fvars-v4?)

  (define tmp0 (car (current-patch-instructions-registers)))

  ;; Return true if i is an int64 but not an int32.
  (define/contract (big-int? i)
    (-> any/c boolean?)
    (and (int64? i)
         (not (int32? i))))

  (define (patch-instructions-p p)
    (match p
      [`(begin ,s ...)
        `(begin
           ,@(append-map patch-instructions-s s))]))

  ;; Returns a list of equivalent instructions in paren-x64-fvars-v4
  ;; to s.
  ;;
  ;; para-asm-lang-v4-s -> (listof paren-x64-fvars-v4-s)
  (define/contract (patch-instructions-s s)
    (-> any/c list?)
    (match s
      [`(halt ,opand)
        `((set! ,(current-return-value-register) ,opand)
          (jump done))]
      ;; Loc must be a register. Otherwise we need a temp.
      ;; Supported opands: int32/reg/fvar
      [`(set! ,loc (,binop ,loc ,opand))
        (use-tmp
          loc
          (list register?)
          (current-patch-instructions-registers)
          (lambda (r registers)
            (use-tmp
              opand
              (list int32? register? fvar?)
              registers
              (lambda (op _)
                `((set! ,r (,binop ,r ,op))
                  ;; TODO: this part... is there a better way??
                  ,@(if (equal? loc r)
                      '()
                      ;; move it back
                      `((set! ,loc ,r))))))))]
      ;; Combinations -> Supported by paren-x64-fvars
      ;; reg / int32  -> Y
      ;; reg / int64  -> N
      ;; reg / label  -> N
      ;; reg / reg    -> Y
      ;; reg / fvar   -> Y
      ;; fvar / int32 -> Y
      ;; fvar / int64 -> N
      ;; fvar / label -> Y
      ;; fvar / reg   -> Y
      ;; fvar / fvar  -> N
      [`(set! ,loc ,triv)
        (if (or (big-int? triv)
                (and (register? loc) (label? triv))
                (and (fvar? loc) (fvar? triv)))
          `((set! ,tmp0 ,triv)
            (set! ,loc ,tmp0))
          (list s))]
      [`(jump ,trg)
        ;; paren-x64-fvars-v4 only supports reg/label
        ;; para-asm-lang-v4 suports label/reg/fvar
        (use-tmp
          trg
          (list register? label?)
          (current-patch-instructions-registers)
          (lambda (reg/label _)
            `((jump ,reg/label))))]
      [`(with-label ,label ,s)
        (define ss (patch-instructions-s s))
        `((with-label ,label ,(car ss))
          ,@(cdr ss))]
      [`(compare ,loc ,opand)
        (use-tmp
          loc
          ;; first param must be a reg in paren-x64-fvars-v4
          (list register?)
          (current-patch-instructions-registers)
          (lambda (r registers)
            (use-tmp
              opand
              (list int64? register?)
              registers
              (lambda (opand _)
                `((compare ,r ,opand))))))]
      ;; trg must be a label in paren-x64-fvars-v4
      [`(jump-if ,relop ,trg)
        (if (label? trg)
          (list s)
          (let ([tmp-label (fresh-label)])
            `((jump-if ,(invert-relop relop) ,tmp-label)
              ;; paren-x64-fvars-v4 jump only supports a label or reg
              ;; so we send it thorugh patch-instructions-s
              ,@(patch-instructions-s `(jump ,trg))
              ;; need some noop
              (with-label ,tmp-label (set! ,tmp0 ,tmp0)))))]))


  ;; not used
  #;
  (define (patch-instructions-triv t)
    (match t
      [(? label?) (void)]
      [opand opand]))

  ;; Returns true if any procs return #t when passed v.
  (define/contract (satisfies-any v procs)
    (-> any/c (listof (-> any/c boolean?)) boolean?)
    (if (empty? procs)
      #t
      (if ((car procs) v)
        (satisfies-any v (cdr procs))
        #f)))

  ;; If v does not satisfy any of the procedures in procs,
  ;; move v into a tmp from registers and pass that temp to f.
  ;; Otherwise call (f v registers).
  ;; f: v | register? -> (listof s)
  ;; -> (listof s)
  (define/contract (use-tmp v procs registers f)
    (-> any/c (listof (-> any/c boolean?)) (listof register?) (-> any/c (listof register?) list?)
        list?)
    (if (satisfies-any v procs)
      (f v registers)
      `((set! ,(car registers) ,v)
        ,@(f (car registers) (cdr registers)))))
                  
  ;; Returns the opposite of relop
  ;;
  ;; relop -> relop
  (define (invert-relop op)
    (match op
      ['< '>=]
      ['<= '>]
      ['= '!=]
      ['>= '<]
      ['> '<=]
      ['!= '=]))

  (patch-instructions-p p))

(module+ test
  (require rackunit)

  (define big-int (add1 (max-int 32)))

  ;; Returns true if p returns the answer
  ;; to life.
  (define (check-42 p)
    (check-equal?
      (interp-paren-x64-fvars-v4 (patch-instructions p))
      42))

  (check-42
     ;; (halt opand/int64)
    '(begin (halt 42)))

  ;; halt not in tail position
  (check-42
    '(begin
       (halt 42)
       (halt 10)))

  (check-42
    '(begin
       ;; (set! loc/reg triv/opand/int64)
       (set! rsi 42)
       ;; (halt opand/loc/reg)
       (halt rsi)))

  (check-42
    '(begin
       ;; (set! loc/fvar triv/opand/int64)
       (set! fv1 42)
       ;; (halt opand/loc/fvar)
       (halt fv1)))

  (check-42
    '(begin
       (set! rsi 0)
       ;; (set! loc/reg triv/label)
       (set! r9 L.a.1)
       ;; (jump trg/loc/reg)
       (jump r9)
       (halt 1)
       ;; (with-label label s)
       ;; (set! loc_1/reg (binop loc_1 opand/int32))
       (with-label L.a.1 (set! rsi 42))
       ;; (set! loc/reg triv/opand/loc/reg)
       (set! r12 rsi)
       (halt r12)))

  (check-42
    '(begin
       (set! rsi 0)
       ;; (set! loc/fvar triv/label)
       (set! fv1 L.a.1)
       ;; (jump trg/loc/fvar)
       (jump fv1)
       (halt 1)
       (with-label L.a.1 (set! rsi 42))
       (halt rsi)))

  (check-42
    '(begin
      (set! fv1 42)
      ;; (set! loc/reg triv/opand/loc/fvar)
      (set! rsi fv1)
      ;; (set! loc/fvar triv/opand/loc/reg)
      (set! fv2 rsi)
      ;; (set! loc/fvar triv/opand/loc/fvar)
      (set! fv3 fv2)
      (halt fv3)))

  (check-42
    `(begin
       (set! r12 0)
       ;; (set! loc_1/reg (binop loc_1 opand/int64))
       (set! r12 (+ r12 ,big-int))
       (set! r9 ,big-int)
       ;; (compare loc/reg opand/loc/reg)
       (compare r9 r12)
       ;; (jump-if relop trg/label)
       (jump-if = L.a.1)
       (halt 1)
       (with-label L.a.1 (halt 42))))

  (check-42
    `(begin
       (set! fv1 2)
       (set! r12 3)
       (set! r9 7)
       ;; (set! loc_1/reg (binop loc_1 opand/loc/reg))
       (set! r12 (* r12 r9))
       ;; (set! loc_1/reg (binop loc_1 opand/loc/fvar))
       (set! r12 (* r12 fv1))
       (halt r12)))

  (check-42
    `(begin
       (set! fv1 2)
       ;; (set! loc_1/fvar (binop loc_1 opand/int64))
       (set! fv1 (+ fv1 ,big-int))
       (set! r9 ,big-int)
       ;; (compare loc/fvar opand/loc/reg)
       (compare fv1 r9)
       (jump-if > L.a.1)
       (halt 1)
       (with-label L.a.1 (halt 42))))

  (check-42
    `(begin
       (set! fv1 1)
       (set! rsi 1)
       ;; (set! loc_1/fvar (binop loc_1 opand/loc/reg))
       (set! fv1 (+ fv1 rsi))
       (set! fv2 40)
       ;; (set! loc_1/fvar (binop loc_1 opand/loc/fvar))
       (set! fv1 (+ fv1 fv2))
       (halt fv1)))

  (check-42
    `(begin
       ;; (jump trg/label)
       (jump L.a.1)
       (halt 1)
       (with-label L.a.1 (halt 42))))

  ;; TODO
  (check-42
    `(begin
       (set! fv2 2)
       (set! r9 1)
       (set! fv1 2)
       (set! fv3 L.a.1)
       (set! r12 L.b.1) ;; TODO
       ;; (compare loc/reg opand/loc/fvar)
       (compare fv1 r9)
       ;; (jump-if relop trg/loc/fvar)
       (jump-if > fv3) ;; TODO
       (halt 1)
       ;; (compare loc/fvar opand/loc/fvar)
       (with-label L.a.1 (compare fv2 fv1))
       ;; (jump-if relop trg/loc/reg)
       (jump-if = r12)
       (halt 2)
       (with-label L.b.1 (halt 42))))

  ;; Use jump-if with all relops with trg being a register
  (check-42
    `(begin
       (set! r9 42)
       ;; =
       (compare r9 42)
       (set! rsi L.a.1)
       (jump-if = rsi)
       (halt 1)
       ;; <
       (with-label L.a.1 (compare r9 100))
       (set! rsi L.b.1)
       (jump-if < rsi)
       (halt 1)
       ;; <=
       (with-label L.b.1 (compare r9 42))
       (set! rsi L.c.1)
       (jump-if <= rsi)
       (halt 1)
       ;; >=
       (with-label L.c.1 (compare r9 42))
       (set! rsi L.d.1)
       (jump-if >= rsi)
       (halt 1)
       ;; >
       (with-label L.d.1 (compare r9 41))
       (set! rsi L.e.1)
       (jump-if > rsi)
       (halt 1)
       ;; !=
       (with-label L.e.1 (compare r9 42))
       (set! rsi L.f.1)
       (jump-if != rsi)
       (halt 42)
       (with-label L.f.1 (halt 0))))

  ;; (compare loc/reg opand/int64) -- doesn't work due to bug @258
  ;; (compare loc/fvar opand/int64) -- same as above


  ;; M2 tests
  (define large-int (add1 (max-int 32)))
  ;; return from immediate
  (check-42
    '(begin (halt 42)))


  ;; return from register rbx
  (check-42
    '(begin
       (set! rbx 42)
       (halt rbx)))

  ;; return from rax
  (check-42
    '(begin
       (set! rax 42)
       (halt rax)))

  ;; binop, dest register, src imm
  (check-42
   '(begin
      (set! r9 40)
      (set! r9 (+ r9 2))
      (halt r9)))

  ;; halt from fvar
  (check-42
    '(begin
       (set! fv1 42)
       (halt fv1)))

  ;; binop with fvar as dest, imm src
  ;; This shouldn't work with interp-paren-x64-fvars-v2
  ;; '(begin (set! fv2 40) (set! fv2 (+ fv2 2)) (set! rax fv2))
  (check-42
    '(begin
       (set! fv2 40)
       (set! fv2 (+ fv2 2))
       (halt fv2)))

  ;; imm64 to fvar move
  (check-42
    `(begin
       (set! fv2 ,large-int)
       (halt 42)))

  ;; fvar to fvar move
  (check-42
    `(begin
       (set! fv1 10)
       (set! fv2 fv1)
       (halt 42)))

  ;; add imm64 to reg
  (check-42
    `(begin
       (set! rsi 0)
       (set! rsi (+ rsi ,large-int))
       (halt 42)))

  ;; add imm64 to fvar
  (check-42
    `(begin
       (set! fv0 0)
       (set! fv0 (+ fv0 ,large-int))
       (halt 42)))
  )
