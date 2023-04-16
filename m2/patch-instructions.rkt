#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  "../utils/gen-utils.rkt")

(provide patch-instructions)

;; Milestone 2 Exercise 10
;; Milestone 4 Exercise 5
;; Milestone 6 Exercise 18
;; Milestone 7 Exercise 8
;; Milestone 8 Exercise 12
;;
;; Compile the Para-asm-lang v8 to Paren-x64-mops v8 by patching instructions that
;; have no x64 analogue into to a sequence of instructions and an auxiliary
;; register from current-patch-instructions-registers.
(define/contract (patch-instructions p)
  (-> para-asm-lang-v8? paren-x64-mops-v8?)

  (define/contract (big-int? i)
    (-> any/c boolean?)
    (and (int64? i)
         (not (int32? i))))

  (define/contract (addr? a)
    (-> any/c boolean?)
    (match a
      [`(,fbp - ,dispoffset)
        #:when (and (frame-base-pointer-register? fbp)
                    (dispoffset? dispoffset))
        #t]
      [_ #f]))

  ;; para-asm-lang-v8 -> paren-x64-mops-v8
  (define (patch-instructions-p p)
    (match p
      [`(begin ,s ...)
        `(begin
           ,@(append-map patch-instructions-s s))]))

  ;; Exception struct for when there are no more patch registers
  ;; Constructor: string continuation-mark-set -> exn:no-more-patch-regs
  (struct exn:no-more-patch-regs exn:fail ())

  ;; -> Exception
  ;; Raise the error for when there are no more patch registers
  (define (raise-no-more-patch-regs)
    (raise
      (exn:no-more-patch-regs
        "No more patch registers"
        (current-continuation-marks))))

  ;; Syntax:
  ;; (use-tmp
  ;;   ([triv (-> triv boolean)] ...)
  ;;   (-> triv paren-x64-mops-v8-s))
  ;; -> (List-of paren-x64-mops-v8-s) or exn:no-more-patch-regs
  ;;
  ;; triv ::= label|int64|reg|addr
  ;;
  ;; Use patch registers for unsupported 's'.
  ;; 'create-s' will be called where each arg will either be the value in 'vals' or
  ;; a patch register if the check? in checks returns true (i.e. it is unsupported).
  ;; Returns the list with 'set!' for setting the unsupported 'vals' to patch registers
  ;; and the 'set!' created by 'create-s'.
  ;; Raises exn:no-more-patch-regs if we run out of patch registers.
  (define-syntax-rule (use-tmp ([vals checks] ...) create-s)
    (use-tmp-impl (list vals ...) (list checks ...) create-s))

  ;; (List-of triv)
  ;; (List-of (triv -> boolean))
  ;; (triv -> paren-x64-mops-v8-s)
  ;; -> (List-of paren-x64-mops-v8-s) or exn:no-more-patch-regs
  ;;
  ;; triv ::= label|int64|reg|addr
  ;;
  ;; Use patch registers for unsupported 's'. See above; do not call directly!
  ;; Raises exn:no-more-patch-regs if we run out of patch registers.
  (define (use-tmp-impl vals checks create-s)
    (for/fold ([new-vals '()]
               [set-list '()]
               [regs (current-patch-instructions-registers)]
               #:result `(,@set-list ,@(apply create-s new-vals)))
              ([val vals]
               [check? checks])
      (cond
        [(check? val)
         (when (empty? regs) (raise-no-more-patch-regs))
         (values
           (append new-vals (list (car regs)))
           (append set-list `((set! ,(car regs) ,val)))
           (cdr regs))]
        [else
         (values
           (append new-vals (list val))
           set-list
           regs)])))

  ;; addr addr|int64 addr|int64
  ;; -> (List-of paren-x64-mops-v8-s) or exn:no-more-patch-regs
  ;; Resolve running out of patch-instructions-registers in patching memset!
  ;; by adding the reg and the index together and freeing a patch register.
  ;; Raises exn:no-more-patch-regs if we run out of patch registers.
  (define (resolve-memset-patch loc index triv)
    (unless (>= (length (current-patch-instructions-registers)) 2)
      (raise-no-more-patch-regs))
    (define tmp0 (first (current-patch-instructions-registers)))
    (define tmp1 (second (current-patch-instructions-registers)))
    `((set! ,tmp0 ,loc)
      (set! ,tmp1 ,index)
      (set! ,tmp0 (+ ,tmp0 ,tmp1))
      (set! ,tmp1 ,triv)
      (mset! ,tmp0 0 ,tmp1)))

  ;; Use tmp for the location receiving the value
  ;; loc (loc -> boolean?) (loc -> (List-of paren-x64-mops-v8-s))
  ;; -> (List-of paren-x64-mops-v8-s)
  (define (use-tmp-rec loc check? fn)
    (if (check? loc)
        (let ([tmp (first (current-patch-instructions-registers))])
          `(,@(fn tmp)
            (set! ,loc ,tmp)))
        (fn loc)))

  ;; para-asm-lang-v8-s -> paren-x64-mops-v8-s
  (define (patch-instructions-s s)
    (match s
      [`(set! ,loc1 (mref ,loc2 ,index))
        ;; para-asm-lang-v8        | Need tmp?
        ;; ------------------------|----------
        ;; reg   reg   reg|int32   | N
        ;; addr  _     _           | Y
        ;; _     addr  _           | Y
        ;; _     _     addr|int64  | Y
        (use-tmp
          ([loc2 addr?]
           [index (or-checks addr? big-int?)])
          (lambda (r2 i)
            (use-tmp-rec
              loc1
              addr?
              (lambda (r1)
                `((set! ,r1 (mref ,r2 ,i)))))))]
      [`(set! ,loc (,b ,loc ,o))
        ;; para-asm-lang-v8        | Need tmp for o?
        ;; ------------------------|----------
        ;; loc/reg  opand/int32    | N
        ;; loc/reg  opand/int64    | Y
        ;; loc/reg  opand/loc/reg  | N
        ;; loc/reg  opand/loc/addr | N
        ;; loc/addr opand/int32    | N
        ;; loc/addr opand/int64    | Y
        ;; loc/addr opand/loc/reg  | N
        ;; loc/addr opand/loc/addr | N
        ;; always need a tmp for loc if addr
        (use-tmp
          ([loc addr?]
           [o big-int?])
          (lambda (r o)
            `((set! ,r (,b ,r ,o))
              ,@(if (addr? loc) `((set! ,loc ,r)) '()))))]
      [`(set! ,loc ,triv)
        ;; para-asm-lang-v8             | Need tmp?
        ;; -----------------------------|----------
        ;; loc/reg  triv/opand/int64    | N
        ;; loc/reg  triv/opand/loc/reg  | N
        ;; loc/reg  triv/opand/loc/addr | N
        ;; loc/reg  triv/label)         | N
        ;; loc/addr triv/opand/int32    | N
        ;; loc/addr triv/opand/int64    | Y
        ;; loc/addr triv/opand/loc/reg  | N
        ;; loc/addr triv/opand/loc/addr | Y
        ;; loc/addr triv/label          | N
        (use-tmp
          ([triv
            (lambda (t)
              (and (addr? loc)
                   (or (big-int? t) (addr? t))))])
          (lambda (v) `((set! ,loc ,v))))]
      [`(mset! ,loc ,index ,triv)
        ;; para-asm-lang-v8                 | Need tmp?
        ;; ---------------------------------|----------
        ;; reg   reg|int32 reg|label|int32  | N
        ;; addr  _         _                | Y
        ;; _     addr      _                | Y
        ;; _     int64     _                | Y
        ;; _     _         addr             | Y
        ;; _     _         int64            | Y
        (with-handlers ([exn:no-more-patch-regs?
                         (lambda (e) (resolve-memset-patch loc index triv))])
          (use-tmp
            ([loc addr?]
             [index (or-checks big-int? addr?)]
             [triv (or-checks big-int? addr?)])
            (lambda (reg index iort)
              `((mset! ,reg ,index ,iort)))))]
      [`(jump ,trg)
        ;; label, reg, or addr -> reg or label
        (use-tmp
          ([trg addr?])
          (lambda (v) `((jump ,v))))]
      [`(with-label ,l ,s)
        (define stmts (patch-instructions-s s))
        `((with-label ,l ,(car stmts))
          ,@(cdr stmts))]
      [`(compare ,l ,o)
        (use-tmp
          ;; l must be a reg
          ([l addr?]
           ;; @278
           [o (or-checks addr? big-int?)])
          (lambda (r o) `((compare ,r ,o))))]
      [`(jump-if ,r ,t)
        (if (label? t)
          (list s)
          (let ([tmp-label (fresh-label 'tmp)])
            `((jump-if ,(invert-relop r) ,tmp-label)
              ;; paren-x64-fvars-v4 jump only supports a label or reg
              ;; so send it through again
              ,@(patch-instructions-s `(jump ,t))
              ;; some noop
              (with-label ,tmp-label (set! rax rax)))))]))


  ;; not used
  #;
  (define (patch-instructions-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  ;; not used
  #;
  (define (patch-instructions-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc
        (void)]))

  ;; not used
  #;
  (define (patch-instructions-trg t)
    (match t
      [(? label?)
       (void)]
      [loc
       (void)]))

  ;; not used
  #;
  (define (patch-instructions-loc l)
    (match l
      [(? register?)
       (void)]
      [addr (void)]))

  ;; not used
  #;
  (define (patch-instructions-index i)
    (match i
      [(? int64?)
       (void)]
      [loc (void)]))

  ;; not used
  #;
  (define (patch-instructions-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]
      ['bitwise-and (void)]
      ['bitwise-ior (void)]
      ['bitwise-xor (void)]
      ['arithmetic-shift-right (void)]))

  ;; relop -> relop
  (define (invert-relop r)
    (match r
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
  (define 64-bit-int (max-int 64))
  (define rax (current-return-value-register))

  (define-check (check-42 p)
    (check-equal?
      (interp-paren-x64-mops-v8 (patch-instructions p))
      42))

  (define/contract (fbp o)
    (-> dispoffset? any/c)
    `(,(current-frame-base-pointer-register) - ,o))

  ;; M6 tests

  (check-42
    ;; (set! loc/reg triv/opand/int32)
    `(begin
      (set! ,rax 42)
      (jump done)))

  (check-42
    `(begin
       ;; (set! loc/reg triv/opand/int64)
       (set! ,rax ,(* 2 big-int))
       ;; (set! loc/reg (binop loc/reg opand/int64))
       (set! ,rax (- ,rax ,(- (* 2 big-int) 42)))
       (jump done)))

  (check-42
    `(begin
       (set! rcx 42)
       ;; (set! loc/reg triv/opand/loc/reg)
       (set! ,rax rcx)
       (jump done)))

  (check-42
    `(begin
       ;; (set! loc/addr triv/opand/int32)
       (set! ,(fbp 0) 42)
       ;; (set! loc/reg triv/opand/loc/addr)
       (set! ,rax ,(fbp 0))
       (jump done)))

  (check-42
    `(begin
       ;; (set! loc/addr triv/opand/int64)
       (set! ,(fbp 0) ,(* 2 big-int))
       ;; (set! loc/addr (binop loc/addr opand/int64))
       (set! ,(fbp 0) (- ,(fbp 0) ,(- (* 2 big-int) 42)))
       (set! rax ,(fbp 0))
       (jump done)))

  (check-42
    `(begin
       (set! rcx 7)
       ;; (set! loc/reg (binop loc/reg opand/int32))
       (set! rcx (* rcx 3))
       ;; (set! loc/addr triv/opand/loc/reg)
       (set! ,(fbp 0) rcx)
       ;; (set! loc/addr triv/opand/loc/addr)
       (set! ,(fbp 8) ,(fbp 0))
       ;; (set! loc/addr (binop loc/addr opand/int32))
       (set! ,(fbp 8) (* ,(fbp 8) 2))
       (set! ,rax ,(fbp 8))
       (jump done)))

  (check-42
    `(begin
       (set! r8 1)
       (set! r9 2)
       ;; (set! loc/reg (binop loc/reg opand/loc/reg))
       (set! r9 (+ r9 r8))
       (set! ,(fbp 0) 3)
       ;; (set! loc/reg (binop loc/reg opand/loc/addr))
       (set! r9 (+ r9 ,(fbp 0)))
       (set! ,(fbp 0) r9)
       (set! rcx 4)
       ;; (set! loc/addr (binop loc/addr opand/loc/reg))
       (set! ,(fbp 0) (+ ,(fbp 0) rcx))
       (set! ,(fbp 8) 5)
       ;; (set! loc/addr (binop loc/addr opand/loc/addr))
       (set! ,(fbp 0) (* ,(fbp 0) ,(fbp 8)))
       (set! ,(fbp 8) 8)
       (set! ,(fbp 0) (- ,(fbp 0) ,(fbp 8)))
       (set! ,rax ,(fbp 0))
       (jump done)))

  (check-42
    `(begin
       (set! ,rax 0)
       ;; (set! loc/reg triv/label)
       (set! rcx L.a.1)
       ;; (set! loc/addr triv/label)
       (set! ,(fbp 0) L.b.1)
       ;; (jump trg/loc/reg)
       (jump rcx)
       (set! ,rax (+ ,rax 1))
       ;; (with-label label s)
       ;; (jump trg/loc/addr)
       (with-label L.a.1 (jump ,(fbp 0)))
       (set! ,rax (+ ,rax 1))
       ;; (jump trg/label)
       (with-label L.b.1 (jump L.c.1))
       (set! ,rax (+ ,rax 1))
       (with-label L.c.1 (set! rsi 10))
       (compare rsi 10)
       ;; (jump-if relop trg/label)
       (jump-if = L.done.1)
       (set! ,rax (+ ,rax 1))
       (with-label L.done.1 (set! ,rax (+ ,rax 42)))
       (jump done)))


  (check-42
    `(begin
       (set! r9 ,big-int)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,rax 0)
       ;; (compare loc/reg opand/int64)
       (compare r9 ,big-int)
       ;; relop=/true
       (jump-if = rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9, 10))
       ;; relop=/false
       (jump-if = rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,(fbp 0) 10)
       (set! rcx 10)
       (set! r9 8)
       ;; (compare loc/reg opand/loc/reg)
       (compare r9 rcx)
       ;; relop</true
       (jump-if < rsi)
       (set! ,rax (+ ,rax 1))
       ;; (compare loc/reg opand/loc/addr)
       (with-label L.a.1 (compare rcx ,(fbp 0)))
       ;; relop</false
       (jump-if < rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! r9 ,(sub1 big-int))
       ;; (compare loc/addr opand/int64)
       (compare r9 ,big-int)
       ;; relop<=/true
       (jump-if <= rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 2))
       ;; relop<=/false
       (jump-if <= rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,(fbp 0) 10)
       (set! ,(fbp 8) 20)
       (set! r9 5)
       ;; (compare loc/addr opand/loc/reg)
       (compare ,(fbp 0) r9)
       ;; relop>=/true
       (jump-if >= rsi)
       (set! ,rax (+ ,rax 1))
       ;; (compare loc/addr opand/loc/addr)
       (with-label L.a.1 (compare ,(fbp 0) ,(fbp 8)))
       ;; relop>=/false
       (jump-if >= rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! r9 10)
       (set! ,(fbp 0) L.done.1)
       (compare r9 4)
       ;; relop>/true
       (jump-if > rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 r9))
       ;; relop>/false
       ;; (jump-if relop trg/loc/addr)
       (jump-if > ,(fbp 0))
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  (check-42
    `(begin
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,rax 0)
       (set! r9 10)
       (compare r9 4)
       ;; relop!=/true
       (jump-if != rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 r9))
       ;; relop!=/false
       ;; (jump-if relop trg/loc/reg)
       (jump-if != rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  ;; Verify bitwise operations with 64-bit integers
  (check-42
    `(begin
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,rax 0)
       (set! r9 10)
       (set! r9 (bitwise-xor r9 r9))          ;; set to 0
       (set! r9 (bitwise-ior r9 2))           ;; set to 2
       (set! r9 (arithmetic-shift-right r9 2));; multiply by 4
       (set! r9 (bitwise-ior r9 2))           ;; add 2 since lower 2 bits are 0
       (set! r9 (bitwise-and r9 ,64-bit-int)) ;; bitwise-and with 64 bit all-ones integer
       (compare r9 4)
       ;; relop!=/true
       (jump-if != rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 r9))
       ;; relop!=/false
       ;; (jump-if relop trg/loc/reg)
       (jump-if != rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))
       (jump done)))

  (parameterize ([current-patch-instructions-registers '()])
    (check-exn
      exn:fail?
      (lambda ()
        (patch-instructions
          `(begin
             (set! ,(fbp 0) ,big-int)
             (set! ,rax ,(fbp 0))
             (jump done))))))

  ;; Check that registers are patch-instructions-registers
  (define (patch-regs? . rs)
    (andmap
      (lambda (x)
        (set-member? (current-patch-instructions-registers) x))
      rs))

  ;; mset!, mref offset int64
  (check-match
    (patch-instructions
      `(begin
        (mset! r12 ,big-int 10)
        (mset! r12 ,(+ big-int 8) ,big-int)
        (set! rax (mref r12 ,big-int))
        (jump done)))
    `(begin
      (set! ,tmp0 ,v0)
      (mset! r12 ,tmp0 10)
      (set! ,tmp1 ,v1)
      (set! ,tmp2 ,v0)
      (mset! r12 ,tmp1 ,tmp2)
      (set! r10 ,v0)
      (set! rax (mref r12 r10))
      (jump done))
    (and (equal? v0 big-int)
         (equal? v1 (+ big-int 8))
         (not (equal? tmp1 tmp2))
         (patch-regs? tmp0 tmp1 tmp2)))

  ;; mset! all three require temp => base + index
  (check-match
    (patch-instructions
      `(begin
        (mset! (rbp - 0) (rbp - 8) (rbp - 16))))
    `(begin
      (set! ,tmp0 (rbp - 0))
      (set! ,tmp1 (rbp - 8))
      (set! ,tmp0 (+ ,tmp0 ,tmp1))
      (set! ,tmp1 (rbp - 16))
      (mset! ,tmp0 0 ,tmp1))
    (and (not (equal? tmp0 tmp1))
         (patch-regs? tmp0 tmp1)))

  ;; mset!
  (check-42
    `(begin
      (set! rcx 8)
      (set! rdx -15)
      (set! ,(fbp 0) 24)
      (set! ,(fbp 8) 64)
      (set! ,(fbp 16) r12)
      (set! ,(fbp 16) (+ ,(fbp 16) 88))
      (set! ,(fbp 24) 7)
      (set! ,(fbp 32) 2)
      (mset! r12 rcx rdx)                        ;[00+08]: 15
      (mset! r12 ,(fbp 0) done)                  ;[00+24]: done
      (mset! r12 32 ,(fbp 32))                   ;[00+32]: 2
      (mset! r12 ,(fbp 8) ,(fbp 24))             ;[00+64]: 7
      (mset! ,(fbp 16) 8 177)                    ;[88+08]: 177
      (mset! ,(fbp 16) ,(fbp 0) ,(+ big-int 16)) ;[88+24]: 2147483664
      (set! rax (mref r12 112))   ;; rax:2147483664
      (set! rax (- rax ,big-int)) ;; rax:16
      (set! rcx (mref r12 8))
      (set! rax (- rax rcx))      ;; rax:16-(-15)=31
      (set! rcx (mref r12 64))
      (set! rax (* rax rcx))      ;; rax:31*7=217
      (set! rcx (mref r12 96))
      (set! rax (- rax rcx))      ;; rax:217-177=40
      (set! rcx (mref r12 32))
      (set! rax (+ rax rcx))      ;; rax:40+2=42
      (set! rdx (mref r12 24))
      (jump rdx)))

  ;; mref w/ reg & int32
  (check-42
    `(begin
      (set! rdx 8)
      (mset! r12 8 51)
      (mset! r12 16 9)
      (set! rax (mref r12 rdx))
      (set! rcx (mref r12 16))
      (set! rax (- rax rcx))
      (jump done)))

  ; mref w/ addr
  (check-42
    `(begin
      (set! rdx 8)
      (set! ,(fbp 0) 8)
      (set! ,(fbp 8) r12)
      (set! ,(fbp 16) 32)
      (mset! r12 8 9)
      (mset! r12 16 2)
      (mset! r12 24 6)
      (mset! r12 32 done)
      (set! rax (mref r12 ,(fbp 0)))
      (set! rcx (mref ,(fbp 8) 16))
      (set! rax (- rax rcx))
      (set! ,(fbp 64) (mref r12 24))
      (set! rax (* rax ,(fbp 64)))
      (set! ,(fbp 72) (mref ,(fbp 8) ,(fbp 16)))
      (jump ,(fbp 72))))
  )
