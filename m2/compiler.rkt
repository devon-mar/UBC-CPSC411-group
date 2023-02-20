#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v2

  "uniquify.rkt"
  "sequentialize-let.rkt"
  "normalize-bind.rkt"
  "select-instructions.rkt"
  "assign-homes.rkt"
  "uncover-locals.rkt"
  "assign-fvars.rkt"
  "replace-locations.rkt"
  "flatten-begins.rkt"
  "patch-instructions.rkt"
  "implement-fvars.rkt"
  "generate-x64.rkt"
  "interp-values-lang.rkt"
  "check-values-lang.rkt"
  "check-paren-x64.rkt"
  "interp-paren-x64.rkt"
)

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 assign-fvars
 replace-locations
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 check-paren-x64
 generate-x64

 interp-values-lang

 interp-paren-x64)

;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
#;
(define-values (check-values-lang
                ;interp-values-lang
                ;uniquify
                ;sequentialize-let
                ;normalize-bind
                ;select-instructions
                ;assign-homes
                ;uncover-locals
                ;assign-fvars
                ;replace-locations
                ;flatten-begins
                ;patch-instructions
                ;implement-fvars
                check-paren-x64
                ;generate-x64
                )
  (values
   ;values
   ;values
   ;values
   ;values
   ; values
   ;values
   ;values
   ;values
   ;values
   ;values
   ;values
   ;values
   ;values
   values
   ;values
   ))


(current-pass-list
 (list
  check-values-lang
  uniquify
  sequentialize-let
  normalize-bind
  select-instructions
  assign-homes
  flatten-begins
  patch-instructions
  implement-fvars
  generate-x64
  wrap-x64-run-time
  wrap-x64-boilerplate))

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/public/v3
   ;; NB: Workaround typo in shipped version of cpsc411-lib
   (except-in cpsc411/langs/v3 values-lang-v3)
   cpsc411/langs/v2)

  (run-tests
   (v3-public-test-sutie
    (current-pass-list)
    (list
     interp-values-lang-v3
     interp-values-lang-v3
     interp-values-unique-lang-v3
     interp-imp-mf-lang-v3
     interp-imp-cmf-lang-v3
     interp-asm-lang-v2
     interp-nested-asm-lang-v2
     interp-para-asm-lang-v2
     interp-paren-x64-fvars-v2
     interp-paren-x64-v2
     #f #f))))
