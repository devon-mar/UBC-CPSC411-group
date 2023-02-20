#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
 
  "m2/uniquify.rkt"
  "m2/sequentialize-let.rkt"
  "m2/normalize-bind.rkt"
  "m2/select-instructions.rkt"
  "m2/assign-homes.rkt"
  "m2/uncover-locals.rkt"
  "m2/assign-fvars.rkt"
  "m2/replace-locations.rkt"
  "m2/flatten-begins.rkt"
  "m2/patch-instructions.rkt"
  "m2/implement-fvars.rkt"
  "m2/generate-x64.rkt"
  "m2/check-paren-x64.rkt"
  "m2/interp-paren-x64.rkt"
  "m2/check-values-lang.rkt"
  "m2/interp-values-lang.rkt"

  "m3/undead-analysis.rkt"
  "m3/conflict-analysis.rkt"
  "m3/assign-registers.rkt"
  "m3/assign-homes-opt.rkt"
  "m3/compile-m2.rkt"
  "m3/compile-m3.rkt")

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 undead-analysis
 conflict-analysis
 assign-registers
 replace-locations
 assign-homes-opt
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 generate-x64

 compile-m2
 compile-m3)

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v3
   cpsc411/langs/v2-reg-alloc
   cpsc411/langs/v2
   cpsc411/test-suite/public/v3
   cpsc411/test-suite/public/v2-reg-alloc)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  ;; If you modify, you must modify the corresponding interpreter in the
  ;; interp-ls, at least by interesting #f as the interpreter for the new pass.
  ;; See the documentation for v3-public-test-suite for details on the structure
  ;; of the interpreter list.
  (current-pass-list (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes-opt
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate))

  (define interp-ls (list
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
                     #f #f))

  (run-tests (v3-public-test-sutie (current-pass-list) interp-ls))
  (run-tests (v2-reg-alloc-public-test-suite undead-analysis conflict-analysis assign-registers)))
