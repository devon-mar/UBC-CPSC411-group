# CPSC 411 Milestone 6 Skeleton
This branch provides the skeleton for CPSC 411 Milestone 6.

When submitting, you should ensure the name of the `compiler.rkt` file and the
provides from that file are the same as in this commit.
You may change the code base in any other way, including moving code to other
files, as long as `compiler.rkt` reprovides the appropriate functions.
You can check this is the case by running `raco test interface-test.rkt`.

To submit, make sure you most recent work is merged into this branch and this
branch is pushed.

For versions of the skeletons from prior versions of the course, see the tags.

## Exercise Locations

### Milestone 2

| Exercise | Pass                    | File                         |
|----------|-------------------------|------------------------------|
| 1        | `uniquify`             |  `m2/uniquify.rkt`            |
| 2        | `sequentialize-let`    |  `m2/sequentialize-let.rkt`   |
| 3        | `normalize-bind`       |  `m2/normalize-bind.rkt`      |
| 4        | `select-instructions`  |  `m2/select-instructions.rkt` |
| 5        | `assign-homes`         |  `m2/assign-homes.rkt`        |
| 6        | `uncover-locals`       |  `m2/uncover-locals.rkt`      |
| 7        | `assign-fvars`         |  `m2/assign-fvars.rkt`        |
| 8        | `replace-locations`    |  `m2/replace-locations.rkt`   |
| 9        | `flatten-begins`       |  `m2/flatten-begins.rkt`      |
| 10       | `patch-instructions`   |  `m2/patch-instructions.rkt`  |
| 11       | `implement-fvars`      |  `m2/implement-fvars.rkt`     |
| 12       | `generate-x64`         |  `m2/generate-x64.rkt`        |
| 15       | `check-paren-x64`      |  `m2/check-paren-x64.rkt`     |
| 16       | `interp-paren-x64`     |  `m2/interp-paren-x64.rkt`    |
| 17       | `check-values-lang`    |  `m2/check-values-lang.rkt`   |
| 18       | `interp-values-lang`   |  `m2/interp-values-lang.rkt`  |

### Milestone 3

| Exercise | Pass                | File                       |
|----------|---------------------|----------------------------|
| 1        | `undead-analysis`   | `m3/undead-analysis.rkt`   |
| 2        | `conflict-analysis` | `m3/conflict-analysis.rkt` |
| 3        | `assign-registers`  | `m3/assign-registers.rkt`  |
| 4        | `assign-homes-opt`  | `m3/assign-homes-opt.rkt`  |
| 5        | `compile-m2`        | `m3/compile-m2.rkt`        |
| 5        | `compile-m3`        | `m3/compile-m3.rkt`        |
| 6        | `bury-dead`         | `m3/bury-dead.rkt`         |

### Milestone 4

| Exercise | Pass                  | File                         |
|----------|-----------------------|------------------------------|
| 1        | `generate-x64`        | `m2/generate-x64.rkt`        |
| 2        | `link-paren-x64`      | `m4/link-paren-x64.rkt`      |
| 3        | `interp-paren-x64`    | `m2/interp-paren-x64.rkt`    |
| 4        | `implement-fvars`     | `m2/implement-fvars.rkt`     |
| 5        | `patch-instructions`  | `m2/patch-instructions.rkt`  |
| 6        | `flatten-program`     | `m4/flatten-program.rkt`     |
| 7        | `inline-jumps`        | `m4/inline-jumps.rkt`        |
| 8        | `trace-schedule`      | `m4/trace-schedule.rkt`      |
| 9        | `resolve-predicates`  | `m4/resolve-predicates.rkt`  |
| 10       | `expose-basic-blocks` | `m4/expose-basic-blocks.rkt` |
| 11       | `optimize-predicates` | `m4/optimize-predicates.rkt` |
| 12       | `replace-locations`   | `m2/replace-locations.rkt`   |
| 13       | `assign-registers`    | `m3/assign-registers.rkt`    |
| 14       | `conflict-analysis`   | `m3/conflict-analysis.rkt`   |
| 15       | `undead-analysis`     | `m3/undead-analysis.rkt`     |
| 16       | `uncover-locals`      | `m2/uncover-locals.rkt`      |
| 17       | `select-instructions` | `m2/select-instructions.rkt` |
| 18       | `sequentialize-let`   | `m2/sequentialize-let.rkt`   |
| 19       | `normalize-bind`      | `m2/normalize-bind.rkt`      |
| 20       | `uniquify`            | `m2/uniquify.rkt`            |
| 21       | `interp-values-lang`  | `m2/interp-values-lang.rkt`  |
| 22       | `check-values-lang`   | `m2/check-values-lang.rkt`   |

### Milestone 5

| Exercise | Pass                         | File                                |
|----------|------------------------------|-------------------------------------|
| 1        | `check-values-lang`          | `m5/check-values-lang.rkt`          |
| 2        | `uniquify`                   | `m2/uniquify.rkt`                   |
| 3        | `sequentialize-let`          | `m2/sequentialize-let.rkt`          |
| 4        | `normalize-bind`             | `m2/normalize-bind.rkt`             |
| 5        | `impose-calling-conventions` | `m5/impose-calling-conventions.rkt` |
| 6        | `select-instructions`        | `m2/select-instructions.rkt`        |
| 7        | `uncover-locals`             | `m2/uncover-locals.rkt`             |
| 8        | `undead-analysis`            | `m3/undead-analysis.rkt`            |
| 9        | `conflict-analysis`          | `m3/conflict-analysis.rkt`          |
| 10       | `assign-registers`           | `m3/assign-registers.rkt`           |
| 11       | `replace-locations`          | `m2/replace-locations.rkt`          |
| 12       | `optimize-predicates`        | `m4/optimize-predicates.rkt`        |
| 13       | `expose-basic-blocks`        | `m4/expose-basic-blocks.rkt`        |

### Milestone 6

| Exercise | Pass                           | File                                  |
|----------|--------------------------------|---------------------------------------|
| 1        | `check-values-lang`            | `m2/check-values-lang.rkt`            |
| 2        | `uniquify`                     | `m2/uniquify.rkt`                     |
| 3        | `sequentialize-let`            | `m2/sequentialize-let.rkt`            |
| 4        | `normalize-bind`               | `m2/normalize-bind.rkt`               |
| 5        | `impose-calling-conventions`   | `m5/impose-calling-conventions.rkt`   |
| 6        | `select-instructions`          | `m2/select-instructions.rkt`          |
| 7        | `uncover-locals`               | `m2/uncover-locals.rkt`               |
| 8        | `undead-analysis`              | `m3/undead-analysis.rkt`              |
| 9        | `conflict-analysis`            | `m3/conflict-analysis.rkt`            |
| 10       | `assign-call-undead-variables` | `m6/assign-call-undead-variables.rkt` |
| 11       | `allocate-frames`              | `m6/allocate-frames.rkt`              |
| 12       | `assign-registers`             | `m3/assign-registers.rkt`             |
| 13       | `assign-frame-variables`       | `m6/assign-frame-variables.rkt`       |
| 14       | `replace-locations`            | `m2/replace-locations.rkt`            |
| 15       | `optimize-predicates`          | `m4/optimize-predicates.rkt`          |
| 16       | `expose-basic-blocks`          | `m4/expose-basic-blocks.rkt`          |
| 17       | `implement-fvars`              | `m2/implement-fvars.rkt`              |
| 18       | `patch-instructions`           | `m2/patch-instructions.rkt`           |
| 19       | `generate-x64`                 | `m2/generate-x64.rkt`                 |
