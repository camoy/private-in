#lang scribble/manual

@require[@for-label[private-in
                    rackunit
                    (except-in racket/base require)]
         scribble/example]

@(define evaluator
  (make-base-eval
    '(require private-in)))

@title{private-in}
@author{Cameron Moy}

@defmodule[private-in]

This module provides functionality like @code{require/expose},
but as a require specification instead.
Importing private bindings
should be considered @bold{unsafe}---use
at your own risk.

@defform[(private-in module-path)]{
  Imports provided bindings from @code{module-path}
  and unexported run-time and transformer bindings
  defined in the module.
  @; We're cheating here because `private-in` is unhappy about resolving
  @; top-level submodules.
  @examples[#:eval evaluator
    (module inner racket/base
      (define foo 42))
    (eval:alts (require (private-in 'inner)) (define foo 42))
    foo]
}

@defform[(require require-spec ...)]{
  You must use the @code{require} exported by this module
  instead of the base @code{require}.
}
