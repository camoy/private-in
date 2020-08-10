#lang scribble/manual

@require[@for-label[require-private
                    rackunit
                    (except-in racket/base require)]]

@title{Require Private}
@author{Cameron Moy}

@defmodule[require-private]

This module provides functionality like @code{require/expose},
but as a require spec instead.
Importing private bindings
should be considered @bold{unsafe}---use
at your own risk.

@defform[(private-in module-path)]{
  Imports provided bindings from @code{module-path}
  and unexported run-time and transformer bindings
  defined in the module.
}

@defform[(require require-spec ...)]{
  You must use the @code{require} exported by this module
  instead of the base @code{require}.
}
