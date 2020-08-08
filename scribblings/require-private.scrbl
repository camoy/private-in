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
  Imports all exported bindings from @code{module-path},
  including unexported run-time bindings.
}

@defform[(only-private-in module-path)]{
  Imports only unexported run-time bindings from @code{module-path}.
}

@defform[(require require-spec ...)]{
  You must use the @code{require} exported by this module
  instead of the base @code{require}.
  Otherwise, the private identifiers will not be defined.
}
