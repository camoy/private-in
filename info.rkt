#lang info

;; General

(define collection "require-private")
(define pkg-desc "Require private bindings.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/require-private.scrbl" ())))
(define compile-omit-paths '("test"))

;; Dependencies

(define deps
  '("base"))

(define build-deps
  '("chk-lib"
    "rackunit-doc"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"))
