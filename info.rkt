#lang info

;; General

(define collection "private-in")
(define pkg-desc "Require private bindings.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/private-in.scrbl" ())))
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
