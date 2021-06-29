#lang info

(define collection "piecrust")
(define pkg-desc "Automated CRUD API generation")

(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/piecrust.scrbl" ())))

(define version "0.1")
