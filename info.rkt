#lang info

(define collection "piecrust")

(define pkg-desc "An automated (RESTful) CRUD API generator for use with the Racket web server.")

(define deps '("base"
               "db-lib"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/piecrust.scrbl" ())))

(define version "0.1")
