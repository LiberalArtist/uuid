#lang info

(define pkg-name "uuid")
(define collection "uuid")
(define pkg-desc "Generate random UUIDs in pure Racket")
(define version "0.0")
(define pkg-authors '(philip))

(define deps '(["base" #:version "6.4"]))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "srfi-lite-lib"
                     "rackunit-lib"))

(define scribblings '(("scribblings/uuid.scrbl" ())))
