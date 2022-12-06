#lang info

(define pkg-name "uuid")
(define collection "uuid")
(define pkg-desc "Generate random UUIDs in pure Racket")
(define version "0.1")
(define pkg-authors '(philip))
(define license 'Apache-2.0)

(define deps '(["base" #:version "7.6"]))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))

(define scribblings '(("scribblings/uuid.scrbl" ())))
