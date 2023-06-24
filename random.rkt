#lang racket/base

(require racket/random
         racket/symbol
         racket/contract
         "base.rkt"
         (submod "base.rkt" private)
         "private/variant-and-version.rkt"
         "private/hex.rkt")

(provide (contract-out
          [uuid-symbol
           (-> symbol?)] ; uuid-symbol?
          [uuid-string
           (-> (and/c string? immutable?))])) ; strict-uuid-string?

(module+ test
  (require #;(submod "..") ; TODO: enforce contracts again once we're ready to expose #:libuuid?
           rackunit))

(define-uuid-generators (uuid-symbol #:libuuid? [libuuid? #f])
  #:string uuid-string
  (bytes-set-variant-and-version4! (crypto-random-bytes 16)
                                   #:libuuid? libuuid?))

(define type4-uuid-px
  #px"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
(define type4-uuid-px/libuuid
  #px"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89][0-9a-f]{3}-[0-9a-f]{12}$")

(module+ test
  (for ([i (in-range 100)])
    (check-regexp-match
     type4-uuid-px
     (uuid-string))
    (check-regexp-match
     type4-uuid-px/libuuid
     (uuid-string #:libuuid? #t))))
