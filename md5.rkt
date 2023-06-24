#lang racket/base

(require openssl/md5
         racket/contract
         "base.rkt"
         "private/hash-utils.rkt"
         "private/variant-and-version.rkt")

(provide (contract-out
          [build-md5-uuid
           (->* [(or/c uuid-symbol? uuid-string?)
                 bytes?]
                [#:libuuid? any/c]
                symbol?)] ; uuid-symbol?
          [build-md5-uuid-string
           (->* [(or/c uuid-symbol? uuid-string?)
                 bytes?]
                [#:libuuid? any/c]
                (and/c string? immutable?))])) ; strict-uuid-string?

(define-hash-uuid-builders md5
  bytes-set-variant-and-version3!
  md5-bytes)

(module+ test
  (require (submod "..")
           (prefix-in ns- "namespace.rkt")
           rackunit)
  (for ([libuuid? '(#t #f)])
    (test-case
     (format "#:libuuid? ~v" libuuid?)
     ;; example taken from the Python documentation
     (check-eq? (build-md5-uuid ns-dns #"python.org" #:libuuid? libuuid?)
                '6fa459ea-ee8a-3ca4-894e-db77e160355e))))
