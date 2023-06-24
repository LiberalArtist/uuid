#lang racket/base

(require racket/contract
         "base.rkt"
         "private/hash-utils.rkt"
         "private/variant-and-version.rkt")

(provide (contract-out
          [build-sha1-uuid
           (->* [(or/c uuid-symbol? uuid-string?)
                 bytes?]
                [#:libuuid? any/c]
                symbol?)] ; uuid-symbol?
          [build-sha1-uuid-string
           (->* [(or/c uuid-symbol? uuid-string?)
                 bytes?]
                [#:libuuid? any/c]
                (and/c string? immutable?))])) ; strict-uuid-string?

(define-hash-uuid-builders sha1
  bytes-set-variant-and-version5!
  (λ (in)
    (subbytes (sha1-bytes in) 0 16)))
