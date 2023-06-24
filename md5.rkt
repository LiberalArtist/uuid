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
