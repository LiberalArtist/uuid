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

(module+ test
  (require (submod "..")
           (prefix-in ns- "namespace.rkt")
           rackunit)
  (for ([libuuid? '(#t #f)])
    (test-case
     (format "#:libuuid? ~v" libuuid?)
     ;; example taken from the Python documentation
     (check-eq? (build-sha1-uuid ns-dns #"python.org")
                '886313e1-3b8a-5372-9b90-0c9aee199e5d))))
