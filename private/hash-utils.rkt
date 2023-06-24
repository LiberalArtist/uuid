#lang racket/base

(provide define-hash-uuid-builders)

(require racket/symbol
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "../base.rkt"
         (submod "../base.rkt" private))

(define known-namespaces
  (make-weak-hasheq))

(define (uuid->namespace-bytes u)
  (cond
    [(string? u)
     (string->16bytes (string-foldcase u))]
    [(hash-ref known-namespaces u #f)]
    [else
     (define bs
       (datum-intern-literal
        (string->16bytes (symbol->immutable-string u))))
     (hash-set! known-namespaces u bs)
     bs]))

(define (build-hash-uuid ns-uuid bs bsvav! hash-bytes #:libuuid? [libuuid? #f])
  (bsvav! (hash-bytes (open-input-bytes (bytes-append (uuid->namespace-bytes ns-uuid) bs)))
          #:libuuid? libuuid?))

(define-syntax define-hash-uuid-builders
  (syntax-parser
    [(_ algo:id bsvav!-proc:expr hash-bytes-proc:expr)
     #:with bsvav! #'bytes-set-variant-and-version!
     #:with hash-bytes (format-id #'xyz "~a-bytes-for-uuid" #'algo)
     #:with (build-algo-uuid build-algo-uuid-string)
     (for/list ([sfx '("" "-string")])
       (format-id #'algo "build-~a-uuid~a" #'algo sfx #:subs? #t))
     #`(begin
         (define bsvav! bsvav!-proc)
         (define hash-bytes hash-bytes-proc)
         (define-uuid-generators (build-algo-uuid ns-uuid bs #:libuuid? [libuuid? #f])
           #:string build-algo-uuid-string
           (build-hash-uuid ns-uuid bs bsvav! hash-bytes #:libuuid? [libuuid? #f])))]))
