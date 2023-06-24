#lang racket/base

;; TODO: Does the hash actually save time,
;; since we have symbol->immutable-string now
;; and don't have to allocate?
;; Should do a benchmark ...

(require racket/random
         racket/symbol
         racket/contract
         "private/variant-and-version.rkt"
         (only-in file/sha1
                  bytes->hex-string))

(provide uuid-symbol?
         uuid-string?
         strict-uuid-string?
         (contract-out
          [uuid-symbol
           (-> symbol?)] ;; uuid-symbol?
          [uuid-string
           (-> (and/c string? immutable?))] ;; strict-uuid-string?
          [uuid-string->symbol
           (-> uuid-string? symbol?)] ;; uuid-symbol?
          ))

(module+ test
  (require rackunit
           (submod "..")))

(define (uuid-symbol)
  (define s (string->symbol (uuid-generate*)))
  (hash-set! known-uuid-symbols s #t)
  s)

(define (uuid-string)
  (string->immutable-string (uuid-generate*)))

(define (uuid-string->symbol u)
  (let ([u (string->immutable-string u)])
    (unless (uuid-string? u)
      (raise-argument-error 'uuid-string->symbol
                            "uuid-string?"
                            u))
    (define sym (string->symbol (string-foldcase u)))
    (hash-set! known-uuid-symbols sym #t)
    sym))

(module+ test
  (check-eq? (uuid-string->symbol "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
             'f81d4fae-7dec-11d0-a765-00a0c91e6bf6)
  (check-eq? (uuid-string->symbol "F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6")
             'f81d4fae-7dec-11d0-a765-00a0c91e6bf6)
  (check-eq? (uuid-string->symbol "f81D4fAE-7dec-11D0-A765-00a0C91e6bf6")
             'f81d4fae-7dec-11d0-a765-00a0c91e6bf6))

(module+ test
  (define-simple-check (check-not-pred pred v)
    (not (pred v))))

(define uuid-px
  #px"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(define uuid-ci-px
  #px"^[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}$")

(define type4-uuid-px
  #px"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")

(define known-uuid-symbols
  (make-weak-hasheq))

(define (uuid-symbol? v)
  (cond
    [(hash-has-key? known-uuid-symbols v)]
    [(and (symbol? v)
          (regexp-match? uuid-px (symbol->immutable-string v)))
     (hash-set! known-uuid-symbols v #t)
     #t]
    [else
     #f]))

(module+ test
  (check-pred uuid-symbol?
              'f81d4fae-7dec-11d0-a765-00a0c91e6bf6)
  (check-not-pred uuid-symbol?
                  'F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6)
  (check-not-pred uuid-symbol?
                  'f81D4fAE-7dec-11D0-A765-00a0C91e6bf6))

(define (uuid-string? v)
  (and (string? v) (regexp-match? uuid-ci-px v)))

(module+ test
  (check-pred uuid-string?
              "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
  (check-pred uuid-string?
              "F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6")
  (check-pred uuid-string?
              "f81D4fAE-7dec-11D0-A765-00a0C91e6bf6"))

(define (strict-uuid-string? v)
  (and (string? v) (regexp-match? uuid-px v)))

(module+ test
  (check-pred strict-uuid-string?
              "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
  (check-not-pred strict-uuid-string?
                  "F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6")
  (check-not-pred strict-uuid-string?
                  "f81D4fAE-7dec-11D0-A765-00a0C91e6bf6"))

;; time-low "-" time-mid "-" time-high-and-version "-"
;;   clock-seq-and-reserved clock-seq-low "-" node

;; time-low               = 4hexOctet
;; time-mid               = 2hexOctet
;; time-high-and-version  = 2hexOctet

;; clock-seq-and-reserved = hexOctet
;; clock-seq-low          = hexOctet
;; node                   = 6hexOctet

;; hexOctet               = hexDigit hexDigit

;; urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6

;; Each field is treated as an integer and has its value printed as a
;; zero-filled hexadecimal digit string with the most significant
;; digit first.  The hexadecimal values "a" through "f" are output as
;; lower case characters and are case insensitive on input.

(define (uuid-bytes->string [bs (crypto-random-bytes 16)])  
  (define (extract-hex start [end start])
    (bytes->hex-string (subbytes bs start (add1 end))))
  (string-append
   (extract-hex 0 3) "-" (extract-hex 4 5) "-"
   (extract-hex 6 7) "-"
   (extract-hex 8) (extract-hex 9) "-"
   (extract-hex 10 15)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uuid-generate* #:libuuid? [libuuid? #f])
  (define bs
    (crypto-random-bytes 16))
  (bytes-set-variant-and-version4! bs #:libuuid? libuuid?)
  (uuid-bytes->string bs))

(module+ test
  (for ([i (in-range 100)])
    (check-regexp-match
     type4-uuid-px
     (uuid-generate*))))

