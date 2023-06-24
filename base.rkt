#lang racket/base

(require racket/symbol
         racket/contract
         (prefix-in ns: "namespace.rkt")
         "private/variant-and-version.rkt"
         "private/hex.rkt")

(provide uuid-symbol?
         uuid-string?
         strict-uuid-string?
         (contract-out
          [uuid-string->symbol
           (-> uuid-string? symbol?)])) ; uuid-symbol?

(module+ private
  (provide define-uuid-generators
           string->16bytes))

(module+ test
  (require (submod "..")
           rackunit))

(define known-uuid-symbols
  ;; TODO: Does the hash actually save time,
  ;; since we have symbol->immutable-string now
  ;; and don't have to allocate?
  ;; Should do a benchmark ...
  (make-weak-hasheq `([,ns:dns . #t]
                      [,ns:url . #t]
                      [,ns:oid . #t]
                      [,ns:x500 . #t])))
(define uuid-px
  #px"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(define uuid-ci-px
  #px"^[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}$")

(module+ test
  (define-simple-check (check-not-pred pred v)
    (not (pred v))))

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
                  "f81D4fAE-7dec-11D0-A765-00a0C91e6bf6")
  (check-pred strict-uuid-string?
              (symbol->immutable-string ns:dns))
  (check-pred strict-uuid-string?
              (symbol->immutable-string ns:url))
  (check-pred strict-uuid-string?
              (symbol->immutable-string ns:oid))
  (check-pred strict-uuid-string?
              (symbol->immutable-string ns:x500)))

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
                  'f81D4fAE-7dec-11D0-A765-00a0C91e6bf6)
  (check-pred uuid-symbol? ns:dns)
  (check-pred uuid-symbol? ns:url)
  (check-pred uuid-symbol? ns:oid)
  (check-pred uuid-symbol? ns:x500))

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

(define (16bytes->string bs)  
  (define (extract-hex start [end start])
    (bytes->hex-string bs start (add1 end)))
  (string-append
   (extract-hex 0 3) "-" (extract-hex 4 5) "-"
   (extract-hex 6 7) "-"
   (extract-hex 8) (extract-hex 9) "-"
   (extract-hex 10 15)))

(define (string->16bytes str)
  (define (get a b)
    (hex-string->bytes str a b))
  (bytes-append
   (get 0 8)
   (get 9 13)
   (get 14 18)
   (get 19 23)
   (get 24 36)))

(module+ private
  (require (for-syntax racket/base
                       racket/syntax
                       syntax/parse/lib/function-header
                       syntax/parse))
  (define (finish-uuid-symbol str)
    (define s (string->symbol str))
    (hash-set! known-uuid-symbols s #t)
    s)
  (define-syntax define-uuid-generators
    (syntax-parser
      [(_ (gen-symbol:id . args:formals)
          (~optional (~seq #:string gen-string:id))
          body:expr ...+)
       #:with generate*:id (format-id #'xyz "~a:generate*" #'gen-symbol)
       #`(begin
           (define (generate* . args.params)
             (16bytes->string
              (let ()
                body ...)))
           (~? (define (gen-string . args)
                 (string->immutable-string
                  (generate* . args.params))))
           (define (gen-symbol . args)
             (finish-uuid-symbol
              (generate* . args.params))))])))
