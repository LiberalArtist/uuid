#lang racket/base

(provide bytes-set-variant-and-version1!
         bytes-set-variant-and-version2!
         bytes-set-variant-and-version3!
         bytes-set-variant-and-version4!
         bytes-set-variant-and-version5!)

(require (for-syntax racket/base
                     racket/syntax
                     (rename-in syntax/parse
                                [attribute $]))
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit
           racket/match
           (only-in file/sha1
                    bytes->hex-string)))

(module+ test
  (define (byte->binary b)
    (define base (number->string b 2))
    (define target (- 8 (string-length base)))
    (if (zero? target)
        base
        (string-append-immutable (make-string target #\0) base)))
  (test-case
   "byte->binary"
   (for ([b (in-range 256)])
     (test-case
      (format "b = ~v" b)
      (check-eqv? (string->number (byte->binary b) 2)
                  b)))))


(define (bytes-update! bs i proc)
  (bytes-set! bs i (proc (bytes-ref bs i))))

(define (bytes-set-variant-and-version*! bs bsvn #:libuuid? [libuuid? #f])
  (bytes-update! bs 6 bsvn)
  (bytes-update! bs 8 (λ (b)
                        (byte-set-variant b #:libuuid? libuuid?)))
  bs)

;; 4.1.1 Variant
;;
;; The variant field determines the layout of the UUID.  That is, the
;; interpretation of all other bits in the UUID depends on the setting
;; of the bits in the variant field.  [...]  The variant field consists
;; of a variable number of the most significant bits of octet 8 of the UUID.
;;
;; The following table lists the contents of the variant field, where
;; the letter "x" indicates a "don't-care" value.
;;
;; Msb0  Msb1  Msb2  Description
;;  1     0     x    The variant specified in this document.
;;
;;;;;;;; BUT SEE Erratum 5560:
;;
;; Msb0  Msb1  Msb2  Description
;;  1     0     0    What libuuid does.
;;
;;;; " If Msb2 is a « don't-care » value, this means it's not wrong to set
;;;;   the bit to 0 or 1.  In the case of UUIDv3 and UUIDv5, this does not
;;;;   specify if the bit from the hash output should be left untouched or not.
;;;;   It's not stated that it's illegal to reset it to 0 when setting Msb0
;;;;   and Msb1 altogether (as libuuid does), since it's a « don't-care » value.
;;;;   But letting it untouched whenever it's set to 1 by the hash output (as
;;;;   the Python stdlib does) causes two UUIDv{3,5} to be different for the
;;;;   same input namespaces and data. (Example: NS=Nil UUID, data = 0x44 («D»).
;;;; "

(define (byte-set-variant b #:libuuid? [libuuid? #f])
  (bitwise-ior #b10000000
               (bitwise-and (if libuuid?
                                #b00011111 ; see above re erratum 5560
                                #b00111111)
                            b)))

(module+ test
  (test-case
   "byte-set-variant"
   (for ([libuuid? '(#f #t)])
     (test-case
      (format "#:libuuid? ~v" libuuid?)
      (for ([b (in-range 256)])
        (test-case
         (format "b = ~v" b)
         (define s (byte->binary b))
         (define b* (byte-set-variant b #:libuuid? libuuid?))
         (define s* (byte->binary b*))
         (check-regexp-match (if libuuid?
                                 #rx"^100"
                                 #rx"^10")
                             s*)
         (check-regexp-match (if libuuid?
                                 #rx"^[89]"
                                 #rx"^[89ab]")
                             (bytes->hex-string (bytes b*)))
         (let ([len (if libuuid?
                        3
                        2)])
           (check-equal? (substring s* len)
                         (substring s len)))))))))




;; 4.1.3.  Version
;;
;; The version number is in the most significant 4 bits of the time
;; stamp [erratum ommited].
(define (byte-set-version-number* b vers)
  (bitwise-ior vers
               (bitwise-and #b00001111 b)))
(define-simple-macro (define-version-numbers [4bits:exact-positive-integer
                                              V:id]
                       ...)
  #:with ([byte-set-version-numberV bytes-set-variant-and-versionV!] ...)
  (for/list ([id ($ V)])
    (map (λ (fmt)
           (format-id id fmt id #:subs? #t))
         '("byte-set-version-number~a"
           "bytes-set-variant-and-version~a!")))
  #:with (ms4bits ...) 
  (for/list ([n ($ 4bits)])
    (arithmetic-shift (syntax-e n) 4))
  (begin (~@ (define V 'ms4bits)
             (define (byte-set-version-numberV b)
               (byte-set-version-number* b V))
             (define (bytes-set-variant-and-versionV! bs #:libuuid? [libuuid? #f])
               (bytes-set-variant-and-version*! bs byte-set-version-numberV #:libuuid? libuuid?)))
         ...))
(define-version-numbers
  ;; The following table lists the currently-defined versions for this
  ;; UUID variant.
  ;;
  ;; Msb0  Msb1  Msb2  Msb3   Version  Description
  ;;
  ;;  0     0     0     1        1     The time-based version
  ;;                                   specified in this document.
  [#b0001 \1]
  ;;
  ;;  0     0     1     0        2     DCE Security version, with
  ;;                                   embedded POSIX UIDs.
  [#b0010 \2]
  ;;
  ;;  0     0     1     1        3     The name-based version
  ;;                                   specified in this document
  ;;                                   that uses MD5 hashing.
  [#b0011 \3]
  ;;
  ;;  0     1     0     0        4     The randomly or pseudo-
  ;;                                   randomly generated version
  ;;                                   specified in this document.
  [#b0100 \4]
  ;;
  ;;  0     1     0     1        5     The name-based version
  ;;                                   specified in this document
  ;;                                   that uses SHA-1 hashing.
  [#b0101 \5])

(module+ test
  (test-case
   "byte-set-version-number*"
   (for ([spec `([1 ,\1 ,byte-set-version-number1 #rx"^0001" #rx"^1"]
                 [2 ,\2 ,byte-set-version-number2 #rx"^0010" #rx"^2"]
                 [3 ,\3 ,byte-set-version-number3 #rx"^0011" #rx"^3"]
                 [4 ,\4 ,byte-set-version-number4 #rx"^0100" #rx"^4"]
                 [5 ,\5 ,byte-set-version-number5 #rx"^0101" #rx"^5"])])
     (match-define (list n V bsvn binary-rx decimal-rx) spec)
     (test-case
      (format "version number ~v" n)
      (check-eq? (arithmetic-shift V -4)
                 n)
      (for ([b (in-range 256)])
        (test-case
         (format "b = ~v" b)
         (define s (byte->binary b))
         (define b* (bsvn b))
         (define s* (byte->binary b*))
         (check-regexp-match binary-rx s*)
         (check-regexp-match decimal-rx
                             (bytes->hex-string (bytes b*)))
         (check-equal? (substring s* 4)
                       (substring s 4))))))))
