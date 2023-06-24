#lang racket/base
;; lifted from file/sha1, and intended to be upstreamed there
(provide bytes->hex-string
         hex-string->bytes)

(define (bytes->hex-string bstr [start 0] [stop (bytes-length bstr)])
  (let* ([len (- stop start)]
         [bstr2 (make-bytes (* len 2))]
         [digit
          (lambda (v)
            (if (v . < . 10)
                (+ v (char->integer #\0))
                (+ v (- (char->integer #\a) 10))))])
    (for ([i (in-range len)]
          [c (in-bytes bstr start stop)])
        (bytes-set! bstr2 (* 2 i) (digit (arithmetic-shift c -4)))
        (bytes-set! bstr2 (+ (* 2 i) 1) (digit (bitwise-and c #xF))))
    (bytes->string/latin-1 bstr2)))

(define (hex-string->bytes s [start 0] [stop (string-length s)])
  (unless (and (string? s) (regexp-match? #px"^([[:xdigit:]]{2})*$" s start stop))
    (raise-argument-error 'hex-string->bytes
                          "(and/c string? #px\"^([[:xdigit:]]{2})*$\")" s))
  
  (define (hex-char->int c)
    (cond ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
          ((char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a))))
          ((char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A))))))
  
  (define bsize (/ (- stop start) 2))
  (define b (make-bytes bsize))
  
  (for ((i (in-range bsize)))
    (define high (hex-char->int (string-ref s (+ start i i))))
    (define low  (hex-char->int (string-ref s (+ start i i 1))))
    (bytes-set! b i (+ (arithmetic-shift high 4) low)))
  
  b)
