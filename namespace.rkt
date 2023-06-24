#lang racket/base

(provide dns url oid x500)

;; Appendix C.  Appendix C - Some Name Space IDs
;;
;; This appendix lists the name space IDs for some potentially
;; interesting name spaces, as initialized C structures and in the
;; string representation defined above.
;;
;; /* Name string is a fully-qualified domain name */
(define dns '6ba7b810-9dad-11d1-80b4-00c04fd430c8)
;;
;; /* Name string is a URL */
(define url '6ba7b811-9dad-11d1-80b4-00c04fd430c8)
;;
;; /* Name string is an ISO OID */
(define oid '6ba7b812-9dad-11d1-80b4-00c04fd430c8)
;;
;; /* Name string is an X.500 DN (in DER or a text output format) */
(define x500 '6ba7b814-9dad-11d1-80b4-00c04fd430c8)
