;; ---
;; checks
;; ---

(define-inline (check-positive-fixnum n loc)
  (when (fx< n 0)
    (error loc "bad argument type - not a positive fixnum" n)
    (exit 1)))

(define-inline (check-positive-integer n loc)
  (when (negative? n)
    (error loc "bad argument type - not a positive integer" n)
    (exit 1)))

(define-inline (well-formed-list? l)
  (let loop ((i l))
    (if (list? i)
      (if (pair? i)
        (if (every list? i)
          (let ((l (length (car i))))
            (if (every
                  (lambda (_)
                    (= (length _) l))
                  (cdr i))
              (every loop i)
              #f))
          (every
            (lambda (_)
              (not (list? _)))
            i))
        #f)
      #t)))

;; ---
;; data types
;; ---

(define blob-init!
  (foreign-lambda* void ((blob c) (size_t s) (bool v))
    "memset(c, v ? ~0 : 0, s * sizeof(*c));"))

(define (make-bitset s v)
  (let* ((s (+ (quotient s 8) 1)) (c (make-blob s)))
    (blob-init! c s v)
    c))

(define bitset-ref
  (foreign-lambda* bool ((blob c) (size_t i))
    "C_return(c[i / 8] & 1 << i % 8);"))

(define bitset-set!
  (foreign-lambda* void ((blob c) (size_t i) (bool v))
    "c[i / 8] &= ~(1 << i % 8) | v << i % 8;"))

;; ---
;; helpers
;; ---

(define-inline (delete-successive-duplicates l #!optional (? =))
  (if (null? l)
    '()
    (let loop ((l l))
      (let ((t (cdr l)))
        (if (null? t)
          l
          (let ((h (car l)))
            (if (? h (cadr l))
              (loop t)
              (cons h (loop t)))))))))
