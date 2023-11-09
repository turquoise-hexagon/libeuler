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

(define-inline (fxclosed? a b c)
  (and (fx< a b)
       (fx< b c)))

(define-inline (well-formed-list? l)
  (let loop ((i l))
    (if (list? i)
      (if (pair? i)
        (if (every list? i)
          (let ((l (length (car i))))
            (if (every
                  (lambda (_)
                    (fx= (length _) l))
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

(define-inline (make-bitset s v)
  (let* ((s (fx+ (fx/ s 8) 1)) (c (make-blob s)))
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

(define-inline (map/vector f v)
  (let* ((l (##sys#size v)) (t (make-vector l)))
    (let loop ((i 0))
      (if (fx= i l)
        t
        (begin
          (##sys#setslot t i (f (##sys#slot v i)))
          (loop (fx+ i 1)))))))

(define-inline (fxnth-root n k)
  (let loop ((h 1))
    (if (fx< (_fxexpt h k) n)
      (loop (fx* h 2))
      (let loop ((l (fx/ h 2)) (h h))
        (if (fx= (fx- h l) 1)
          (if (fx= (_fxexpt h k) n) h l)
          (let* ((m (fx/ (fx+ l h) 2)) (_ (_fxexpt m k)))
            (cond
              ((fx< _ n) (loop m h))
              ((fx< n _) (loop l m))
              (else m))))))))
