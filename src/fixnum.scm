;; ---
;; functions
;; ---

(define-inline (_fxsqrt n)
  (if (fx= n 0)
    0
    (let loop ((i n))
      (let ((_ (fx/ (fx+ i (fx/ n i)) 2)))
        (if (fx< _ i)
          (loop _)
          i)))))

(define-inline (_fxexpt b e)
  (if (fx= e 0)
    1
    (let loop ((a b) (b e) (c 1))
      (let ((c (if (fxodd? b)
                 (fx* c a)
                 c))
            (b (fx/ b 2)))
        (if (fx= b 0)
          c
          (loop (fx* a a) b c))))))

(define-inline (_fxabs n)
  (if (fx< n 0)
    (fxneg n)
    n))

;; ---
;; wrappers
;; ---

(define (fxsqrt n)
  (##sys#check-fixnum n 'fxsqrt)
  (when (fx< n 0)
    (##sys#error-bad-exact-uinteger n 'fxsqrt))
  (_fxsqrt n))

(define (fxexpt b e)
  (##sys#check-fixnum b 'fxexpt)
  (##sys#check-fixnum e 'fxexpt)
  (when (fx< e 0)
    (##sys#error-bad-exact-uinteger e 'fxexpt))
  (_fxexpt b e))

(define (fxabs n)
  (##sys#check-fixnum n 'fxabs)
  (_fxabs n))
