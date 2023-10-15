;; ---
;; constants
;; ---

(define-constant _stored-primes
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
    101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191
    193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283
    293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401
    409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509
    521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631
    641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751
    757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877
    881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997))

;; ---
;; functions
;; ---

(define-inline (_factorial n)
  (if (zero? n)
    1
    (let loop ((i 1) (acc n))
      (if (> acc i)
        (* (loop (+ i i) acc)
           (loop (+ i i) (- acc i)))
        acc))))

(define-inline (_binomial n k)
  (let main ((n n) (k k))
    (cond
      ((< k 0) 0)
      ((> k n) 0)
      ((= k 0) 1)
      ((= k n) 1)
      ((> (+ k k) n)
       (main n (- n k)))
      (else
       (let loop ((i 2) (acc (+ (- n k) 1)))
         (if (> i k)
           acc
           (loop (+ i 1) (quotient (* acc (+ (- n k) i)) i))))))))

(define-inline (_fibonacci n)
  (define (helper a b)
    (let loop ((a a) (b b) (c 3) (n (quotient n 2)))
      (if (> n 1)
        (let ((tc (- (* c c) 2)) (tn (quotient n 2)))
          (if (even? n)
            (loop a (+ a (* b c)) tc tn)
            (loop (+ b (* a c)) b tc tn)))
        (+ b (* a c)))))
  (cond
    ((zero? n) 0)
    ((< n 3) 1)
    (else
      (if (even? n)
        (helper 0  1)
        (helper 1 -1)))))

(define-inline (_modular-inverse a b)
  (let* ((b (if (negative? b) (- b) b))
         (a (if (negative? a) (- b (modulo (- a) b)) a)))
    (let loop ((t 0) (nt 1) (r b) (nr (modulo a b)))
      (if (zero? nr)
        (cond
          ((> r 1) -1)
          ((negative? t) (+ t b))
          (else t))
        (let ((q (quotient r nr)))
          (loop
            nt (- t (* q nt))
            nr (- r (* q nr))))))))

(define-inline (_solve-chinese a n)
  (let ((p (apply * n)))
    (let loop ((la a) (ln n) (acc 0))
      (if (or (null? la)
              (null? ln))
        (modulo acc p)
        (let* ((a (car la)) (n (car ln)) (i (quotient p n)) (m (modular-inverse i n)))
          (if (= m -1)
            -1
            (loop (cdr la) (cdr ln) (+ acc (* a m i)))))))))

(define-inline (_modular-expt b e m)
  (let loop ((b b) (e e) (acc 1))
    (if (zero? e)
      acc
      (loop (modulo (* b b) m) (quotient e 2)
        (if (odd? e)
          (modulo (* b acc) m)
          acc)))))

(define-inline (_primes n)
  (if (fx< n 2) '()
    (let* ((lim (fx/ (fx- n 1) 2)) (sieve (make-bitset lim #t)))
      (let loop ((i 0) (t 3) (l '(2)))
        (cond
          ((fx< n (fx* t t))
           (do ((i i (fx+ i 1))
                (t t (fx+ t 2))
                (l l (if (bitset-ref sieve i)
                       (cons t l)
                       l)))
             ((fx= i lim) (reverse l))))
          ((bitset-ref sieve i)
           (do ((x (fx+ 3 (fx+ (fx* 2 (fx* i i)) (fx* 6 i))) (fx+ x t)))
             ((fx<= lim x) (loop (fx+ i 1) (fx+ t 2) (cons t l)))
             (bitset-set! sieve x #f)))
          (else
           (loop (fx+ i 1) (fx+ t 2) l)))))))

(define-inline (_discrete-log b n m)
  (let ((l (inexact->exact (ceiling (sqrt m)))) (h (make-hash-table)))
    (let loop ((i 0) (t 1))
      (unless (= i l)
        (hash-table-set! h t i)
        (loop (+ i 1) (modulo (* t b) m))))
    (let ((c (modular-expt (modular-expt b (- m 2) m) l m)))
      (let loop ((i 0) (t n))
        (if (= i l)
          -1
          (if (hash-table-exists? h t)
            (let ((_ (+ (hash-table-ref h t) (* i l))))
              (if (positive? _)
                (if (= (modular-expt b _ m) n)
                  _
                  -1)
                (loop (+ i 1) (modulo (* t c) m))))
            (loop (+ i 1) (modulo (* t c) m))))))))

(define-inline (_trial-division-prime? n)
  (let loop ((l _stored-primes))
    (if (null? l)
      #t
      (let ((_ (car l)))
        (cond
          ((> (* _ _) n) #t)
          ((zero? (modulo n _)) #f)
          (else (loop (cdr l))))))))

(define-inline (_witness? n a)
  (do ((d (- n 1) (quotient d 2))
       (s 0 (+ s 1)))
      ((odd? d)
       (let ((t (modular-expt a d n)))
         (if (or (= t 1)
                 (= t (- n 1)))
           #t
           (do ((s s (- s 1))
                (t t (modular-expt t 2 n)))
             ((or (zero? s)
                  (= t (- n 1)))
              (positive? s))))))))

(define-inline (_prime? n)
  (if (< n 2)
    #f
    (if (< n #e1e6)
      (_trial-division-prime? n)
      (every
        (lambda (a)
          (_witness? n a))
        '(2 325 9375 28178 450775 9780504 1795265022)))))

(define-inline (_factor n)
  (let main ((n n) (c 1))
    (let ((f (lambda (x) (modulo (+ (* x x) c) n))))
      (let loop ((t 2) (h 2) (d 1))
        (cond
          ((= d 1)
           (let* ((t (f t))
                  (h (f h))
                  (h (f h))
                  (d (gcd (- t h) n)))
             (loop t h d)))
          ((= d n) (main n (+ c 1)))
          ((prime? d) d)
          (else    (main d (+ c 1))))))))

(define-inline (_factors n)
  (let loop ((n n) (acc '()))
    (if (< n 2)
      acc
      (if (even? n)
        (loop (quotient n 2) (cons 2 acc))
        (let loop ((n n) (acc acc))
          (if (prime? n)
            (cons n acc)
            (let ((_ (_factor n)))
              (loop (quotient n _) (cons _ acc)))))))))

(define-inline (_divisors n)
  (let loop ((l (run-length (factors n))))
    (if (null? l)
      '(1)
      (apply
        (lambda (a b)
          (foldl
            (lambda (acc t)
              (do ((i 0 (+ i 1))
                   (t t (* t b))
                   (acc acc (cons t acc)))
                ((> i a) acc)))
            '() (loop (cdr l))))
        (car l)))))

(define-inline (_totient n)
  (foldl
    (lambda (acc i)
      (- acc (quotient acc i)))
    n (_delete-successive-duplicates (_factors n))))

;; ---
;; wrappers
;; ---

(define (factorial n)
  (##sys#check-integer n 'factorial)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'factorial))
  (_factorial n))

(define (binomial n k)
  (##sys#check-integer n 'binomial)
  (##sys#check-integer n 'binomial)
  (_binomial n k))

(define (fibonacci n)
  (##sys#check-integer n 'fibonacci)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'fibonacci))
  (_fibonacci n))

(define (modular-inverse a b)
  (##sys#check-integer a 'modular-inverse)
  (##sys#check-integer b 'modular-inverse)
  (_modular-inverse a b))

(define (solve-chinese a n)
  (##sys#check-list a 'solve-chinese)
  (##sys#check-list n 'solve-chinese)
  (_solve-chinese a n))

(define (modular-expt b e m)
  (##sys#check-integer b 'modular-expt)
  (##sys#check-integer e 'modular-expt)
  (##sys#check-integer m 'modular-expt)
  (when (negative? b) (##sys#error-bad-exact-uinteger b 'modular-expt))
  (when (negative? e) (##sys#error-bad-exact-uinteger e 'modular-expt))
  (when (negative? m) (##sys#error-bad-exact-uinteger m 'modular-expt))
  (_modular-expt b e m))

(define (primes n)
  (check-positive-fixnum n 'primes)
  (_primes n))

(define (discrete-log b n m)
  (##sys#check-integer b 'discrete-log)
  (##sys#check-integer n 'discrete-log)
  (##sys#check-integer m 'discrete-log)
  (when (negative? b) (##sys#error-bad-exact-uinteger b 'discrete-log))
  (when (negative? n) (##sys#error-bad-exact-uinteger n 'discrete-log))
  (when (negative? m) (##sys#error-bad-exact-uinteger m 'discrete-log))
  (_discrete-log b n m))

(define (prime? n)
  (##sys#check-integer n 'prime?)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'prime?))
  (_prime? n))

(define (factors n)
  (##sys#check-integer n 'factors)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'factors))
  (_factors n))

(define (divisors n)
  (##sys#check-integer n 'divisors)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'divisors))
  (_divisors n))

(define (totient n)
  (##sys#check-integer n 'totient)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'totient))
  (_totient n))
