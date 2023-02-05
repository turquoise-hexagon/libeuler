;; ---
;; constants
;; ---

(define _stored-primes
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
  (if (= n 0)
    1
    (let loop ((i 1) (acc n))
      (if (> acc i)
        (* (loop (+ i i) acc)
           (loop (+ i i) (- acc i)))
        acc))))

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
    ((= n 0) 0)
    ((< n 3) 1)
    (else
      (if (even? n)
        (helper 0  1)
        (helper 1 -1)))))

(define-inline (_modular-inverse a b)
  (let* ((b (if (< b 0) (- b) b))
         (a (if (< a 0) (- b (modulo (- a) b)) a)))
    (let loop ((t 0) (nt 1) (r b) (nr (modulo a b)))
      (if (= nr 0)
        (cond
          ((> r 1) -1)
          ((< t 0) (+ t b))
          (else t))
        (let ((q (quotient r nr)))
          (loop
            nt (- t (* q nt))
            nr (- r (* q nr))))))))

(define-inline (_modular-expt b e m)
  (let loop ((b b) (e e) (acc 1))
    (if (= e 0)
      acc
      (loop (modulo (* b b) m) (quotient e 2)
        (if (odd? e)
          (modulo (* b acc) m)
          acc)))))

(define-inline (_primes n)
  (if (fx< n 2) '()
    (let* ((lim (fx/ (fx- n 1) 2)) (sieve (make-vector lim #t)))
      (let loop ((i 0) (t 3) (l '(2)))
        (cond
          ((fx< n (fx* t t))
           (do ((i i (fx+ i 1))
                (t t (fx+ t 2))
                (l l (if (vector-ref sieve i)
                       (cons t l)
                       l)))
             ((fx= i lim) (reverse l))))
          ((vector-ref sieve i)
           (do ((x (fx+ 3 (fx+ (fx* 2 (fx* i i)) (fx* 6 i))) (fx+ x t)))
             ((fx<= lim x) (loop (fx+ i 1) (fx+ t 2) (cons t l)))
             (vector-set! sieve x #f)))
          (else
           (loop (fx+ i 1) (fx+ t 2) l)))))))

(define-inline (_discrete-log g h p)
  (let* ((n (inexact->exact (ceiling (sqrt (- p 1))))) (c (modular-expt g (* n (- p 2)) p)))
    (let ((mem (make-hash-table)))
      (let loop ((i 0))
        (unless (= i n)
          (hash-table-set! mem (modular-expt g i p) i)
          (loop (+ i 1))))
      (let loop ((i 0))
        (if (= i n)
          -1
          (let ((x (modulo (* h (modular-expt c i p)) p)))
            (if (hash-table-exists? mem x)
              (let ((a (+ (* i n) (hash-table-ref mem x))))
                (if (> a 0)
                  a
                  (loop (+ i 1))))
              (loop (+ i 1)))))))))

(define-inline (_trial-division-prime? n)
  (let loop ((l _stored-primes))
    (if (null? l)
      #t
      (let ((_ (car l)))
        (if (= n _)
          #t
          (if (= (modulo n _) 0)
            #f
            (loop (cdr l))))))))

(define-inline (_witness? n a)
  (do ((d (- n 1) (/ d 2))
       (s 0 (+ s 1)))
      ((odd? d)
       (let ((t (modular-expt a d n)))
         (if (or (= t 1)
                 (= t (- n 1)))
           #t
           (do ((s s (- s 1))
                (t t (modular-expt t 2 n)))
             ((or (= s 0)
                  (= t (- n 1)))
              (> s 0))))))))

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

(define-inline (_factorize n)
  (let loop ((n n) (acc '()))
    (if (< n 2)
      acc
      (if (even? n)
        (loop (/ n 2) (cons 2 acc))
        (let loop ((n n) (acc acc))
          (if (prime? n)
            (cons n acc)
            (let ((_ (_factor n)))
              (loop (/ n _) (cons _ acc)))))))))

(define-inline (_divisors n)
  (let-values (((occurences factors) (unzip2 (run-length (factorize n)))))
    (map
      (lambda (multis)
        (apply * (map expt factors multis)))
      (apply product (map (lambda (_) (range 0 _)) occurences)))))

;; ---
;; wrappers
;; ---

(define (factorial n)
  (##sys#check-integer n 'factorial)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'factorial))
  (_factorial n))

(define (fibonacci n)
  (##sys#check-integer n 'fibonacci)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'fibonacci))
  (_fibonacci n))

(define (modular-inverse a b)
  (##sys#check-integer a 'modular-inverse)
  (##sys#check-integer b 'modular-inverse)
  (_modular-inverse a b))

(define (modular-expt b e m)
  (##sys#check-integer b 'modular-expt)
  (##sys#check-integer e 'modular-expt)
  (##sys#check-integer m 'modular-expt)
  (when (< b 0) (##sys#error-bad-exact-uinteger b 'modular-expt))
  (when (< e 0) (##sys#error-bad-exact-uinteger e 'modular-expt))
  (when (< m 0) (##sys#error-bad-exact-uinteger m 'modular-expt))
  (_modular-expt b e m))

(define (primes n)
  (##sys#check-fixnum n 'primes)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'primes))
  (_primes n))

(define (discrete-log g h p)
  (##sys#check-integer g 'discrete-log)
  (##sys#check-integer h 'discrete-log)
  (##sys#check-integer p 'discrete-log)
  (when (< g 0) (##sys#error-bad-exact-uinteger g 'discrete-log))
  (when (< h 0) (##sys#error-bad-exact-uinteger h 'discrete-log))
  (when (< p 0) (##sys#error-bad-exact-uinteger p 'discrete-log))
  (_discrete-log g h p))

(define (prime? n)
  (##sys#check-integer n 'prime?)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'prime?))
  (_prime? n))

(define (factorize n)
  (##sys#check-integer n 'factorize)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'factorize))
  (_factorize n))

(define (divisors n)
  (##sys#check-integer n 'divisors)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'divisors))
  (_divisors n))
