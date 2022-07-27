(define _primes
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
    101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191
    193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283
    293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401
    409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509
    521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631
    641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751
    757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877
    881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997))

(define (factorial n)
  (if (= n 0)
    1
    (let loop ((i 1) (acc n))
      (if (> acc i)
        (* (loop (+ i i) acc)
           (loop (+ i i) (- acc i)))
        acc))))

(define (primes n)
  (if (< n 2) '()
    (let* ((lim (quotient (- n 1) 2)) (sieve (make-vector lim #t)))
      (let loop ((i 0) (t 3) (l '(2)))
        (cond
          ((< n (* t t))
           (do ((i i (+ i 1))
                (t t (+ t 2))
                (l l (if (vector-ref sieve i)
                       (cons t l)
                       l)))
             ((= i lim) (reverse l))))
          ((vector-ref sieve i)
           (do ((x (+ (* 2 i i) (* 6 i) 3) (+ x t)))
             ((<= lim x) (loop (+ i 1) (+ t 2) (cons t l)))
             (vector-set! sieve x #f)))
          (else
           (loop (+ i 1) (+ t 2) l)))))))

(define (expt-mod base expo mod)
  (let loop ((base base) (expo expo) (acc 1))
    (if (= expo 0)
      acc
      (loop (modulo (* base base) mod) (quotient expo 2)
        (if (odd? expo)
          (modulo (* base acc) mod)
          acc)))))

(define (discrete-log g h p)
  (let ((n (inexact->exact (ceiling (sqrt (- p 1))))))
    (let ((mem (make-hash-table)))
      (for-each
        (lambda (i)
          (hash-table-set! mem (expt-mod g i p) i))
        (range 0 (- n 1)))
      (let ((c (expt-mod g (* n (- p 2)) p)))
        (call/cc
          (lambda (_)
            (for-each
              (lambda (i)
                (let ((x (modulo (* h (expt-mod c i p)) p)))
                  (when (hash-table-exists? mem x)
                    (let ((a (+ (* i n) (hash-table-ref mem x))))
                      (when (> a 0) (_ a))))))
              (range 0 (- n 1)))
            (_ -1)))))))

(define (_prime? n)
  (call/cc
    (lambda (_)
      (for-each
        (lambda (i)
          (cond
            ((> (* i i) n)
             (_ #t))
            ((= (modulo n i) 0)
             (_ #f))))
        _primes)
      (_ #t))))

(define (_spsp? n a)
  (do ((d (- n 1) (/ d 2))
       (s 0 (+ s 1)))
      ((odd? d)
       (let ((t (expt-mod a d n)))
         (if (or (= t 1)
                 (= t (- n 1)))
           #t
           (do ((s s (- s 1))
                (t t (expt-mod t 2 n)))
             ((or (= s 0)
                  (= t (- n 1)))
              (> s 0))))))))

(define (prime? n)
  (cond
    ((< n 2)
     #f)
    ((< n 1000000)
     (_prime? n))
    (else
      (every
        (lambda (a)
          (_spsp? n a))
        '(2 325 9375 28178 450775 9780504 1795265022)))))

(define (_rho-factor n c)
  (let ((f (lambda (x) (modulo (+ (* x x) c) n))))
    (let loop ((t 2) (h 2) (d 1))
      (cond
        ((= d 1)
         (let* ((t (f t))
                (h (f h))
                (h (f h))
                (d (gcd (- t h) n)))
           (loop t h d)))
        ((= d n) (_rho-factor n (+ c 1)))
        ((prime? d) d)
        (else    (_rho-factor d (+ c 1)))))))

(define (factorize n)
  (let loop ((n n) (acc '()))
    (cond
      ((< n 2)
       acc)
      ((even? n)
       (loop (/ n 2) (cons 2 acc)))
      (else
        (let loop ((n n) (acc acc))
          (if (prime? n)
            (cons n acc)
            (let ((f (_rho-factor n 1)))
              (loop (/ n f) (cons f acc)))))))))

(define (divisors n)
  (let-values (((occurences factors) (unzip2 (run-length (factorize n)))))
    (map
      (lambda (multis)
        (apply * (map expt factors multis)))
      (apply product (map (lambda (_) (range 0 _)) occurences)))))
