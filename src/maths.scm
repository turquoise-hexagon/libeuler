;; ---
;; globals
;; ---

(define-constant _trial-division-prime?-limit
  #e1e6)

(define-constant _primes-pi-limit
  #e1e12)

(define _trial-division-prime?-primes
  #f)

;; ---
;; functions
;; ---

(define-inline (_factorial n)
  (if (zero? n)
    1
    (let loop ((i 1) (acc n))
      (if (> acc i)
        (let ((_ (+ i i)))
          (* (loop _ acc)
             (loop _ (- acc i))))
        acc))))

(define-inline (_binomial n k)
  (let main ((n n) (k k))
    (let ((_ (- n k)))
      (cond
        ((negative? k) 0)
        ((zero? k) 1)
        ((negative? _) 0)
        ((zero? _) 1)
        ((> (+ k k) n)
         (main n _))
        (else
         (let loop ((i 2) (acc (+ _ 1)))
           (if (> i k)
             acc
             (loop (+ i 1) (quotient (* acc (+ _ i)) i)))))))))

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

(define-inline (_chinese-remainder-theorem a n)
  (let ((p (apply * n)))
    (let loop ((la a) (ln n) (acc 0))
      (if (or (null? la)
              (null? ln))
        (modulo acc p)
        (let* ((a (car la)) (n (car ln)) (i (quotient p n)) (m (_modular-inverse i n)))
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

(define-inline (_phi x a)
  (let loop ((x x) (a a))
    (if (fx= a 0)
      x
      (if (fx= a 1)
        (fx- x (fxshr x 1))
        (let* ((a (fx- a 1)) (p (vector-ref ps a)))
          (if (fx> p x)
            1
            (fx- (loop x a) (loop (fx/ x p) a))))))))

(define-inline (_primes-pi n)
  (when (fx> n _primes-pi-limit)
    (error 'primes-pi "value exceeds maximum" n))
  (if (fx< n 2)
    0
    (let* ((ps (list->vector (_primes (_fxsqrt n)))) (a (vector-length ps)))
      (fx+ (_phi n a) (fx- a 1)))))

(define-inline (_discrete-log b n m)
  (let ((l (inexact->exact (ceiling (sqrt m)))) (h (make-hash-table)))
    (let loop ((i 0) (t 1))
      (unless (= i l)
        (hash-table-set! h t i)
        (loop (+ i 1) (modulo (* t b) m))))
    (let ((c (_modular-expt (_modular-expt b (- m 2) m) l m)))
      (let loop ((i 0) (t n))
        (if (= i l)
          -1
          (if (hash-table-exists? h t)
            (let ((_ (+ (hash-table-ref h t) (* i l))))
              (if (positive? _)
                (if (= (_modular-expt b _ m) n)
                  _
                  -1)
                (loop (+ i 1) (modulo (* t c) m))))
            (loop (+ i 1) (modulo (* t c) m))))))))

(define-inline (_trial-division-prime? n)
  (unless _trial-division-prime?-primes
    (set! _trial-division-prime?-primes (_primes (_fxsqrt _trial-division-prime?-limit))))
  (let loop ((l _trial-division-prime?-primes))
    (if (null? l)
      #t
      (let ((_ (car l)))
        (cond
          ((fx> (fx*   _ _) n) #t)
          ((fx= (fxmod n _) 0) #f)
          (else (loop (cdr l))))))))

(define-inline (_witness? n a)
  (do ((d (- n 1) (quotient d 2))
       (s 0 (+ s 1)))
      ((odd? d)
       (let ((t (_modular-expt a d n)))
         (if (or (= t 1)
                 (= t (- n 1)))
           #t
           (do ((s s (- s 1))
                (t t (_modular-expt t 2 n)))
             ((or (zero? s)
                  (= t (- n 1)))
              (positive? s))))))))

(define-inline (_prime? n)
  (if (< n 2)
    #f
    (if (< n _trial-division-prime?-limit)
      (_trial-division-prime? n)
      (every
        (lambda (i)
          (_witness? n i))
        '(2 325 9375 28178 450775 9780504 1795265022)))))

(define-inline (_factor n)
  (cond
    ((< n 2) #f)
    ((even? n) 2)
    ((prime? n) n)
    (else
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
             ((_prime? d) d)
             (else    (main d (+ c 1))))))))))

(define-inline (_factors n)
  (let loop ((n n) (acc '()))
    (let ((_ (_factor n)))
      (if _
        (loop (quotient n _) (cons _ acc))
        acc))))

(define-inline (_divisors n)
  (if (zero? n)
    '()
    (let loop ((n n))
      (if (= n 1)
        '(1)
        (let ((a (_factor n)))
          (let subloop ((n n) (b 0))
            (if (zero? (modulo n a))
              (subloop (quotient n a) (+ b 1))
              (foldl
                (lambda (acc t)
                  (do ((i 0 (+ i 1))
                       (t t (* t a))
                       (acc acc (cons t acc)))
                    ((> i b) acc)))
                '() (loop n)))))))))

(define-inline (_totient n)
  (let loop ((n n) (acc n))
    (let ((_ (_factor n)))
      (if _
        (let subloop ((n n))
          (if (zero? (modulo n _))
            (subloop (quotient n _))
            (loop n (- acc (quotient acc _)))))
        acc))))

(define-inline (_moebius n)
  (if (zero? n)
    0
    (let loop ((n n) (acc 1))
      (if (= n 1)
        acc
        (let ((_ (_factor n)))
          (if (zero? (modulo n (* _ _)))
            0
            (loop (quotient n _) (* acc -1))))))))

;; ---
;; wrappers
;; ---

(define (factorial n)
  (##sys#check-integer    n 'factorial)
  (check-positive-integer n 'factorial)
  (_factorial n))

(define (binomial n k)
  (##sys#check-integer    n 'binomial)
  (##sys#check-integer    k 'binomial)
  (check-positive-integer n 'binomial)
  (check-positive-integer k 'binomial)
  (_binomial n k))

(define (fibonacci n)
  (##sys#check-integer    n 'fibonacci)
  (check-positive-integer n 'fibonacci)
  (_fibonacci n))

(define (modular-inverse a b)
  (##sys#check-integer a 'modular-inverse)
  (##sys#check-integer b 'modular-inverse)
  (_modular-inverse a b))

(define (chinese-remainder-theorem a n)
  (##sys#check-list a 'chinese-remainder-theorem)
  (##sys#check-list n 'chinese-remainder-theorem)
  (_chinese-remainder-theorem a n))

(define (modular-expt b e m)
  (##sys#check-integer    b 'modular-expt)
  (##sys#check-integer    e 'modular-expt)
  (##sys#check-integer    m 'modular-expt)
  (check-positive-integer b 'modular-expt)
  (check-positive-integer e 'modular-expt)
  (check-positive-integer m 'modular-expt)
  (_modular-expt b e m))

(define (primes n)
  (##sys#check-fixnum    n 'primes)
  (check-positive-fixnum n 'primes)
  (_primes n))

(define (primes-pi n)
  (##sys#check-fixnum    n 'primes-pi)
  (check-positive-fixnum n 'primes-pi)
  (_primes-pi n))

(define (discrete-log b n m)
  (##sys#check-integer    b 'discrete-log)
  (##sys#check-integer    n 'discrete-log)
  (##sys#check-integer    m 'discrete-log)
  (check-positive-integer b 'discrete-log)
  (check-positive-integer n 'discrete-log)
  (check-positive-integer m 'discrete-log)
  (_discrete-log b n m))

(define (prime? n)
  (##sys#check-integer    n 'prime?)
  (check-positive-integer n 'prime?)
  (_prime? n))

(define (factors n)
  (##sys#check-integer    n 'factors)
  (check-positive-integer n 'factors)
  (_factors n))

(define (divisors n)
  (##sys#check-integer    n 'divisors)
  (check-positive-integer n 'divisors)
  (_divisors n))

(define (totient n)
  (##sys#check-integer    n 'totient)
  (check-positive-integer n 'totient)
  (_totient n))

(define (moebius n)
  (##sys#check-integer    n 'moebius)
  (check-positive-integer n 'moebius)
  (_moebius n))
