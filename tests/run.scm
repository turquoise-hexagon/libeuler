(import
  (chicken sort)
  (euler)
  (euler-syntax)
  (srfi 1)
  (test))

(test-group "euler"
  (test-group "digits"
    (test-group "digitsum"
      (test 10 (digitsum 1234))
      (test 5  (digitsum 1234 2)))

    (test-group "list->number"
      (test 1234 (list->number '(1 2 3 4)))
      (test 1234 (list->number '(1 0 0 1 1 0 1 0 0 1 0) 2)))

    (test-group "number->list"
      (test '(1 2 3 4)               (number->list 1234))
      (test '(1 0 0 1 1 0 1 0 0 1 0) (number->list 1234 2)))

    (test-group "integer-log"
      (test 3 (integer-log 1234))
      (test 4 (integer-log 12345))
      (test 5 (integer-log 123456)))

    (test-group "palindrome?"
      (test #f (palindrome? 1234))
      (test #t (palindrome? 1221))
      (test #f (palindrome? 14 2))
      (test #t (palindrome? 15 2))))

  (test-group "list"
    (test-group "insert-at"
      (test '(0 1 2 3) (insert-at '(1 2 3) 0 0))
      (test '(1 2 3 4) (insert-at '(1 2 3) 3 4)))

    (test-group "delete-at"
      (test '(2 3) (delete-at '(1 2 3) 0))
      (test '(1 2) (delete-at '(1 2 3) 2)))

    (test-group "delete-first"
      (test '(1 2 3 4 5) (delete-first '(1 2 2 3 4 5) 2))
      (test '(1 2 3 4 5) (delete-first '(1 2 3 4 5) 7)))

    (test-group "range"
      (test '(0 1 2 3 4 5 6 7 8 9 10) (range 10))
      (test '(10 9 8 7 6 5 4 3 2 1 0) (range 10 0))
      (test '(0 2 4 6 8 10)           (range 0 10 2)))

    (test-group "run-length"
      (test '((2 . 1))             (run-length '(1 1)))
      (test '((1 . 1) (1 . 2) (2 . 1)) (run-length '(1 2 1 1)))
      (test '((3 . 1) (2 . 2) (1 . 1)) (run-length '(1 1 1 2 2 1))))

    (test-group "extremum"
      (test 1       (extremum '(1 2 3 4 5)))
      (test "aaaaa" (extremum '("a" "aa" "aaa" "aaaa" "aaaaa") string-length >)))

    (test-group "product"
      (test '((1 2))                                                           (product '(1) '(2)))
      (test '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))           (product '(1 2 3) '(4 5 6)))
      (test '((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6)) (product '(1 2) '(3 4) '(5 6))))

    (test-group "power"
      (test '((1) (2) (3))                                           (power '(1 2 3) 1))
      (test '((1 1) (1 2) (2 1) (2 2))                               (power '(1 2) 2))
      (test '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (power '(1 2 3) 2)))

    (test-group "powerset"
      (test '((1) ())                                   (powerset '(1)))
      (test '((1 2) (1) (2) ())                         (powerset '(1 2)))
      (test '((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ()) (powerset '(1 2 3))))

    (test-group "combinations"
      (test '((1) (2) (3) (4))                     (combinations '(1 2 3 4) 1))
      (test '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) (combinations '(1 2 3 4) 2))
      (test '((1 2 3) (1 2 4) (1 3 4) (2 3 4))     (combinations '(1 2 3 4) 3)))

    (test-group "permutations"
      (test '((1))                                             (permutations '(1)))
      (test '((1 2) (2 1))                                     (permutations '(1 2)))
      (test '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (permutations '(1 2 3)))))

 (test-group "array"
   (let ((test-lst '((1 2 3) (4 5 6))))
     (test-group "array?/list->array"
       (test #t (array? (list->array test-lst))))

     (test-group "make-array"
       (test #t (array? (make-array '(2 3) 0))))

     (let ((test-array (list->array test-lst)))
       (test-group "array-indexes"
         (test '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2)) (array-indexes test-array)))

       (test-group "array-dimensions"
         (test '(2 3) (array-dimensions test-array)))

       (test-group "array->list"
         (test test-lst (array->list test-array)))

       (test-group "array-copy"
         (test test-array (array-copy test-array)))

       (test-group "array-ref"
         (test 1 (array-ref test-array '(0 0)))
         (test 4 (array-ref test-array '(1 0))))

       (test-group "array-set!"
         (array-set! test-array '(0 0) 7)
         (array-set! test-array '(1 0) 8)
         (test 7 (array-ref test-array '(0 0)))
         (test 8 (array-ref test-array '(1 0))))

       (test-group "array-exists?"
         (test #t (array-exists? test-array '(0 0)))
         (test #f (array-exists? test-array '(0 3)))))))

 (test-group "maths"
   (let ((test-lst '(0 1 2 3 4 5 6 7 8 9)))
     (test-group "factorial"
       (test '(1 1 2 6 24 120 720 5040 40320 362880) (map factorial test-lst)))

     (test-group "binomial"
       (test 11628 (binomial 19  5))
       (test 54264 (binomial 21 15)))

     (test-group "fibonacci"
       (test '(0 1 1 2 3 5 8 13 21 34) (map fibonacci test-lst))))

   (test-group "modular-inverse"
     (test 1969 (modular-inverse 42 2017))
     (test 0    (modular-inverse 40 1))
     (test 96   (modular-inverse 52 -217))
     (test 121  (modular-inverse -486 217))
     (test -1   (modular-inverse 40 2018)))

   (test-group "chinese-remainder-theorem"
     (test 23   (chinese-remainder-theorem '(2 3 2) '(3 5 7)))
     (test 1731 (chinese-remainder-theorem '(1 2 3 4) '(5 7 9 11))))

   (test-group "modular-expt"
     (test 4   (modular-expt 2 50 13))
     (test 445 (modular-expt 4 13 497)))

   (test-group "primes"
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) (primes 50)))

   (test-group "prime-pi"
     (test 9592 (prime-pi #e1e5)))

   (test-group "discrete-log"
     (test 1665442  (discrete-log 7 11239946 20201227))
     (test 18474687 (discrete-log 7 10464955 20201227)))

   (test-group "prime?"
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97) (filter prime? (iota 100)))
     (test '(1000003 1000033 1000037 1000039 1000081 1000099) (filter prime? (iota 100 #e1e6))))

   (test-group "factors"
     (test '(6857 1471 839 71) (factors 600851475143)))

   (test-group "divisors"
     (test '(2 1 1234 617) (divisors 1234))
     (test '(20 10 5 60 30 15 4 2 1 12 6 3) (divisors 60)))

   (test-group "totient"
     (test 60 (totient 99))
     (test 44 (totient 69)))

   (test-group "moebius"
     (test '(1 -1 -1 0 -1 1 -1 0 0 1) (map moebius (iota 10 1)))))

 (test-group "fixnum"
   (test-group "fxsqrt"
     (test 16 (fxsqrt 265))
     (test 24 (fxsqrt 617))
     (test 29 (fxsqrt 845)))

   (test-group "fxexpt"
     (test 10 (fxexpt 10 1))
     (test 4096 (fxexpt 8 4))
     (test 16777216 (fxexpt 8 8)))

   (test-group "fxabs"
     (test 441 (fxabs -441))
     (test 390 (fxabs -390))
     (test 869 (fxabs 869)))

   (test-group "fxlcm"
     (test 14874 (fxlcm 111 402))
     (test 38850 (fxlcm 111 -350))
     (test 78729 (fxlcm -483 -489))))

 (test-group "queue"
    (test-group "priority-queue?/priority-queue"
      (test #t (priority-queue? (priority-queue <))))

    (let* ((test-lst '(3 6 1 0 2 7 9 4 5 8)) (test-lst-sorted (sort test-lst <)) (test-queue (list->priority-queue test-lst <)))
      (test-group "list->priority-queue/priority-queue->list"
        (test #t (priority-queue? test-queue))
        (test test-lst-sorted (priority-queue->list test-queue)))

      (let ((square (lambda (n) (* n n))))
        (test-group "priority-queue-map->list"
          (test (map square test-lst-sorted) (priority-queue-map->list test-queue square))))

      (test-group "priority-queue-fold"
        (test (reverse test-lst-sorted) (priority-queue-fold test-queue xcons '())))

      (test-group "priority-queue-filter->list"
        (test (filter odd? test-lst-sorted) (priority-queue-filter->list test-queue odd?)))

      (test-group "priority-queue-take"
        (test (take test-lst-sorted 5) (priority-queue->list (priority-queue-take test-queue 5))))

      (test-group "priority-queue-drop"
        (test (drop test-lst-sorted 5) (priority-queue->list (priority-queue-drop test-queue 5))))

      (test-group "priority-queue-length"
        (test 10 (priority-queue-length test-queue)))))

 (test-group "memoize"
   (define-memoized (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

   (test 832040 (fib 30))))

(test-exit)
