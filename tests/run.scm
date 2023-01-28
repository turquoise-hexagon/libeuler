(import
  (chicken sort)
  (euler)
  (test)
  (srfi 1))

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

    (test-group "range"
      (test '(0 1 2 3 4 5 6 7 8 9 10) (range 0 10))
      (test '(10 9 8 7 6 5 4 3 2 1 0) (range 10 0))
      (test '(0 2 4 6 8 10)           (range 0 10 2)))

    (test-group "run-length"
      (test '((2 1))             (run-length '(1 1)))
      (test '((1 1) (1 2) (2 1)) (run-length '(1 2 1 1)))
      (test '((3 1) (2 2) (1 1)) (run-length '(1 1 1 2 2 1))))

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
      (test '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 2 1) (3 1 2)) (permutations '(1 2 3)))))

 (test-group "array"
   (let ((test-lst '((1 2 3) (4 5 6))))
     (test-group "array?/list->array"
       (test #t (array? (list->array test-lst))))

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

     (test-group "fibonacci"
       (test '(0 1 1 2 3 5 8 13 21 34) (map fibonacci test-lst))))

   (test-group "expt-mod"
     (test 4   (expt-mod 2 50 13))
     (test 445 (expt-mod 4 13 497)))

   (test-group "primes"
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) (primes 50)))

   (test-group "discrete-log"
     (test 1665442  (discrete-log 7 11239946 20201227))
     (test 18474687 (discrete-log 7 10464955 20201227)))

   (test-group "prime?"
     (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97) (filter prime? (iota 100)))
     (test '(1000003 1000033 1000037 1000039 1000081 1000099) (filter prime? (iota 100 #e1e6))))

   (test-group "factorize"
     (test '(6857 1471 839 71) (factorize 600851475143)))

   (test-group "divisors"
     (test '(1 2 617 1234) (divisors 1234))))

 (test-group "queue"
    (test-group "priority-queue?/priority-queue"
      (test #t (priority-queue? (priority-queue <))))

    (let* ((test-lst '(3 6 1 0 2 7 9 4 5 8)) (test-lst-sorted (sort test-lst <)) (test-queue (list->priority-queue test-lst <)))
      (test-group "list->priority-queue/priority-queue->list"
        (test #t (priority-queue? test-queue))
        (test test-lst-sorted (priority-queue->list test-queue)))

      (let ((square (lambda (n) (* n))))
        (test-group "priority-queue-map->list"
          (test (map square test-lst-sorted) (priority-queue-map->list test-queue square))))

      (test-group "priority-queue-fold"
        (test (reverse test-lst-sorted) (priority-queue-fold test-queue xcons '())))

      (test-group "priority-queue-filter->list"
        (test (filter odd? test-lst-sorted) (priority-queue-filter->list test-queue odd?)))

      (test-group "priority-queue-take"
        (test (take test-lst-sorted 5) (priority-queue->list (priority-queue-take test-queue 5))))

      (test-group "priority-queue-drop"
        (test (drop test-lst-sorted 5) (priority-queue->list (priority-queue-drop test-queue 5)))))))

(test-exit)
