#!/usr/bin/csi -s

(import
  (chicken sort)
  (srfi 1)
  (srfi 69)
  (srfi 133)
  (test))

(include-relative "../src/lists.scm")
(include-relative "../src/digits.scm")
(include-relative "../src/arrays.scm")
(include-relative "../src/queues.scm")
(include-relative "../src/maths.scm")

(test-group "euler"
    (test-group "range"
      (test '(0 1 2 3 4 5 6 7 8 9 10) (range 0 10))
      (test '(10 9 8 7 6 5 4 3 2 1 0) (range 10 0))
      (test '(0 2 4 6 8 10)           (range 0 10 2)))

    (test-group "run-length"
      (test '((2 1))             (run-length '(1 1)))
      (test '((1 1) (1 2) (2 1)) (run-length '(1 2 1 1)))
      (test '((3 1) (2 2) (1 1)) (run-length '(1 1 1 2 2 1))))

    (test-group "product"
      (test '((1 2))                                                           (product '(1) '(2)))
      (test '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))           (product '(1 2 3) '(4 5 6)))
      (test '((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6)) (product '(1 2) '(3 4) '(5 6))))

    (test-group "combinations"
      (test '((1) (2) (3))                                           (combinations '(1 2 3) 1))
      (test '((1 1) (2 1) (1 2) (2 2))                               (combinations '(1 2) 2))
      (test '((1 1) (2 1) (3 1) (1 2) (2 2) (3 2) (1 3) (2 3) (3 3)) (combinations '(1 2 3) 2)))

    (test-group "powerset"
      (test '((1) (2) (3) (4))                     (powerset '(1 2 3 4) 1))
      (test '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) (powerset '(1 2 3 4) 2))
      (test '((1 2 3) (1 2 4) (1 3 4) (2 3 4))     (powerset '(1 2 3 4) 3)))

    (test-group "permutations"
      (test '((1))                                             (permutations '(1)))
      (test '((1 2) (2 1))                                     (permutations '(1 2)))
      (test '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 2 1) (3 1 2)) (permutations '(1 2 3))))

    (test-group "number->list"
      (test '(1 2 3 4)               (number->list 1234))
      (test '(1 0 0 1 1 0 1 0 0 1 0) (number->list 1234 2)))

    (test-group "list->number"
      (test 1234 (list->number '(1 2 3 4)))
      (test 1234 (list->number '(1 0 0 1 1 0 1 0 0 1 0) 2)))

    (test-group "digitsum"
      (test 10 (digitsum 1234))
      (test 5  (digitsum 1234 2)))

    (test-group "palindrome"
      (test #f (palindrome? 1234))
      (test #t (palindrome? 1221))
      (test #f (palindrome? 14 2))
      (test #t (palindrome? 15 2)))

    (test-group "array"
      (let* ((lst '((1 2 3) (4 5 6))) (array (list->array lst)))
        (test #t                                     (array?           array))
        (test lst                                    (array->list      array))
        (test array                                  (array-copy       array))
        (test '(2 3)                                 (array-dimensions array))
        (test '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2)) (array-indexes    array))

        (array-set! array '(0 0) "hello")
        (array-set! array '(0 1) "world")

        (test "hello" (array-ref array '(0 0)))
        (test "world" (array-ref array '(0 1)))

        (test #t (array-exists? array '(0 0)))
        (test #f (array-exists? array '(5 5)))))

    (test-group "priority-queue"
      (define (list->priority-queue comp? lst)
        (let loop ((lst lst) (acc priority-queue-empty))
          (if (null? lst)
            acc
            (loop (cdr lst)
              (priority-queue-insert comp? (car lst) acc)))))

      (define (priority-queue->list comp? queue)
        (let loop ((queue queue) (acc '()))
          (if (priority-queue-empty? queue)
            (reverse acc)
            (loop (priority-queue-rest comp? queue)
              (cons (priority-queue-first queue) acc)))))

      (let ((lst '(14 15 12 9 17 5 8 4 7 14 13 12 11 15 17 5 3 16 12 19)))
        (test (sort lst <) (priority-queue->list < (list->priority-queue < lst)))))

    (test-group "factorial"
      (test '(1 1 2 6 24 120 720 5040 40320 362880) (map factorial (range 0 9))))

    (test-group "primes"
      (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) (primes 50)))

    (test-group "expt-mod"
      (test 4   (expt-mod 2 50 13))
      (test 445 (expt-mod 4 13 497)))

    (test-group "discrete-log"
      (test 1665442  (discrete-log 7 11239946 20201227))
      (test 18474687 (discrete-log 7 10464955 20201227)))

    (test-group "prime?"
      (test (primes 50) (filter prime? (range 1 50))))

    (test-group "factorize"
      (test '(997123)                (sort (factorize 997123) <))
      (test '(2 2 2 2 3 3 5 7 11 13) (sort (factorize 720720) <))
      (test '(71 839 1471 6857)      (sort (factorize 600851475143) <)))

    (test-group "divisors"
      (test '(1 2 3 4 6 12)                  (sort (divisors 12) <))
      (test '(1 2 3 4 5 6 10 12 15 20 30 60) (sort (divisors 60) <))
      (test '(1 2 617 1234)                  (sort (divisors 1234) <))))

(test-exit)
