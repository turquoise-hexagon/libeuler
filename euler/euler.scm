(module euler
  (;; digits
   digitsum
   list->number
   number->list
   integer-log
   palindrome?

   ;; list
   delete-at
   insert-at
   delete-first
   range
   run-length
   extremum
   product
   power
   powerset
   combinations
   permutations

   ;; array
   array?
   array-indexes
   array-dimensions
   make-array
   list->array
   array->list
   array-copy
   array-ref
   array-set!
   array-exists?

   ;; maths
   factorial
   binomial
   fibonacci
   modular-inverse
   chinese-remainder-theorem
   modular-expt
   primes
   prime-pi
   discrete-log
   prime?
   factors
   divisors
   totient
   moebius

   ;; fixnum
   fxsqrt
   fxexpt
   fxabs
   fxlcm

   ;; queue
   priority-queue?
   priority-queue
   priority-queue-empty?
   priority-queue-insert
   priority-queue-first
   priority-queue-rest
   list->priority-queue
   priority-queue->list
   priority-queue-map->list
   priority-queue-for-each
   priority-queue-fold
   priority-queue-filter->list
   priority-queue-take
   priority-queue-drop
   priority-queue-length)

  (import
    (scheme)
    (chicken base)
    (chicken blob)
    (chicken fixnum)
    (chicken foreign)

    (only (srfi 1)
      xcons)

    (only (srfi 69)
      hash-table-exists?
      hash-table-ref
      hash-table-set!
      make-hash-table))

  (include-relative "src/utils/utils.scm")

  (include-relative "src/digits.scm")
  (include-relative "src/fixnum.scm")
  (include-relative "src/list.scm")
  (include-relative "src/array.scm") ;; depends on `range`
  (include-relative "src/maths.scm")
  (include-relative "src/queue.scm"))
