(module euler
  (;; digits
   digitsum
   list->number
   number->list
   palindrome?

   ;; list
   delete-at
   insert-at
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
   list->array
   array->list
   array-copy
   array-ref
   array-set!
   array-exists?

   ;; maths
   factorial
   fibonacci
   expt-mod
   primes
   discrete-log
   prime?
   factorize
   divisors

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
   priority-queue-drop)

  (import
    (scheme)
    (chicken base)
    (chicken fixnum)
    (chicken sort)

    (only (srfi 1)
      every
      fold
      unzip2)

    (only (srfi 69)
      hash-table-exists?
      hash-table-ref
      hash-table-set!
      make-hash-table)

    (only (srfi 133)
      vector-map))

  (include "src/digits.scm")
  (include "src/list.scm")
  (include "src/array.scm")
  (include "src/maths.scm")
  (include "src/queue.scm"))
