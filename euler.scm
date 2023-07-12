(module euler
  (;; digits
   digitsum
   list->number
   number->list
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
   list->array
   array->list
   array-copy
   array-ref
   array-set!
   array-exists?

   ;; maths
   factorial
   fibonacci
   modular-inverse
   solve-chinese
   modular-expt
   primes
   discrete-log
   prime?
   factors
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
    (chicken blob)
    (chicken fixnum)
    (chicken foreign)
    (chicken sort)

    (only (srfi 1)
      every
      fold)

    (only (srfi 69)
      hash-table-exists?
      hash-table-ref
      hash-table-set!
      make-hash-table))

  (include "src/digits.scm")
  (include "src/list.scm")
  (include "src/array.scm")
  (include "src/maths.scm")
  (include "src/queue.scm"))
