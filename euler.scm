(module euler
  (;; lists
   range
   run-length
   product
   power
   powerset
   combinations
   permutations
   ;; digits
   number->list
   list->number
   digitsum
   palindrome?
   ;; arrays
   array?
   array-dimensions
   array-indexes
   list->array
   array->list
   array-copy
   array-ref
   array-set!
   array-exists?
   ;; queues
   priority-queue-empty
   priority-queue-empty?
   priority-queue-insert
   priority-queue-first
   priority-queue-rest
   priority-queue->list
   list->priority-queue
   ;; maths
   factorial
   primes
   expt-mod
   discrete-log
   prime?
   factorize
   divisors)

  (import
    (scheme)
    (chicken base)
    (srfi 1)
    (srfi 69)
    (srfi 133))

  (include "src/lists.scm")
  (include "src/digits.scm")
  (include "src/arrays.scm")
  (include "src/queues.scm")
  (include "src/maths.scm"))
