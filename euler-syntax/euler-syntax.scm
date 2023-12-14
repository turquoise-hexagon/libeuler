(module euler-syntax
  (memoize
   define-memoized)

  (import
    (scheme)
    (chicken base)

    (only (srfi 69)
      hash-table-exists?
      hash-table-ref
      hash-table-set!
      make-hash-table))

  (include-relative "src/memoize.scm"))
