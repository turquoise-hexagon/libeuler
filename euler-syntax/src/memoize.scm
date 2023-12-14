(define (memoize id)
  (##sys#check-closure id 'memoize)
  (let ((cache (make-hash-table)))
    (lambda pat
      (if (hash-table-exists? cache pat)
        (hash-table-ref cache pat)
        (let ((acc (apply id pat)))
          (hash-table-set! cache pat acc)
          acc)))))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (id . pat) expr expr* ...)
     (define id (memoize (lambda pat expr expr* ...))))))
