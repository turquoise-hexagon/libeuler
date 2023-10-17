;; ---
;; structures
;; ---

(define-record array
  content
  indexes
  dimensions)

;; ---
;; functions
;; ---

(define-inline (_array-content l)
  (let loop ((i l))
    (if (list? i)
      (list->vector (map loop i))
      i)))

(define-inline (_array-indexes l)
  (apply product
    (let loop ((i l))
      (if (list? i)
        (cons
          (let ((_ (length i)))
            (if (zero? _)
              (list 0)
              (range 0 (- _ 1))))
          (loop (car i)))
        '()))))

(define-inline (_array-dimensions l)
  (let loop ((i l))
    (if (list? i)
      (cons (length i) (loop (car i)))
      '())))

(define-inline (_list->array l)
  (make-array
    (_array-content l)
    (_array-indexes l)
    (_array-dimensions l)))

(define-inline (_array->list a)
  (let loop ((i (array-content a)))
    (if (vector? i)
      (map loop (vector->list i))
      i)))

(define-inline (_array-content-copy a)
  (let loop ((i (array-content a)))
    (if (vector? i)
      (list->vector (map loop (vector->list i)))
      i)))

(define-inline (_array-copy a)
  (make-array
    (_array-content-copy a)
    (array-indexes a)
    (array-dimensions a)))

(define-inline (_array-ref a c)
  (let loop ((acc (array-content a)) (c c) (d (array-dimensions a)))
    (if (null? c)
      acc
      (if (null? d)
        (error 'array-ref "error - out of range" acc c)
        (if (< -1 (car c) (car d))
          (loop (vector-ref acc (car c)) (cdr c) (cdr d))
          (error 'array-ref "error - out of range" acc c))))))

(define-inline (_array-set! a c i)
  (let loop ((acc (array-content a)) (c c) (d (array-dimensions a)))
    (let ((c (car c)) (d (car d)) (tc (cdr c)) (td (cdr d)))
      (if (null? tc)
        (if (vector? acc)
          (if (< -1 c d)
            (vector-set! acc c i)
            (error 'array-set! "error - out of range" acc c))
          (error 'array-set! "error - out of range" acc c))
        (if (< -1 c d)
          (loop (vector-ref acc c) tc td)
          (error 'array-set! "error - out of range" acc c))))))

(define-inline (_array-exists? a c)
  (let loop ((c c) (d (array-dimensions a)))
    (if (null? c)
      #t
      (if (null? d)
        #f
        (if (< -1 (car c) (car d))
          (loop (cdr c) (cdr d))
          #f)))))

;; ---
;; others
;; ---

(define-record-printer (array a p)
  (format p "@~s" (_array->list a)))

;; ---
;; wrappers
;; ---

(define (list->array l)
  (unless (well-formed-list? l)
    (error 'list->array "error - malformed array list" l))
  (_list->array l))

(define (array->list a)
  (##sys#check-structure a 'euler#array 'array->list)
  (_array->list a))

(define (array-copy a)
  (##sys#check-structure a 'euler#array 'array-copy)
  (_array-copy a))

(define (array-ref a c)
  (##sys#check-structure a 'euler#array 'array-ref)
  (##sys#check-list c 'array-ref)
  (_array-ref a c))

(define (array-set! a c i)
  (##sys#check-structure a 'euler#array 'array-set!)
  (##sys#check-pair c 'array-set!)
  (_array-set! a c i))

(define (array-exists? a c)
  (##sys#check-structure a 'euler#array 'array-exists?)
  (##sys#check-list c 'array-exists?)
  (_array-exists? a c))
