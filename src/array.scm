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
    (map
      (lambda (i)
        (_range 0 (fx- i 1) 1))
      l)))

(define-inline (_array-dimensions l)
  (let loop ((i l))
    (if (list? i)
      (cons (length i) (loop (##sys#slot i 0)))
      '())))

(define-inline (_list->array l)
  (let*
    ((c (_array-content    l))
     (d (_array-dimensions l))
     (i (_array-indexes    d)))
    (##sys#make-structure 'euler#array c i d)))

(define-inline (_array->list a)
  (let loop ((i (##sys#slot a 1)))
    (if (vector? i)
      (map loop (vector->list i))
      i)))

(define-inline (_array-content-copy a)
  (let loop ((i (##sys#slot a 1)))
    (if (vector? i)
      (map/vector loop i)
      i)))

(define-inline (_array-copy a)
  (##sys#make-structure 'euler#array
    (_array-content-copy a)
    (##sys#slot a 2)
    (##sys#slot a 3)))

(define-inline (_array-ref a c)
  (let loop ((acc (##sys#slot a 1)) (c c) (d (##sys#slot a 3)))
    (if (null? c)
      acc
      (if (null? d)
        (error 'array-ref "out of range" acc c)
        (if (fxclosed? -1 (##sys#slot c 0) (##sys#slot d 0))
          (loop (##sys#slot acc (##sys#slot c 0)) (##sys#slot c 1) (##sys#slot d 1))
          (error 'array-ref "out of range" acc c))))))

(define-inline (_array-set! a c i)
  (let loop ((acc (##sys#slot a 1)) (c c) (d (##sys#slot a 3)))
    (let ((c (##sys#slot c 0)) (tc (##sys#slot c 1)) (d (##sys#slot d 0)) (td (##sys#slot d 1)))
      (if (fxclosed? -1 c d)
        (if (null? tc)
          (if (null? td)
            (##sys#setslot acc c i)
            (error 'array-set! "out of range" acc c))
          (if (null? td)
            (error 'array-set! "out of range" acc c)
            (loop (##sys#slot acc c) tc td)))
        (error 'array-set! "out of range" acc c)))))

(define-inline (_array-exists? a c)
  (let loop ((c c) (d (array-dimensions a)))
    (if (null? c)
      #t
      (if (null? d)
        #f
        (if (fxclosed? -1 (##sys#slot c 0) (##sys#slot d 0))
          (loop (##sys#slot c 1) (##sys#slot d 1))
          #f)))))

;; ---
;; others
;; ---

(define-record-printer (array a p)
  (let ((_ (_array->list a)))
    (##sys#print "@" #f p)
    (##sys#print  _  #f p)))

;; ---
;; wrappers
;; ---

(define (list->array l)
  (unless (well-formed-list? l)
    (error 'list->array "malformed array list" l))
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
