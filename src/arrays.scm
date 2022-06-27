(define-record array content dimensions indexes)

(define (_array-content lst)
  (let loop ((tmp lst))
    (if (list? tmp)
      (list->vector (map loop tmp))
      tmp)))

(define (_array-dimensions lst)
  (let loop ((tmp lst))
    (if (list? tmp)
      (cons (length tmp) (loop (car tmp)))
      '())))

(define (_array-indexes lst)
  (apply product
    (map
      (lambda (i)
        (range 0 (- i 1)))
      lst)))

(define (_array-copy array)
  (let loop ((tmp (array-content array)))
    (if (vector? tmp)
      (vector-copy (vector-map loop tmp))
      tmp)))

(define (list->array lst)
  (let* ((content    (_array-content    lst))
         (dimensions (_array-dimensions lst))
         (indexes    (_array-indexes    dimensions)))
    (make-array content dimensions indexes)))

(define (array->list array)
  (let loop ((tmp (array-content array)))
    (if (vector? tmp)
      (map loop (vector->list tmp))
      tmp)))

(define (array-copy array)
  (make-array
    (_array-copy      array)
    (array-dimensions array)
    (array-indexes    array)))

(define (array-ref array coord)
  (foldl vector-ref (array-content array) coord))

(define (array-set! array coord value)
  (let loop ((tmp (array-content array)) (coord coord))
    (apply
      (lambda (index . coord)
        (let ((next (vector-ref tmp index)))
          (if (vector? next)
            (loop next coord)
            (vector-set! tmp index value))))
      coord)))

(define (array-exists? array coord)
  (every
    (lambda (index dimension)
      (< -1 index dimension))
    coord (array-dimensions array)))
