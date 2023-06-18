;; ---
;; structures
;; ---

(define-record priority-queue
  comparator?
  content)

;; --
;; functions
;; ---

(define-inline (_priority-queue ?)
  (make-priority-queue ? '()))

(define-inline (_priority-queue-empty? q)
  (null? (priority-queue-content q)))

(define-inline (_priority-queue-merge ? a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    (else
     (if (? (car b) (car a))
       (cons (car b) (cons a (cdr b)))
       (cons (car a) (cons b (cdr a)))))))

(define-inline (_priority-queue-merge-pairs ? a)
  (let main ((a a))
    (cond
       ((null? a)
        '())
       ((null? (cdr a))
        (car a))
       (else
        (_priority-queue-merge ?
          (_priority-queue-merge ? (car a) (cadr a))
          (main (cddr a)))))))

(define-inline (_priority-queue-insert q i)
  (let ((? (priority-queue-comparator? q)) (a (priority-queue-content q)))
    (make-priority-queue ? (_priority-queue-merge ? (list i) a))))

(define-inline (_priority-queue-first q)
  (if (_priority-queue-empty? q)
    (error 'priority-queue-first "error - empty priority queue" q)
    (car (priority-queue-content q))))

(define-inline (_priority-queue-rest q)
  (if (_priority-queue-empty? q)
    (error 'priority-queue-rest "error - empty priority queue" q)
    (let ((? (priority-queue-comparator? q)) (a (priority-queue-content q)))
      (make-priority-queue ? (_priority-queue-merge-pairs ? (cdr a))))))

(define-inline (_list->priority-queue l ?)
  (let loop ((l l) (acc (_priority-queue ?)))
    (if (null? l)
      acc
      (loop (cdr l) (_priority-queue-insert acc (car l))))))

(define-inline (_priority-queue->list q)
  (let loop ((q q))
    (if (_priority-queue-empty? q)
      '()
      (cons (_priority-queue-first q) (loop (_priority-queue-rest q))))))

(define-inline (_priority-queue-map->list q p)
  (let loop ((q q))
    (if (_priority-queue-empty? q)
      '()
      (cons (p (_priority-queue-first q)) (loop (_priority-queue-rest q))))))

(define-inline (_priority-queue-for-each q p)
  (let loop ((q q))
    (unless (_priority-queue-empty? q)
      (p (_priority-queue-first q))
      (loop (_priority-queue-rest q)))))

(define-inline (_priority-queue-fold q p a)
  (let loop ((q q) (a a))
    (if (_priority-queue-empty? q)
      a
      (loop (_priority-queue-rest q) (p a (_priority-queue-first q))))))

(define-inline (_priority-queue-filter->list q p)
  (let loop ((q q))
    (if (_priority-queue-empty? q)
      '()
      (let ((_ (_priority-queue-first q)))
        (if (p _)
          (cons _ (loop (_priority-queue-rest q)))
          (loop (_priority-queue-rest q)))))))

(define-inline (_priority-queue-take q n)
  (let loop ((q q) (n n) (acc (_priority-queue (priority-queue-comparator? q))))
    (if (_priority-queue-empty? q)
      acc
      (if (zero? n)
        acc
        (loop (_priority-queue-rest q) (- n 1)
          (_priority-queue-insert acc (_priority-queue-first q)))))))

(define-inline (_priority-queue-drop q n)
  (let loop ((q q) (n n))
    (if (_priority-queue-empty? q)
      q
      (if (zero? n)
        q
        (loop (_priority-queue-rest q) (- n 1))))))

;; ---
;; wrappers
;; ---

(define (priority-queue ?)
  (##sys#check-closure ? 'priority-queue)
  (_priority-queue ?))

(define (priority-queue-empty? q)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-empty?)
  (_priority-queue-empty? q))

(define (priority-queue-insert q i)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-insert)
  (_priority-queue-insert q i))

(define (priority-queue-first q)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-first)
  (_priority-queue-first q))

(define (priority-queue-rest q)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-rest)
  (_priority-queue-rest q))

(define (list->priority-queue l ?)
  (##sys#check-list    l 'list->priority-queue)
  (##sys#check-closure ? 'list->priority-queue)
  (_list->priority-queue l ?))

(define (priority-queue->list q)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue->list)
  (_priority-queue->list q))

(define (priority-queue-map->list q p)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-map->list)
  (##sys#check-closure   p 'priority-queue-map->list)
  (_priority-queue-map->list q p))

(define (priority-queue-for-each q p)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-for-each)
  (##sys#check-closure   p 'priority-queue-for-each)
  (_priority-queue-for-each q p))

(define (priority-queue-fold q p a)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-fold)
  (##sys#check-closure   p 'priority-queue-fold)
  (_priority-queue-fold q p a))

(define (priority-queue-filter->list q p)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-filter->list)
  (##sys#check-closure   p 'priority-queue-filter->list)
  (_priority-queue-filter->list q p))

(define (priority-queue-take q n)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-take)
  (##sys#check-integer   n 'priority-queue-take)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'priority-queue-take))
  (_priority-queue-take q n))

(define (priority-queue-drop q n)
  (##sys#check-structure q 'euler#priority-queue 'priority-queue-drop)
  (##sys#check-integer   n 'priority-queue-drop)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'priority-queue-drop))
  (_priority-queue-drop q n))
