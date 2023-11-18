;; ---
;; functions
;; ---

(define-inline (_delete-at l n)
  (let loop ((tl l) (tn n))
    (if (null? tl)
      (error 'delete-at "out of range" l n)
      (if (fx= tn 0)
        (##sys#slot tl 1)
        (##sys#cons (##sys#slot tl 0) (loop (##sys#slot tl 1) (fx- tn 1)))))))

(define-inline (_insert-at l n i)
  (let loop ((tl l) (tn n))
    (if (fx= tn 0)
      (##sys#cons i tl)
      (if (null? tl)
        (error 'insert-at "out of range" l n)
        (##sys#cons (##sys#slot tl 0) (loop (##sys#slot tl 1) (fx- tn 1)))))))

(define-inline (_delete-first l i ?)
  (let loop ((l l))
    (if (null? l)
      '()
      (let ((a (##sys#slot l 0)) (b (##sys#slot l 1)))
        (if (? a i)
          b
          (##sys#cons a (loop b)))))))

(define-inline (_range s e d)
  (let ((c (cond ((positive? d) <)
                 ((negative? d) >)
                 (else =))))
    (let loop ((i e) (acc '()))
      (if (c i s)
        acc
        (loop (- i d) (cons i acc))))))

(define-inline (_run-length l ?)
  (let loop ((l l))
    (if (null? l)
      '()
      (let ((i (car l)))
        (let subloop ((l (cdr l)) (acc 1))
          (if (null? l)
            (cons (cons acc i) (loop l))
            (if (? (car l) i)
              (subloop (cdr l) (fx+ acc 1))
              (cons (cons acc i) (loop l)))))))))

(define-inline (_extremum l p ?)
  (let loop ((l (cdr l)) (t (p (car l))) (acc (car l)))
    (if (null? l)
      acc
      (let* ((i (car l)) (n (p i)))
        (if (? n t)
          (loop (cdr l) n i)
          (loop (cdr l) t acc))))))

(define-inline (_product l)
  (define (f i j)
    (define (g k l)
      (define (h m n)
        (cons (cons k m) n))
      (foldr h l j))
    (foldr g '() i))
  (foldr f '(()) l))

(define-inline (_power l n)
  (let loop ((i 0) (acc '()))
    (if (fx= i n)
      (_product acc)
      (loop (fx+ i 1) (cons l acc)))))

(define-inline (_powerset l)
  (let loop ((l l))
    (if (null? l)
      '(())
      (let ((i (loop (cdr l))))
        (join
          (list
            (map
              (lambda (_)
                (cons (car l) _))
              i)
            i))))))

(define-inline (_combinations l n)
  (cond
    ((fx= n 0)
     '(()))
    ((fx> n (length l))
     '())
    (else
     (let loop ((l l) (t (list-tail l n)))
      (cond
        ((null? t)
         (list l))
        ((eq? (cdr l) t)
         (map list l))
        (else
         (foldr
           (lambda (a b)
             (cons (cons (car l) a) b))
           (loop (cdr l) (cdr t))
           (loop (cdr l) t))))))))

(define-inline (_permutations l)
  (let f ((j '()) (k (reverse l)) (l '()))
    (if (null? k)
      (cons (reverse j) l)
      (let g ((a '()) (b k) (c l))
        (if (pair? b)
          (g (cons (car b) a) (cdr b) (f (cons (car b) j) (foldl xcons (cdr b) a) c))
          c)))))

;; ---
;; wrappers
;; ---

(define (delete-at l n)
  (##sys#check-list   l 'delete-at)
  (##sys#check-fixnum n 'delete-at)
  (when (fx< n 0) (error 'delete-at "out of range" l n))
  (_delete-at l n))

(define (insert-at l n i)
  (##sys#check-list   l 'insert-at)
  (##sys#check-fixnum n 'insert-at)
  (when (fx< n 0) (error 'insert-at "out of range" l n))
  (_insert-at l n i))

(define (delete-first l i #!optional (? =))
  (##sys#check-list    l 'delete-first)
  (##sys#check-closure ? 'delete-first)
  (_delete-first l i ?))

(define (range s e #!optional (d (signum (- e s))))
  (##sys#check-integer s 'range)
  (##sys#check-integer e 'range)
  (##sys#check-integer d 'range)
  (_range s e d))

(define (run-length l #!optional (? =))
  (##sys#check-list    l 'run-length)
  (##sys#check-closure ? 'run-length)
  (_run-length l ?))

(define (extremum l #!optional (p identity) (? <))
  (##sys#check-pair    l 'extremum)
  (##sys#check-closure p 'extremum)
  (##sys#check-closure ? 'extremum)
  (_extremum l p ?))

(define (product . l)
  (##sys#check-list l 'product)
  (_product l))

(define (power l n)
  (##sys#check-list      l 'power)
  (##sys#check-fixnum    n 'power)
  (check-positive-fixnum n 'power)
  (_power l n))

(define (powerset l)
  (##sys#check-list l)
  (_powerset l))

(define (combinations l n)
  (##sys#check-list      l 'combinations)
  (##sys#check-fixnum    n 'combinations)
  (check-positive-fixnum n 'combinations)
  (_combinations l n))

(define (permutations l)
  (##sys#check-list l 'permutations)
  (_permutations l))
