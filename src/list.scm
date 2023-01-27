;; ---
;; functions
;; ---

(define-inline (_delete-at l n)
  (let loop ((tl l) (tn n))
    (if (null? tl)
      (error 'delete-at "out of range" l n)
      (if (= tn 0)
        (cdr tl)
        (cons (car tl) (loop (cdr tl) (- tn 1)))))))

(define-inline (_insert-at l n i)
  (let loop ((tl l) (tn n))
    (if (= tn 0)
      (cons i tl)
      (if (null? tl)
        (error 'insert-at "out of range" l n)
        (cons (car tl) (loop (cdr tl) (- tn 1)))))))

(define-inline (_range s e d)
  (let ((c (cond ((> d 0) <)
                 ((< d 0) >)
                 (else =))))
    (let loop ((i e) (acc '()))
      (if (c i s)
        acc
        (loop (- i d) (cons i acc))))))

(define-inline (_run-length l c)
  (let loop ((l l))
    (if (null? l)
      '()
      (let ((i (car l)))
        (let subloop ((l (cdr l)) (acc 1))
          (if (null? l)
            (cons (list acc i) (loop l))
            (if (c (car l) i)
              (subloop (cdr l) (+ acc 1))
              (cons (list acc i) (loop l)))))))))

(define-inline (_extremum l p c)
  (let loop ((l (cdr l)) (t (p (car l))) (acc (car l)))
    (if (null? l)
      acc
      (let* ((i (car l)) (n (p i)))
        (if (c n t)
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
  (let loop ((l l) (n n))
    (if (= n 0)
      '(())
      (join
        (map
          (lambda (i)
            (map
              (lambda (_)
                (cons _ i))
              l))
          (loop l (- n 1)))))))

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
    ((= n 0) '(()))
    ((> n (length l)) '())
    (else
     (let loop ((l l) (t (list-tail l n)))
      (cond
        ((null? t) (list l))
        ((eq? (cdr l) t) (map list l))
        (else
         (foldr
           (lambda (a b)
             (cons (cons (car l) a) b))
           (loop (cdr l) (cdr t))
           (loop (cdr l) t))))))))

(define-inline (_permutations l)
  (let loop ((l l))
    (cond
      ((null? l)
       '())
      ((null? (cdr l))
       (list l))
      (else
       (let subloop ((l '()) (a (car l)) (b (cdr l)))
         (join
           (list
             (map
               (lambda (_)
                 (cons a _))
               (loop (join (list l b))))
             (if (null? b)
               '()
               (subloop (cons a l) (car b) (cdr b))))))))))

;; ---
;; wrappers
;; ---

(define (delete-at l n)
  (##sys#check-list    l 'delete-at)
  (##sys#check-integer n 'delete-at)
  (when (< n 0)
    (error 'delete-at "out of range" l n))
  (_delete-at l n))

(define (insert-at l n i)
  (##sys#check-list    l 'insert-at)
  (##sys#check-integer n 'insert-at)
  (when (< n 0)
    (error 'insert-at "out of range" l n))
  (_insert-at l n i))

(define (range s e #!optional (d (signum (- e s))))
  (##sys#check-integer s 'range)
  (##sys#check-integer e 'range)
  (##sys#check-integer d 'range)
  (_range s e d))

(define (run-length l #!optional (c =))
  (##sys#check-list    l 'run-length)
  (##sys#check-closure c 'run-length)
  (_run-length l c))

(define (extremum l #!optional (p identity) (c <))
  (##sys#check-pair    l 'extremum)
  (##sys#check-closure p 'extremum)
  (##sys#check-closure c 'extremum)
  (_extremum l p c))

(define (product . l)
  (##sys#check-list l 'product)
  (_product l))

(define (power l n)
  (##sys#check-list    l 'power)
  (##sys#check-integer n 'power)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'power))
  (_power l n))

(define (powerset l)
  (##sys#check-list l)
  (_powerset l))

(define (combinations l n)
  (##sys#check-list    l 'combinations)
  (##sys#check-integer n 'combinations)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'combinations))
  (_combinations l n))

(define (permutations l)
  (##sys#check-list l 'permuations)
  (_permutations l))
