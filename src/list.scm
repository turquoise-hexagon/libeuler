;; ---
;; functions
;; ---

(define-inline (_delete-at l n)
  (let loop ((tl l) (tn n))
    (if (null? tl)
      (error 'delete-at "out of range" l n)
      (if (zero? tn)
        (cdr tl)
        (cons (car tl) (loop (cdr tl) (- tn 1)))))))

(define-inline (_insert-at l n i)
  (let loop ((tl l) (tn n))
    (if (zero? tn)
      (cons i tl)
      (if (null? tl)
        (error 'insert-at "out of range" l n)
        (cons (car tl) (loop (cdr tl) (- tn 1)))))))

(define-inline (_delete-first l i c)
  (let loop ((l l))
    (if (null? l)
      '()
      (let ((a (car l)) (b (cdr l)))
        (if (c a i)
          b
          (cons a (loop b)))))))

(define-inline (_range s e d)
  (let ((c (cond ((positive? d) <)
                 ((negative? d) >)
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
  (let loop ((i 0) (acc '()))
    (if (= i n)
      (_product acc)
      (loop (+ i 1) (cons l acc)))))

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
    ((zero? n)
     '(()))
    ((> n (length l))
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
          (g (cons (car b) a) (cdr b) (f (cons (car b) j) (append (reverse a) (cdr b)) c))
          c)))))

;; ---
;; wrappers
;; ---

(define (delete-at l n)
  (##sys#check-list    l 'delete-at)
  (##sys#check-integer n 'delete-at)
  (when (negative? n)
    (error 'delete-at "out of range" l n))
  (_delete-at l n))

(define (insert-at l n i)
  (##sys#check-list    l 'insert-at)
  (##sys#check-integer n 'insert-at)
  (when (negative? n)
    (error 'insert-at "out of range" l n))
  (_insert-at l n i))

(define (delete-first l i #!optional (c =))
  (##sys#check-list    l 'delete-first)
  (##sys#check-closure c 'delete-first)
  (_delete-first l i c))

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
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'power))
  (_power l n))

(define (powerset l)
  (##sys#check-list l)
  (_powerset l))

(define (combinations l n)
  (##sys#check-list    l 'combinations)
  (##sys#check-integer n 'combinations)
  (when (negative? n)
    (##sys#error-bad-exact-uinteger n 'combinations))
  (_combinations l n))

(define (permutations l)
  (##sys#check-list l 'permutations)
  (_permutations l))
