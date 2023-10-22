;; ---
;; functions
;; ---

(define-inline (_list->number l b)
  (let loop ((l l) (acc 0))
    (if (null? l)
      acc
      (let ((i (car l)))
        (unless (< -1 i b)
          (error 'list->number "invalid value" i))
        (loop (cdr l) (+ (* acc b) i))))))

(define-inline (_number->list/fixnum n b)
  (let loop ((n n) (acc '()))
    (if (fx= n 0)
      acc
      (loop (fx/ n b) (cons (fxmod n b) acc)))))

(define-inline (_number->list/bignum n b)
  (let loop ((n n) (acc '()))
    (if (zero? n)
      acc
      (loop (quotient n b) (cons (modulo n b) acc)))))

(define-inline (_digitsum/fixnum n b)
  (let loop ((n n) (acc 0))
    (if (fx= n 0)
      acc
      (loop (fx/ n b) (fx+ acc (fxmod n b))))))

(define-inline (_digitsum/bignum n b)
  (let loop ((n n) (acc 0))
    (if (zero? n)
      acc
      (loop (quotient n b) (+ acc (modulo n b))))))

(define-inline (_palindrome?/fixnum n b)
  (let loop ((i n) (acc 0))
    (if (fx= i 0)
      (fx= n acc)
      (loop (fx/ i b) (fx+ (fx* acc b) (fxmod i b))))))

(define-inline (_palindrome?/bignum n b)
  (let loop ((i n) (acc 0))
    (if (zero? i)
      (= n acc)
      (loop (quotient i b) (+ (* acc b) (modulo i b))))))

;; ---
;; wrappers
;; ---

(define (list->number l #!optional (b 10))
  (##sys#check-list       l 'list->number)
  (##sys#check-integer    b 'list->number)
  (check-positive-integer b 'list->number)
  (_list->number l b))

(define (number->list n #!optional (b 10))
  (##sys#check-integer    n 'number->list)
  (##sys#check-integer    b 'number->list)
  (check-positive-integer n 'number->list)
  (check-positive-integer b 'number->list)
  ((if (and (fixnum? n)
            (fixnum? b))
     _number->list/fixnum
     _number->list/bignum)
   n b))

(define (digitsum n #!optional (b 10))
  (##sys#check-integer    n 'digitsum)
  (##sys#check-integer    b 'digitsum)
  (check-positive-integer n 'digitsum)
  (check-positive-integer b 'digitsum)
  ((if (and (fixnum? n)
            (fixnum? b))
     _digitsum/fixnum
     _digitsum/bignum)
   n b))

(define (palindrome? n #!optional (b 10))
  (##sys#check-integer    n 'palindrome?)
  (##sys#check-integer    b 'palindrome?)
  (check-positive-integer n 'palindrome?)
  (check-positive-integer b 'palindrome?)
  ((if (and (fixnum? n)
            (fixnum? b))
     _palindrome?/fixnum
     _palindrome?/bignum)
   n b))
