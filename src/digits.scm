;; ---
;; functions
;; ---

(define-inline (_digitsum n b)
  (let loop ((n n) (acc 0))
    (if (= n 0)
      acc
      (loop (quotient n b) (+ acc (modulo n b))))))

(define-inline (_list->number l b)
  (let loop ((l l) (acc 0))
    (if (null? l)
      acc
      (let ((i (car l)))
        (when (< i 0)
          (##sys#error-bad-exact-uinteger i 'list->number))
        (loop (cdr l) (+ (car l) (* acc b)))))))

(define-inline (_number->list n b)
  (let loop ((n n) (acc '()))
    (if (= n 0)
      acc
      (loop (quotient n b) (cons (modulo n b) acc)))))

(define-inline (_palindrome? n b)
  (let loop ((i n) (r 0))
    (if (= i 0)
      (= n r)
      (loop (quotient i b) (+ (* r b) (modulo i b))))))

;; ---
;; wrappers
;; ---

(define (digitsum n #!optional (b 10))
  (##sys#check-integer n 'digitsum)
  (##sys#check-integer b 'digitsum)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'digitsum))
  (_digitsum n b))

(define (list->number l #!optional (b 10))
  (##sys#check-list    l 'list->number)
  (##sys#check-integer b 'list->number)
  (_list->number l b))

(define (number->list n #!optional (b 10))
  (##sys#check-integer n 'number->list)
  (##sys#check-integer b 'number->list)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'number->list))
  (_number->list n b))

(define (palindrome? n #!optional (b 10))
  (##sys#check-integer n 'palindrome?)
  (##sys#check-integer b 'palindrome?)
  (when (< n 0)
    (##sys#error-bad-exact-uinteger n 'palindrome?))
  (_palindrome? n b))
