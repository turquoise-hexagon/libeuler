(define (number->list n #!optional (base 10))
  (let loop ((n n) (acc '()))
    (if (= n 0)
      acc
      (let ((q (quotient n base)) (m (modulo n base))) 
        (loop q (cons m acc))))))

(define (list->number lst #!optional (base 10))
  (foldl
    (lambda (acc i)
      (+ (* acc base) i))
    0 lst))

(define (digitsum n #!optional (base 10))
  (let loop ((n n) (acc 0))
    (if (= n 0)
      acc
      (let ((q (quotient n base)) (m (modulo n base)))
        (loop q (+ acc m))))))

(define (palindrome? n #!optional (base 10))
  (let ((lst (number->list n base)))
    (equal? lst (reverse lst))))
