(define (range start stop #!optional (step (signum (- stop start))))
  (let ((comp (cond ((> step 0) <)
                    ((< step 0) >)
                    (else =))))
    (let loop ((i stop) (acc '()))
      (if (comp i start)
        acc
        (loop (- i step) (cons i acc))))))

(define (run-length lst #!optional (comp =))
  (let loop ((lst lst))
    (if (null? lst)
      '()
      (let ((head (car lst))
            (tail (cdr lst)))
        (let-values (((a b) (span (lambda (_) (comp _ head)) lst)))
          (cons (list (length a) head) (loop b)))))))

(define (product . lst)
  (foldr
    (lambda (lst acc)
      (join
        (map
          (lambda (i)
            (map (lambda (_) (cons i _)) acc))
          lst)))
    '(()) lst))

(define (power lst n)
  (if (= n 0)
    '(())
    (join
      (map
        (lambda (i)
          (map (lambda (_) (cons _ i)) lst))
        (power lst (- n 1))))))

(define (powerset lst)
  (if (null? lst)
    '(())
    (let ((tmp (powerset (cdr lst))))
      (join (list
              (map
                (lambda (i)
                  (cons (car lst) i))
                tmp)
              tmp)))))

(define (combinations lst n)
  (cond
    ((= n 0)
     '(()))
    ((null? lst)
     '())
    (else
      (join
        (list (map
                (lambda (i)
                  (cons (car lst) i))
                (combinations (cdr lst) (- n 1)))
              (combinations (cdr lst) n))))))
    
(define (permutations lst)
  (cond
    ((null? lst)
     '())
    ((null? (cdr lst))
     (list lst))
    (else
      (let loop ((lst '()) (a (car lst)) (b (cdr lst)))
        (join
          (list (map
                  (lambda (_)
                    (cons a _))
                  (permutations (join (list lst b))))
                (if (null? b)
                  '()
                  (loop (cons a lst) (car b) (cdr b)))))))))
