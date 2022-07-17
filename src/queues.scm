(define priority-queue-empty '())
(define priority-queue-empty? null?)

(define (_priority-queue-merge comp? a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    (else
     (if (comp? (car b) (car a))
       (cons (car b) (cons a (cdr b)))
       (cons (car a) (cons b (cdr a)))))))

(define (_priority-queue-merge-pairs comp? queue)
  (cond
    ((null? queue)
     '())
    ((null? (cdr queue))
     (car queue))
    (else
     (_priority-queue-merge comp?
       (_priority-queue-merge comp? (car queue) (cadr queue))
       (_priority-queue-merge-pairs comp? (cddr queue))))))

(define (priority-queue-insert comp? item queue)
  (_priority-queue-merge comp? `(,item) queue))

(define (priority-queue-first queue)
  (if (null? queue)
    (error 'priority-queue-first "empty priority queue")
    (car queue)))

(define (priority-queue-rest comp? queue)
  (if (null? queue)
    (error 'priority-queue-rest "empty priority queue")
    (_priority-queue-merge-pairs comp? (cdr queue))))

(define (priority-queue->list comp? queue)
  (let loop ((queue queue) (acc '()))
    (if (null? queue)
      (reverse acc)
      (loop (priority-queue-rest comp? queue)
        (cons (priority-queue-first queue) acc)))))

(define (list->priority-queue comp? lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
      acc
      (loop (cdr lst) (priority-queue-insert comp? (car lst) acc)))))
