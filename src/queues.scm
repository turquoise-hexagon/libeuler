(define-record priority-queue
  rank
  item
  lkid
  rkid)

(define priority-queue-empty
  (make-priority-queue 0 'priority-queue-empty 'priority-queue-empty 'priority-queue-empty))

(define (priority-queue-empty? q)
  (eqv? q priority-queue-empty))

(define (_priority-queue-swap item lkid rkid)
  (let ((rank/lkid (priority-queue-rank lkid))
        (rank/rkid (priority-queue-rank rkid)))
    (if (< rank/rkid rank/lkid)
      (make-priority-queue (+ rank/rkid 1) item lkid rkid)
      (make-priority-queue (+ rank/lkid 1) item rkid lkid))))

(define (priority-queue-merge comp? a b)
  (cond
    ((priority-queue-empty? a) b)
    ((priority-queue-empty? b) a)
    (else
      (let ((item/a (priority-queue-item a))
            (item/b (priority-queue-item b)))
        (if (comp? item/b item/a)
          (_priority-queue-swap item/b (priority-queue-lkid b)
            (priority-queue-merge comp? a (priority-queue-rkid b)))
          (_priority-queue-swap item/a (priority-queue-lkid a)
            (priority-queue-merge comp? (priority-queue-rkid a) b)))))))

(define (priority-queue-insert comp? x p)
  (priority-queue-merge comp?
    (make-priority-queue 1 x priority-queue-empty priority-queue-empty) p))

(define (priority-queue-first p)
  (if (priority-queue-empty? p)
      (error 'priority-queue-first "empty priority queue")
      (priority-queue-item p)))

(define (priority-queue-rest comp? p)
  (if (priority-queue-empty? p)
      (error 'priority-queue-rest "empty priority queue")
      (priority-queue-merge comp? (priority-queue-lkid p) (priority-queue-rkid p))))
