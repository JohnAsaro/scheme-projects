(define heap2 
  (make-vector 0))

(define dist-pt2
  (lambda (pt1 pt2)
    ;(display "dist-pt2")
    ;(newline)
    ;(display "pt1: ") (display pt1) (newline)
    ;(display "pt2: ") (display pt2) (newline)
    (let ((distance (+ (abs (- (car pt1) (car pt2)))
                       (abs (- (cadr pt1) (cadr pt2))))))
      ;(display "distance: ") (display distance) (newline)
      distance)))

(define dist-start-goal2
  (lambda (node)
    ;(display "dist-both2")
    ;(newline)
    ;(display node)
    (+ (dist-pt2 node goal) (dist-pt2 node current-node))))

(define hierarchy2 ;just to make heapify2 more readable
  (lambda (node-1 node-2)
    ;(display "hierarchy2")
    ;(display node-1)
    ;(display node-2)
    (< (dist-start-goal2 node-1) (dist-start-goal2 node-2)))) ;Guess this means its not "true" branch and bound but who cares this is just to construct a path, the other one is proper branch and bound

(define heapify2
  (lambda ()
    ;(display "heapify2")
    (vector-sort! hierarchy2 heap2)))

(define vector-append2
  (lambda (this-heap2 node)
    ;(display "vector-append2")
    (let* ((temp (vector->list this-heap2)))
      ;(display "temp before") (display temp)
      ;(display "temp after")
      ;(display (list->vector (append temp (list node))))
      (list->vector (append temp (list node))))))

(define insert2
  (lambda (node)
    ;(display "insert2")
    (let* ((new-heap2 (vector-append2 heap2 node)))
      (set! heap2 new-heap2) 
      (heapify2)
      ;(newline)
      ;(display "insert complete")
      ;(display heap2)
      )))

(define front2
  (lambda ()
    ;(display "front2")
    ;(display (vector-length heap2))
    (if 
      (=(vector-length heap2) 0) '()
      ;else
      (vector-ref heap2 0))))

(define extract2
  (lambda ()
    ;(display "extract2")
    ;(display heap2)
    ;(display "front of heap2 is")
    ;(display (vector-ref heap2 0))
    (if (=(vector-length heap2) 0) '()
        ;else   
        (let* ((temp (vector->list heap2)) (head (front2)))
          (set! heap2 (list->vector (cdr temp)))
          (heapify2)
          head))))

(define clearheap2
  (lambda ()
    (set! heap2 (make-vector 0))))