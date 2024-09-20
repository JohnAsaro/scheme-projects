;Not an actual heap but doesnt heap just sort of roll off the tounge better than
;sorted array? 

(define heap 
  (make-vector 0))

(define dist-pt
  (lambda (pt1 pt2)
	;(display "dist-pt")
	;(newline)
	;(display "pt1: ") (display pt1) (newline)
	;(display "pt2: ") (display pt2) (newline)
	(let ((distance (+ (abs (- (car pt1) (car pt2)))
                   	(abs (- (cadr pt1) (cadr pt2))))))
  	;(display "distance: ") (display distance) (newline)
  	distance)))

(define dist-start-goal
  (lambda (node)
  ;(display "dist-both")
  ;(newline)
  ;(display node)
      (+ (dist-pt node goal) (get-steps-count node))))

(define hierarchy ;just to make heapify more readable
  (lambda (node-1 node-2)
    ;(display "hierarchy")
    ;(display node-1)
    ;(display node-2)
    (< (dist-start-goal node-1) (dist-start-goal node-2)))
    )

(define heapify
  (lambda ()
    ;(display "heapify")
    (vector-sort! hierarchy heap)))

(define vector-append
  (lambda (this-heap node)
   ;(display "vector-append")
    (let* ((temp (vector->list this-heap)))
      ;(display "temp before") (display temp)
      ;(display "temp after")
      ;(display (list->vector (append temp (list node))))
      (list->vector (append temp (list node))))))

(define insert
  (lambda (node)
  ;(display "insert")
   (let* ((new-heap (vector-append heap node)))
     (set! heap new-heap) 
     (heapify)
     ;(newline)
     ;(display "insert complete")
     ;(display heap)
     )))

(define front
  (lambda ()
  ;(display "front")
    ;(display (vector-length heap))
    (if 
      (=(vector-length heap)0) '())
      ;else
      (vector-ref heap 0)))

(define extract
  (lambda ()
  ;(display "extract")
  ;(display heap)
  ;(display "front of heap is")
  ;(display (vector-ref heap 0))
    (if (=(vector-length heap)0)
        '()
    ;else   
        (let* ((temp (vector->list heap)) (head (front)))
          (set! heap (list->vector (cdr temp)))
          (heapify)
          head))))

