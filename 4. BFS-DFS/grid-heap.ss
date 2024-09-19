(define heap 
  (make-vector 0))

(define left_child ;find left child
  (lambda (index)
    (+(* index 2)1)))  

(define right_child ;find right child
  (lambda (index)
    (+(* index 2)2)))

(define parent_node ;find parent
  (lambda (index)
    (if (= index 0)
        (-1)
        ;else
        (quotient(- index 1) 2))))

;I just realized you can sort heaps so I might have just made most of this for nothing
(define swap ;swap elements i and j in vec
  (lambda (vec i j)
    (let* ((temp_j (vector-ref vec j)))
      (vector-set! vec j (vector-ref vec i))
      (vector-set! vec i temp_j)
      (vec)
      )
    )
  )

(define dist_pt
  (lambda (pt1 pt2)
	;(display "dist_pt")
	;(newline)
	;(display "pt1: ") (display pt1) (newline)
	;(display "pt2: ") (display pt2) (newline)
	(let ((distance (+ (abs (- (car pt1) (car pt2)))
                   	(abs (- (cadr pt1) (cadr pt2))))))
  	;(display "distance: ") (display distance) (newline)
  	distance)))

(define dist_goal
  (lambda (node)
  ;(display "dist_goal")
    (dist_pt node goal)))

(define dist_start
  (lambda (node)
  ;(display "dist_start")
    (dist_pt node start)))

;(define dist_start_goal
;  (lambda (node)
;  (display "dist_both")
;      (+ (dist_goal node) (dist_start node))))

(define dist_start_goal
  (lambda (node)
  ;(display "dist_both")
      (+ (dist_pt node goal) (dist_pt node start))))

(define hierarchy ;just to make heapify more readable
  (lambda (node_1 node_2)
    ;(display "hierarchy")
    ;(display node_1)
    ;(display node_2)
    (< (dist_start_goal node_1) (dist_start_goal node_2)))
    )

(define heapify
  (lambda ()
    ;(display "heapify")
    (vector-sort! hierarchy heap)))

(define vector-append
  (lambda (this_heap node)
   (display "vector-append")
    (let* ((temp (vector->list this_heap)))
      ;(display "temp before") (display temp)
      ;(display "temp after")
      ;(display (list->vector (append temp (list node))))
      (list->vector (append temp (list node))))))

(define insert
  (lambda (node)
  (display "insert")
   (let* ((new-heap (vector-append heap node)))
     (set! heap new-heap) 
     (heapify)
     (newline)
     ;(display "insert complete")
     ;(display heap)
     )))

(define front
  (lambda ()
  (display "front")
    ;(display (vector-length heap))
    (if 
      (=(vector-length heap)0) '())
      ;else
      (vector-ref heap 0)))

;(define front
;  (lambda ()
;  (display "front")
;    ;(display (vector-length heap))
;    (cond 
;      [(=(vector-length heap)0) '()]
;      [(=(vector-length heap)1) (vector-ref heap 0)]
;      [else(car(vector-ref heap 0))])))

(define extract
  (lambda ()
  (display "extract")
  (display heap)
  (display "front of heap is")
  (display (vector-ref heap 0))
    (if (=(vector-length heap)0)
        '()
    ;else   
        (let* ((temp (vector->list heap)) (head (front)))
          (set! heap (list->vector (cdr temp)))
          (heapify)
          head))))

