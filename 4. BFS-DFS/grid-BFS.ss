(define path-lst '())
(define visited 1)
;(define stop-count (* num-col-row(* num-col-row num-col-row)))
(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (enqueue lst))))

(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! path-lst (cons child-parent path-lst))
         (add-to-path-lst (cdr lst) point)))))

(define set-lst-visited 
  (lambda (lst)
    (if (null? lst)
        '()
    ;else
        (let ((x (car lst)))
          (draw-pt-frontier x)
          ;(block-set! x visited)
          (set-lst-visited (cdr lst))))))
  
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))

(define search
  (lambda (grid stop-count)
    ;(expand robot)
    (block-set! start visited)
    (enqueue (list start))
    (set! path-lst (list (list start '())))
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)
    ;(display queue) ;for troubleshooting
    ;(begin (display path-lst) (newline)) ;for troubleshooting
    (pause pause-num)
    ;(display count)
    ;(newline)
    (let ((next-robot (front)))
       ;(expand next-robot)
      ;(display (=(get-node grid (car next-robot)(cadr next-robot))-1)) ;for troubleshooting
      ;(display (get-node grid (car next-robot)(cadr next-robot))) ;for troubleshooting     
      ;(display (null? queue)) ;same
      (cond
        ;[(>= count stop-count)'no path] ;it stops when the path is impossible anyway so idk what the point of stop count is
        [(null? queue) '()]
        [(= (get-node grid (car next-robot) (cadr next-robot)) -1) (draw-path (get-path next-robot))] 
        ;[(= (get-node grid (car next-robot) (cadr next-robot)) -1) (begin(display "The goal is ") (display next-robot))] ;couldnt get get path working
        [else (begin 
                (expand next-robot)
                (draw-visited (car next-robot)(cadr next-robot))
                (block-set! next-robot visited)
                (dequeue)
                (search2 grid (+ count 1) stop-count))]
        )
     )))

                
(define get-path 
  (lambda (last-node) ;takes the goal node
        ;(begin (display "last node is")(display last-node))
	(let f ((curr-node last-node) (path '())) ;makes a named let that we use to recursively go through path-lst
          ;(begin (display "the curr node is")(display curr-node))
          (let ((parent (find (lambda (parent) (equal? (car parent) curr-node)) path-lst))) ;Applies "is this pairs child to curr-node" to path-lst, we do this because if a node is pointing to the current node, we have found its parent node 
            ;(begin (newline) (display "path is ") (display path) (newline) (display parent))
            (if (equal? (cadr parent) '()) ;If we couldn't find a parent
                (cons curr-node path) ;That means we found the starting node and thus have a path
                  ;else
                (f (cadr parent) (cons curr-node path))) ;Otherwise keep looking
               ))))
       

(define draw-path
  (lambda (path)
    (cond 
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))
 
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))