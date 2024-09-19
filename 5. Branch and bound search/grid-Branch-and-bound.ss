(define path-lst '())
(define visited 1)

(define expand 
  (lambda (point)
    ;(display "expand")
    (let ((lst (adjacentv point)))
     ;(display "expand list")
      ;(display lst)
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (let loop ((exhaust-lst lst))
        (if (null? exhaust-lst) '()
       ;else
       (begin (insert (car exhaust-lst)) (loop (cdr exhaust-lst))))
      ))))

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
          (block-set! x visited)
          (set-lst-visited (cdr lst))))))
  
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))

(define search
  (lambda (grid stop-count)
    ;(display "search")
    (block-set! start visited)
    (set! path-lst (list (list start '())))
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)
    ;(display "search2")
    ;(display heap)
    ;(newline)
    (pause pause-num)
    ;(display count)
    ;(newline)
    (expand robot)
    (let ((next-robot (front)))
      (cond
        ((null? next-robot)
          (display "Cannot reach the goal")
          (newline))
        ((equal? next-robot goal)
          (set! robot (extract))
          (draw-moved-robot (robot-x) (robot-y))
          (display "Found")
          (newline)
          (let ((path (get-path goal)))
            (draw-path path)
            (display path))
          (newline))
        ((>= count stop-count)
          (display "Took too long")
          (newline))
        (else
          (draw-visited (car robot) (cadr robot))
          (set! robot (extract))
          (draw-moved-robot (robot-x) (robot-y))
          (search2 grid (+ count 1) stop-count))))))
    
(define get-path
  (lambda (last-node)
  ;(display "get-path")
  ;(display last-node)
    (if (equal? last-node start)
      (list start)
    ;else
      (let ((next-node (cadr (assoc last-node path-lst))))
        (append (get-path next-node) (list last-node))))))
      
      
(define draw-path
  (lambda (path)
    (cond 
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))
  
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))