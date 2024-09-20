(define path-lst '())
(define visited 1)
(define current-node start) ;current-node must be accessible anywhere

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
    (pause pause-num)
    (expand robot)
    (let ((next-robot (front)))
      (cond
        ((null? next-robot)
         (display "Cannot reach the goal") (newline))
        ((equal? next-robot goal)
         (set! robot (extract))
         (draw-moved-robot (robot-x) (robot-y))
         (display "Found") (newline)
         (let ((path (get-path goal)))
           (draw-path path)
           (display path))
         (newline))
        ((>= count stop-count)
         (display "Took too long") (newline))
        ((> (get-steps-count next-robot) 1) ;If frontier isn't adjacent
         (draw-visited (car robot) (cadr robot))
         (walk-back (get-path next-robot)) ;walk to the far frontier
         (set! robot (extract))
         (set! current-node robot)
         (search2 grid (+ count 1) stop-count))
        (else
         (draw-visited (car robot) (cadr robot))
         (set! robot (extract))
         (set! current-node robot)
         (draw-moved-robot (robot-x) (robot-y))
         (search2 grid (+ count 1) stop-count))))))

(define walk-back
  (lambda (path)
    ;(display "backtracking along path: ")
    ;(display path)
    ;(newline)
    (cond
      ((or (null? path) (equal? (car path) current-node))
       '()) ;Stop if path is empty or we've reached current-node
      (else
       (let ((next-node (car path)))
         (display "currently stepping to ")
         (display next-node)
         (newline)
         (draw-moved-robot (car next-node) (cadr next-node)) 
         (pause 10000000)  ;Add a small pause between movements
         (walk-back (cdr path)))))))
        
(define get-path
  (lambda (last-node)
    (if (equal? last-node current-node)
        (list current-node)
        (let ((result (assoc last-node path-lst)))
          (if result
              (let ((next-node (cadr result)))
                (append (get-path next-node) (list last-node)))
              '())))))  ;Return an empty list if assoc returns #f

(define get-steps-count ;get g(n)
  (lambda (node)
    ;(newline)
    ;(display "get-steps-count")
    ;(newline)
    (if (equal? node current-node)
        0
        ;else
        (steps-counter node))))

(define steps-counter
  (lambda (last-node)
    (if (equal? last-node current-node)
        0
        (let ((result (assoc last-node path-lst)))
          (if result
              (+ (steps-counter (cadr result)) 1)
              0)))))  ;Return 0 if assoc returns #f

(define draw-path
  (lambda (path)
    (cond 
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))
  
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))