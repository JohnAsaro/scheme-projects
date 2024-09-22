(define path-lst2 '())
(define visited2 2)
(define fix-later '())
(define robot2 robot)

(define expand2
  (lambda (point)
    (newline)
    ;(display "expand2")
    (let ((lst (adjacentv2 point)))
     (display "expand list2")
     (display lst)
      (set-lst-visited2 lst)
      (add-to-path-lst2 lst point)
      (let loop ((exhaust-lst lst))
        (if (null? exhaust-lst) '()
       ;else
       (begin (insert2 (car exhaust-lst)) (loop (cdr exhaust-lst))))))
    ))

(define add-to-path-lst2
  (lambda (lst point)
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! path-lst2 (cons child-parent path-lst2))
         (add-to-path-lst2 (cdr lst) point)))))

(define set-lst-visited2
  (lambda (lst)
    (if (null? lst)
        '()
    ;else
        (let ((x (car lst)))
          (block-set! x visited2)
          (set! fix-later (cons x fix-later))
          (set-lst-visited2 (cdr lst))))))

(define search3
  (lambda (grid count stop-count next-node)
    (display "search3")
    (newline)
    (set! robot2 current-node)
    (block-set! current-node visited2)
    (display "here?")
    (set! fix-later (cons current-node fix-later))
    (set! path-lst2 (list (list current-node '())))
    (search4 grid 1 stop-count next-node)))

(define search4
  (lambda (grid count stop-count next-node)
    (display "search4")
    (display heap2)
    (newline)
    ;(display count)
    ;(newline)
    (expand2 robot2)
    (let ((next-robot (front2)))
      (cond
        ((null? next-robot)
          (display "Cannot reach the goal")
          (newline))
        ((equal? next-robot next-node)
          (set! robot2 (extract2))
          (display "Found")
          (newline)
          (let ((path2 (get-path2 next-node)))
            (display path2)
            (newline)
            (let loop ((exhaust-lst fix-later))
              (if (null? exhaust-lst) '()
                  ;else
                  (begin (block-set! (car exhaust-lst) visited) (loop (cdr exhaust-lst)))))
         (set! fix-later '())
         (clearheap2)
         (display "here?") ;aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

         (set! path-lst '())
            path2))
        ((>= count stop-count)
          (display "Took too long")
          (newline))
        (else
          (set! robot2 (extract2))
          (search4 grid (+ count 1) stop-count next-node))))))

(define get-path2
  (lambda (last-node)
  (display "get-path2")
  (display last-node)
  (newline)
    (if (equal? last-node current-node)
      (list current-node)
    ;else
      (let ((next-node (cadr (assoc last-node path-lst2))))
        (append (get-path2 next-node) (list last-node))))))

(define get-steps-count2
  (lambda (node)
    (newline)
    (display "get-steps-count2")
    (newline)
    (if (equal? node current-node)
        0
        ;else
        (steps-counter2 node))))

(define steps-counter2
  (lambda (last-node)
    (newline)
    (display "steps-counter2")
    (newline)
    (if (equal? last-node current-node)
        0
        ;else
        (let ((next-node (cadr (assoc last-node path-lst2))))
          (+ (steps-counter2 next-node) 1)))))

