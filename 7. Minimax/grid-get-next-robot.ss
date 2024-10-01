(define depth 3) ;depth, adjustable
(define flag #f)

(define get-next-robot 
  (lambda (point)
    ;(display "current point")
    ;(display point)
    ;(newline)
    (let* ((lst1 (cons point (adjacento point))) 
           (lst0 (randomize lst1))
           (flst (calculate-h lst0))
           (lst (map list flst lst0))
           (better_lst lst))
      (if(not flag)
        (let loop ((curr_depth 0) (r_lst better_lst))
          (if (< curr_depth depth)
              (if (eq? (modulo curr_depth 2) 0) ;if max
                (loop (+ curr_depth 1) (minimax_max r_lst))
                ;else
                (loop (+ curr_depth 1) (minimax_min r_lst)))
              ;else
              (begin (set! flag #f) r_lst))))
      ;else
      ;(display "better_lst")
      ;(display better_lst)
      ;(newline)
      (set! queue '())
      (enqueue better_lst)
      (let ((num (random 10))
            (len (length lst0))
            (best (front)))
        (cond 
          ((= num 0)
           (list-ref lst0 (random len)))
          (else
           best))))))

            
(define calculate-h
  (lambda (lst)
    (map h lst)))

(define h
  (lambda (point)
    (+ (abs (- (car point) (car goal)))
       (abs (- (cadr point) (cadr goal)))
       )))

(define minimax_max
  (lambda (lst)
    (set! flag #t)
    ;(display "current lst")
    ;(display lst)
    ;(newline)
    ;(display "current queue")
    ;(display queue)
    ;(newline)
    (if (null? lst)
        (begin ;(display "Iterated through list")
         ;(newline)
         '())
        ;else
        (let ((point (cadr (car lst))))  ;Check that this returns a point
          (if (pair? point)
              (cons (get-next-robot point)
                    (minimax_max (cdr lst)))
              ;else if its not returning a point then just ignore this iteration
              (begin ;(display "Error: Expected a pair, got ")
                     ;(display point)
                     ;(newline)
                     (minimax_max(cdr lst))))))))

(define minimax_min
  (lambda (lst)
    ;(set! flag #t)
    ;(display "current lst")
    ;(display lst)
    ;(newline)
    ;(display "current queue")
    ;(display queue)
    ;(newline)
    (if (null? lst)
        (begin 
        ;(display "Iterated through list") 
        ;(newline)
        '())
        ;else
        (let ((point (cadr (car lst))))  ;Check that this returns a point
          (if (pair? point)
              (cons (get-next-goal point)
                    (minimax_min (cdr lst)))
              ;else if its not returning a point then just ignore this iteration
              (begin ;(display "Error: Expected a pair, got ")
                     ;(display point)
                     ;(newline)
                     (minimax_min(cdr lst))))))))