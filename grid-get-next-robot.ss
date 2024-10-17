(define depth 10) ;depth, adjustable
(define flag #f)

(define r-get-next-robot 
  (lambda (point)
    ;(display "current point")
    ;(display point)
    ;(newline)
    (let* ((lst1 (cons point (adjacento point))) 
           (lst0 (randomize lst1))
           (flst (r-calculate-h lst0))
           (lst (map list flst lst0))
           (better_lst lst))
      (if(not flag)
        (let loop ((curr_depth 0) (r_lst better_lst))
          (if (< curr_depth depth)
              (if (eq? (modulo curr_depth 2) 0) ;if max
                (loop (+ curr_depth 1) (r-minimax_max r_lst))
                ;else
                (loop (+ curr_depth 1) (r-minimax_max r_lst)))
              ;else
              (begin (set! flag #f) r_lst))))
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

(define r-get-next-goal-2
  (lambda (point)
    ;(display "current point")
    ;(display point)
    ;(newline)
    (let* ((lst1 (cons point (adjacento point))) 
           (lst0 (randomize lst1))
           (flst (r-calculate-h-goal lst0))
           (lst (map list flst lst0))
           (better_lst lst))
      (if(not flag)
        (let loop ((curr_depth 0) (r_lst better_lst))
          (if (< curr_depth depth)
              (if (eq? (modulo curr_depth 2) 0) ;if max
                (loop (+ curr_depth 1) (r-minimax_max r_lst))
                ;else
                (loop (+ curr_depth 1) (r-minimax_max r_lst)))
              ;else
              (begin (set! flag #f) r_lst))))
      ;(display "better_lst")
      ;(display better_lst)
      ;(newline)
      (set! queue '())
      (enqueue better_lst)
      (set! queue (reverse queue))
      (let ((num (random 10))
            (len (length lst0))
            (best (front)))
        (cond 
          ((= num 0)
           (list-ref lst0 (random len)))
          (else
           best))))))
      
(define r-calculate-h
  (lambda (lst)
    (map r-h lst)))

(define r-h
  (lambda (point)
    (+ (abs (- (car point) (car goal)))
       (abs (- (cadr point) (cadr goal)))
       )))

(define r-calculate-h-goal
  (lambda (lst)
    (map r-h-goal lst)))

(define r-h-goal
  (lambda (point)
    (+ (abs (- (car point) (car robot)))
       (abs (- (cadr point) (cadr robot))))))   
       
(define r-minimax_max
  (lambda (lst)
    (set! flag #t)
    ;(display "current max lst")
    ;(display lst)
    ;(newline)
    ;(display "current queue")
    ;(display queue)
    ;(newline)
    ;(pause 1000)
    (if (null? lst)
        (begin ;(display "Iterated through list")
         ;(newline)
         '())
        ;else
        (let ((point (cadr (car lst))))  ;Extract point from not weird list
          (if (pair? point)
              (cons (r-get-next-robot point)
                    (r-minimax_max (cdr lst)))
              ;else if its not returning a point then just ignore this iteration
              (begin ;Extract point from weird list
                     (cons (r-get-next-robot (car lst))
                     (r-minimax_max (cdr lst)))
                     ))))))

(define r-minimax_max
  (lambda (lst)
    ;(set! flag #t)
    ;(display "current min lst")
    ;(display lst)
    ;(newline)
    ;(display "current queue")
    ;(display queue)
    ;(newline)
    ;(pause 1000)
    (if (null? lst)
        (begin 
        ;(display "Iterated through list") 
        ;(newline)
        '())
        ;else
        (let ((point (cadr (car lst))))  ;Extract point from weird list
          (if (pair? point)
              (cons (r-get-next-goal-2 point)
                    (r-minimax_max (cdr lst)))
              ;else
              (begin ;Extract point from not weird list
                    (cons (r-get-next-goal-2 (car lst))
                    (r-minimax_max (cdr lst)))
                     ))))))