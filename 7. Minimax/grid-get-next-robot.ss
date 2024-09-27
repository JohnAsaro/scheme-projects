(define depth 2)

(define get-next-robot 
  (lambda (point)
    (display "current point")
    (display point)
    (newline)
    (let* ((lst1 (cons point (adjacento point))) 
           (lst0 (randomize lst1))
           (flst (calculate-h lst0))
           (lst (map list flst lst0))
           (better_lst (minimax lst)))
      (display "better_lst")
      (display better_lst)
      (newline)
      (set! queue '())
      (enqueue lst)
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

(define minimax
 (lambda (lst)
    (display "current lst")
    (display lst)
    (newline)
    (display "current queue")
    (display queue)
    (newline)
    (if (null? lst) 
    '()
    ;else
        (cons (get-next-robot (cadr(car lst)))
              (minimax (cdr lst))))))
