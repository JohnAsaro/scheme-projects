;(define threshold-weights '(((1.5 1 1)))) ;This is an "AND" operator

;(define threshold-weights '(((0.5 1 1)))) ;This is an "OR" operator

(define threshold-weights '(((1.25 1 -.75) (1.3 -.75 1)) 
                           ((.65 1.1 1.1)))) ;This is a "XOR" operator

;(define threshold-weights '(((4 3 3) (-2 -3 -3)) 
;                           ((.8 1 1)))) ;This is a "NOT XOR" operator


(define NN  
  (lambda (lst)
    (NN2 lst threshold-weights)))

(define NN2
  (lambda (lst tw)
    (display lst)
    (newline)
    (if (null? tw)
      lst
    ;else
      (let ((next-level (get-next-level lst (car tw))))
         (NN2 next-level (cdr tw))))))

(define get-next-level
  (lambda (lst twl)
     (if (null? twl)
       '()
     ;else
       (cons (get-node lst (car twl)) (get-next-level lst (cdr twl))))))

(define get-node
  (lambda (lst twn)
    (let ((threshold (car twn))
          (weights (cdr twn)))
      (g (+ (get-activations lst weights) (- threshold))))))

(define get-activations
  (lambda (lst w)
    (if (null? lst)
       0
    ;else
       (+ (* (car lst) (car w)) (get-activations (cdr lst) (cdr w))))))

(define g
  (lambda (x)
    (/ 1 (+ 1 (exp (- x)))))) ;sigmoid fucntion, turns some number into a value between (0, 1)

