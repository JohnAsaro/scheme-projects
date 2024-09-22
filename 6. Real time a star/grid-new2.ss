(define adjacent2
  (lambda (block)
    (let ((x (car block))
          (y (cadr block)))
      (append 
        (if (< y 1) '() (list (list x (- y 1))))
        (if (< x 1) '() (list (list (- x 1) y)))
        (if (>= y (- num-col-row 1)) '() (list (list x (+ y 1))))
        (if (>= x (- num-col-row 1)) '() (list (list (+ x 1) y)))))))

(define stepo2
  (lambda (b c)
    (let ((b-status (block-status b))
          (c-status (block-status c))
          (x-diff (abs (- (car b) (car c))))
          (y-diff (abs (- (cadr b) (cadr c)))))
      (if (or (= b-status obstacle) 
              (= c-status obstacle) 
              (not (= (+ x-diff y-diff) 1)))
          #f
      ;else
          c))))

(define stepv2
  (lambda (b c)
    (let ((b-status (block-status b))
          (c-status (block-status c))
          (x-diff (abs (- (car b) (car c))))
          (y-diff (abs (- (cadr b) (cadr c)))))
      (if (or (= b-status obstacle)
              (= c-status obstacle)
              (= c-status 2)
              (= c-status 0) ;We cant go to unexplored nodes thats crazy!
              (not (= (+ x-diff y-diff) 1)))
          #f
      ;else
          c))))

(define step2
  (lambda (b c)
    (let ((c-status (block-status c)))
      (if (= c-status obstacle)
          #f
      ;else
          c))))

(define adjacentv2
  (lambda (block)
    (let* ((adj-lst0 (adjacent2 block))
           (adj-lst1 (map (lambda (z) (stepv2 block z)) adj-lst0)))
      (remove-f2 adj-lst1))))

(define remove-f2
  (lambda (lst)
    (if (null? lst)
        '()
    ;else
        (let ((b (car lst)))
          (if b 
              (cons b (remove-f2 (cdr lst)))
          ;else
              (remove-f2 (cdr lst)))))))
