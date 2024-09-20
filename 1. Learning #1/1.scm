(define is_big
  (lambda (x)
   	(if (> x 1000)
        		#t #f
        )
  )
)

(define sqr
  (lambda (x)
    (* x x)))

(define double
  (lambda (x) (+ x x)))

(define dist
  (lambda (x1 y1 x2 y2)
  	(sqrt (+ (sqr(- x1 x2)) (sqr(- y1 y2))))
    )
  )

(define dist_pt
  (lambda (pt1 pt2)
  	(sqrt (+ (sqr(- (car pt1) (car pt2))) (sqr(- (cadr pt1) (cadr pt2)))))
    )
  )

(define find_tf
  (lambda(lst x)
    (if (= x (car lst))
        #t #f
        )
    )
  )

(define count_all 
  (lambda (lst)
    (if (null? lst)
        0 (+ (count_all (cdr lst)) 1))))

(define find_posit_iterate
  (lambda (lst x count)
    (cond 
      [(null? lst) '()]
      [(eqv? (car lst) x)(- count (count_all lst))]
      [else (find_posit_iterate (cdr lst) x count)])))

(define find_posit
  (lambda (x lst)
    (find_posit_iterate lst x (count_all lst))))

(define init 0)

(define count_twos_iterate
  (lambda (lst count)
    (cond 
      [(null? lst) count]
      [(eqv? (car lst) 2)
      (count_twos_iterate (cdr lst) (+ count 1))]
      [else (count_twos_iterate (cdr lst) count)]
    )
  )
)

(define count_twos
  (lambda (lst)
    (count_twos_iterate lst 0)))
