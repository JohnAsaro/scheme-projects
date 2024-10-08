(define numcolrow 3) ;we assume a 3x3 grid, this is adjustable but there isnt rules accounting for more than a 3x3 grid

(define rules
  '(
  ;finish conditions 
  ;if visited node x = true and goal node x = true, then finished = true 
  (((and ((isVisited? '(x y)) (isGoal? '(x y))))) (finished))

  ;movement conditions
  ;if legal move make it
  ;unify takes care of obstacle detection and checking if we have already visited a certain point, so we do not need a rule for those
  (((and ((isVisited? '(x1 y1)) (unify '(x1 y1) '(x2 y2))))) (visited '(x2 y2)))
  
  ;get height 
  ((isLow? '(x y)) (height-low '(x y)))
  ((isMiddle? '(x y)) (height-middle '(x y)))
  ((isHigh? '(x y)) (height-high '(x y)))

  ;get stability
  ((isStable? '(x y)) (stable '(x y)))
  ))

(define facts
  ;default facts
  '((goal '(2 2)) (visited '(0 0)) (obstacle '(1 1)) (obstacle '(2 1))))

(define adjacent
  (lambda (X Y)
    (let ((max-bound (- numcolrow 1)))  ;max bound based on numcolrow
      (cond ;make sure we stay in bounds
        [(and (> X 0) (> Y 0) (< X max-bound) (< Y max-bound))
         (list (list (+ X 1) Y) (list X (+ Y 1)) (list (- X 1) Y) (list X (- Y 1)))]
        [(and (= X 0) (> Y 0) (< Y max-bound))
         (list (list (+ X 1) Y) (list X (+ Y 1)) (list X (- Y 1)))]
        [(and (> X 0) (= Y 0) (< X max-bound))
         (list (list (+ X 1) Y) (list X (+ Y 1)) (list (- X 1) Y))]
        [(and (> X 0) (> Y 0) (= X max-bound))
         (list (list (- X 1) Y) (list X (+ Y 1)) (list X (- Y 1)))]
        [(and (> X 0) (> Y 0) (= Y max-bound))
         (list (list (+ X 1) Y) (list (- X 1) Y) (list X (- Y 1)))]
        [(and (= X 0) (= Y 0))
         (list (list (+ X 1) Y) (list X (+ Y 1)))]
        [(and (= X 0) (= Y max-bound))
         (list (list (+ X 1) Y) (list X (- Y 1)))]
        [(and (= X max-bound) (= Y 0))
         (list (list (- X 1) Y) (list X (+ Y 1)))]
        [(and (= X max-bound) (= Y max-bound))
         (list (list (- X 1) Y) (list X (- Y 1)))]
      )
    )
  )
)


(define notObs? ;checks if this is not an obstacle
    (lambda (pt) ;pt = (X Y)
        (let loop ((fact-lst facts))
            (cond
                [(null? fact-lst) #t] ;if we looked through all our facts and we havent found an obstacle at pt, return true
                [(eq? (car (car fact-lst)) 'obstacle) ;if this is an obstacle
                    (if (equal? (cadr (car fact-lst)) pt) ;if this obstacle is equal to pt
                            #f ;return false
                        ;else
                    (loop (cdr fact-lst)))] ;keep going
                [else (loop (cdr fact-lst))] ;otherwise keep going
            )
        )
    )
)

(define isVisited? ;checks if we have already visited this
    (lambda (pt) ;pt = (X Y)
        (let loop ((fact-lst facts))
            (cond
                [(null? fact-lst) #f] ;if we looked through all our facts and we havent found this thing is already visited, than it isnt visited
                [(eq? (car (car fact-lst)) 'visited) ;if this is a visited pt
                    (if (equal? (cadr (car fact-lst)) pt) ;if this visited pt is equal to pt
                            #t ;return true
                        ;else
                    (loop (cdr fact-lst)))] ;keep going
                [else (loop (cdr fact-lst))] ;otherwise keep going
            )
        )
    )
)

(define isGoal?
    (lambda (pt)
        (let loop ((fact-lst facts))
            (cond
                [(null? fact-lst) #f] ;if we looked through all our facts and we havent found this thing is the goal, then we return false
                [(eq? (car (car fact-lst)) 'goal) ;if this is the goal
                    (if (equal? (cadr (car fact-lst)) pt) ;if goal pt is equal to pt
                            #t ;return true
                        ;else
                    (loop (cdr fact-lst)))] ;keep going
                [else (loop (cdr fact-lst))] ;otherwise keep going
            )
        )
    )
)

(define isLow? ;for height, if its touching the bottom of the grid it is low
    (lambda (pt)
        (let ((Y (cadr pt)))
            (if (= Y 0)
                #t  
            ;else
            #f
            )
        )
    )
)

(define isMiddle? ;for height, if its not touching the bottom or top of the grid its in the middle
    (lambda (pt)
        (let ((Y (cadr pt)))
            (if (and (> Y 0) (< Y numcolrow))
                #t
            ;else
            #f
            )
        )
    )
)

(define isHigh? ;for height, if its touching the bottom of the top of the grid it is high
    (lambda (pt)
        (let ((Y (cadr pt)))
            (if (= Y numcolrow)
                #t  
            ;else
            #f
            )
        )
    )
)

(define isStable? ;for stability, if we are not bordering the grids bounds, we are stable
    (lambda (pt)
        (if 
            (= (length (adjacent (car pt) (cadr pt))) 4) ;if we expanded to 4 adjacent points
            #t
            ;else if we could not expand to 4 adjacent points
            #f ;its bordering the grid so false
        )
    )
)

(define unify
    (lambda (p1 p2) ;p1 = (X1 Y1), p2 = (X2 Y2)
        (if (and (notObs? p2) (not (isVisited? p2))) ;if p2 is not an obstacle, proceed
            (let loop ((adj (adjacent (car p1) (cadr p1)))) ;check if this is a legal move
                (cond 
                    [(null? adj) #f] ;if we looked through all moves adjacent to p1, and none of them are p2, then its not a legal move
                    [(equal? (car adj) p2) #t] ;if p2 is adjacent, we return true
                    [else (loop (cdr adj))] ;else keep going
                ))
        ;else
        #f ;p2 was an obstacle therefore this was not a legal move
        )
    )
)

(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))
      
(define ModusPonens2 
  (lambda (rule result)
    (if (rule) ;if rule is true
        result ;spit out rule
    ;else
        '() ;else null
    )
  )
)

(define search
  (lambda (count)
    (cond
      ((member 'finished facts)
          (display "goal found")
          (newline))
      ((<= count 0)
        (display "not found")
        (newline))
      (else
        (let* ((firstRule (car rules))
               (remainingRules (append (cdr rules) (list firstRule)))
               (newFact (ModusPonens firstRule)))
          (cond 
            ((null? newFact) ;result of modus ponens was false
                (set! rules remainingRules)
                (search (- count 1)))
            (else
                (set! rules remainingRules)
                (set! facts (append newFact facts))
                (search (- count 1)))))))))