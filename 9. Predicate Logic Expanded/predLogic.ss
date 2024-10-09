;Run with (search X) where X is the amount of iterations you want to give the search

;-------------------------------------------------------------------------------------------------------------------------------------
;GLOBAL VARIABLES
(define numcolrow 5) ;we assume a 5x5 grid, this is adjustable
(define path '()) ;to track of blocks we have already "expanded" from
;-------------------------------------------------------------------------------------------------------------------------------------
;RULES FUNCTIONS

(define finish-rule
    (lambda (pt)
      
        ;(newline)
        ;(display "finish rule called with ")
        ;(display pt)
      
        (if 
            (and (isVisited? pt) (isGoal? pt)) ;if block at xy is visited and its the goal
            (begin
                (set! facts (append facts '(finished))) ;append finished to facts, we are done
                #t ;return true
            ) 
            ;else
            #f ;otherwise return false
        )
    )
)

(define move-rule
    (lambda (pt1 pt2)  ;pt1 = (x1 y1), pt2 = (x2 y2)
       
        ;(newline)
        ;(display "move rule called with ")
        ;(display pt1)
        ;(display " and ")
        ;(display pt2)
       
        ;unify takes care of obstacle detection and checking if we have already visited a certain point, so we do not need a rule for those
        (if 
            (and (isVisited? pt1) (unify pt1 pt2)) ;if legal move and we have visited the starting postion
            (begin
                (set! facts (append facts (list (list 'visited pt2)))) ;add new move to facts
                ;(newline)
                ;(display "thus the new facts are ")
                ;(display facts)
                #t ;return true
            )
            ;else
            #f ;return false
        )
    )
)

(define set-height 
    (lambda (pt)

        ;(newline)
        ;(display "set-height called with ")
        ;(display pt)
        (let ((Y (cadr pt))) ;extract Y value
            (cond 
                [(isLow? Y) (set! facts (append facts (list (list 'height-low pt))))] ;if its touching the bottom of the grid it is low
                [(isMiddle? Y) (set! facts (append facts (list (list 'height-middle pt))))] ;if its not touching the bottom or top of the grid its in the middle
                [(isHigh? Y) (set! facts (append facts (list (list 'height-high pt))))] ;if its touching the bottom of the top of the grid it is high
                [else (display "Error, we somehow got out of bounds")]
            )
        )
    )
)

(define set-stability 
    (lambda (pt)

        ;(newline)
        ;(display "set-stability called with ")
        ;(display pt)
        
        (if 
            (isStable? pt) ;if we are not bordering the grids bounds, we are stable
            (begin
                (set! facts (append facts (list (list 'stable pt)))) ;add that pt is stable to facts
                #t ;return true
            )
            ;else
            (begin
                (set! facts(append facts (list (list 'unstable pt)))) ;add that pt is unstable to facts
                #f ;return false
            )
        )
    )
)
;-------------------------------------------------------------------------------------------------------------------------------------
;FACTS

(define facts
  '((goal (2 2)) (visited (0 0)) (obstacle (1 1)) (obstacle (2 1)) (obstacle (1 2))))
;-------------------------------------------------------------------------------------------------------------------------------------
;HELPER FUNCTIONS

(define adjacent
  (lambda (pt)
    
    ;(newline)
    ;(display "adjacent called with ")
    ;(display pt)
    

    (let ((max-bound (- numcolrow 1)) (X (car pt)) (Y (cadr pt)))  ;max bound based on numcolrow
      ;(display " which means X is ") 
      ;(display X)
      ;(display " and Y is ")
      ;(display Y)
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

        ;(newline)
        ;(display "notObs called with ")
        ;(display pt)

        (let loop ((fact-lst facts))
            
            ;(newline)
            ;(display "we are asking if ")
            ;(display (cadr (car fact-lst)))
            ;(display " is an obstacle ")

            (cond
                [(null? fact-lst) #t] ;if we looked through all our facts and we havent found an obstacle at pt, return true
                [(and (eq? (car (car fact-lst)) 'obstacle) ;if this is an obstacle
                    (equal? (cadr (car fact-lst)) pt)) ;if this obstacle is equal to pt
                            ;(begin
                                ;(display " yeah it is an obstacle ")
                                #f ;return false
                            ;)
                    ]
                [else (loop (cdr fact-lst))] ;otherwise keep going
            )
        )
    )
)

(define isVisited? ;checks if we have already visited this
    (lambda (pt) ;pt = (X Y)

        ;(newline)
        ;(display "isVisited? called with ")
        ;(display pt)

        (let loop ((fact-lst facts))
            
            ;(newline)
            ;(display "facts left in fact-lst ")
            ;(display fact-lst)

            (cond
                [(null? fact-lst) #f] ;if we looked through all our facts and we havent found this thing is already visited, than it isnt visited
                [(and (eq? (car (car fact-lst)) 'visited) ;if this is a visited pt
                     (equal? (cadr (car fact-lst)) pt)) ;if this visited pt is equal to pt
                        ;(begin
                            ;(newline)
                            ;(display "we have found that we have already visited ")
                            ;(display (cadr (car fact-lst)))   
                            #t ;return true
                        ;)
                ]
                [else (loop (cdr fact-lst))] ;otherwise keep going
            )
        )
    )
)

(define isGoal?
    (lambda (pt)

        ;(newline)
        ;(display "isGoal? called with ")
        ;(display pt)

        (let loop ((fact-lst facts))
            (cond
                [(null? fact-lst) #f] ;if we looked through all our facts and we havent found this thing is the goal, then we return false
                [(and (eq? (car (car fact-lst)) 'goal) ;if this is the goal
                    (equal? (cadr (car fact-lst)) pt)) ;if goal pt is equal to pt
                            #t] ;return true 
                [else (loop (cdr fact-lst))] ;otherwise keep going
            )
        )
    )
)

(define isPartOfPath?
    (lambda (pt)

        ;(newline)
        ;(display "isPartOfPath? called with ")
        ;(display pt)

        (let loop ((path-left path))
            (cond
                [(null? path-left) #f] ;if we looked through our path, and couldnt find this, that means its not part of the path
                [(equal? (car path-left) pt) ;if we find our pt in path
                        #t ;return true
                ]
                [else (loop (cdr path-left))] ;otherwise keep going
            )
        )
    )
)

(define isLow? ;for height, if its touching the bottom of the grid it is low
    (lambda (Y)
        
        ;(newline)
        ;(display "isLow? called with Y = ")
        ;(display Y)
        
        (if (= Y 0)
            #t  
        ;else
        #f
        )
    )
)

(define isMiddle? ;for height, if its not touching the bottom or top of the grid its in the middle
    (lambda (Y)

        ;(newline)
        ;(display "isMiddle? called with Y = ")
        ;(display Y)

        (if (and (> Y 0) (< Y numcolrow))
            #t
        ;else
        #f
        )
    )
)

(define isHigh? ;for height, if its touching the bottom of the top of the grid it is high
    (lambda (Y)

        ;(newline)
        ;(display "isHigh? called with Y = ")
        ;(display Y)

        (if (= Y numcolrow)
            #t  
        ;else
        #f
        )
    )
)

(define isStable? ;for stability, if we are not bordering the grids bounds, we are stable
    (lambda (pt)

        ;(newline)
        ;(display "isStable? called with ")
        ;(display pt)

        (if 
            (= (length (adjacent pt)) 4) ;if we expanded to 4 adjacent points
            #t
            ;else if we could not expand to 4 adjacent points
            #f ;its bordering the grid so false
        )
    )
)

(define unify
    (lambda (pt1 pt2) ;pt1 = (X1 Y1), pt2 = (X2 Y2)
        
        ;(newline)
        ;(display "unify called with ")
        ;(display pt1)
        ;(display " and ")
        ;(display pt2)

        ;(newline)
        ;(display "alright so we got the the unify loop so just checking, cond 1 = ")
        ;(display (notObs? pt2))
        ;(display " and cond2 = ")
        ;(display (not (isVisited? pt2)))

        (if (and (notObs? pt2) (not (isVisited? pt2))) ;if pt2 is not an obstacle, proceed
            (let loop ((adj (adjacent pt1))) ;check if this is a legal move
                (cond 
                    [(null? adj) #f] ;if we looked through all moves adjacent to pt1, and none of them are pt2, then its not a legal move
                    [(equal? (car adj) pt2) #t] ;if pt2 is adjacent, we return true
                    [else (loop (cdr adj))] ;else keep going
                ))
        ;else
        #f ;pt2 was an obstacle therefore this was not a legal move
        )
    )
)

;-------------------------------------------------------------------------------------------------------------------------------------
;SEARCH

(define search
  (lambda (count)
    
    ;(newline)
    ;(display "current facts in search are ")
    ;(display facts)
    
    (cond
      ((member 'finished facts)
          (begin
            (display "goal found")
            (newline)
            (display "path is ")
            (display path)
            (newline)
            (display "facts are ")
            (display facts)
            (newline)
          )
          )
      ((<= count 0)
        (display "not found")
        (newline))
      (else
        (let*         
            ((firstFact (car facts))) ;iterate through facts

            ;(newline)
            ;(display "let* condition in search iteration ")
            ;(display count)
            ;(display " successfully reached")
           
            (cond ;apply rules appropriately
                [(equal? (car firstFact) 'goal)
                    (begin
                        
                        ;(newline)
                        ;(display "goal cond in search iteration ")
                        ;(display count)
                        ;(display " successfully reached")
                        
                        (finish-rule (cadr firstFact))
                        (set! facts (append (cdr facts) (list firstFact))) ;move on to next fact
                        (search (- count 1)) 
                    )
                ] ;if we are checking the goal, see if we have reached it
                [(equal? (car firstFact) 'visited) ;if we are checking a visited
                    (let ((pt (cadr firstFact))) ;we are now considering the pt connected to this fact
                            
                        ;(newline)
                        ;(display "move cond in search iteration ")
                        ;(display count)
                        ;(display " successfully reached")
                        
                        (if (and (not (isPartOfPath? pt)) (notObs? pt)) ;if our pt is not part of the path already
                        (begin 
                            
                            ;(newline)
                            ;(display "and also we are doing ") 
                            ;(display pt)
                            ;(display " which is not part of path, ")
                            ;(display "path is currently ")
                            ;(display path)
                            ;(display " but when we add to it, it will be ")

                            (set! path (append path (list pt))) ;add considered pt to path

                            ;(display path)

                            (set-height pt) ;apply height rule
                            (set-stability pt) ;apply stability rule
                            (let loop ((adj (adjacent pt))) ;apply the move rule to the adjacent blocks
                                (if
                                    (null? adj) '() ;if we looked through all moves adjacent to pt, terminate
                                    ;else
                                    (begin
                                        (move-rule pt (car adj)) ;try and move to adjacent pt 
                                        (loop (cdr adj)) ;keep going
                                    )
                                )
                            )
                        ))
                        (set! facts (append (cdr facts) (list firstFact))) ;move on to next fact
                        (search (- count 1))
                    )
                    
                 ] 
                [else
                    (set! facts (append (cdr facts) (list firstFact))) ;move on to next fact
                    (search count) ;we didn't really look over something that was relevant so we don't decrease the count
                ]
            )
        )
      )
    )
  )
)
;-------------------------------------------------------------------------------------------------------------------------------------