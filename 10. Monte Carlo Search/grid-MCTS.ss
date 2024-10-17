;nodes are represented as a list ((xpos ypos) n score UCB (list of children))
(define num-exp 5)
(define C 2)
(define rollout-depth 5)

(define get-next-robot
    (lambda (robot)
      (if (member goal (get-moves robot)) goal)
      (let* ((tree (build-tree robot)) (root (get-root tree)))
        (let* ((children (node-get-children root)) (best (best-child tree children (car children))))
          best))))

(define best-child
  (lambda (tree lst curr-best)
    (cond
      ((null? lst) curr-best)
      (else
        (if (< (pt-get-score tree (car lst)) (pt-get-score tree curr-best))
            (best-child tree (cdr lst) (car lst))
            (best-child tree (cdr lst) curr-best))))))

(define pt-get-score
  (lambda (tree pt)
    (cond
      ((null? tree) '())
      ((equal? (node-get-pt (car tree)) pt) (node-get-score (car tree)))
      (else (pt-get-score (cdr tree) pt)))))
    

(define get-root 
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((and (equal? (node-get-pt (car tree)) robot) (not (equal? (node-get-children (car tree)) '()))) (car tree))
      ((and (equal? (node-get-pt (car tree)) robot) (= (length tree) 1)) (car tree))
      (else (get-root (cdr tree))))))

(define build-tree 
  (lambda (root)
    (build-tree2 0 (list robot 1 1 0 '()) (list (list robot 1 1 0 '())))))

(define build-tree2
  (lambda (count root tree)
    (cond
      ((= count num-exp) tree)
      ((leaf? root)      
        (cond
          ((= (node-get-n root) 1)
           (let ((new-tree tree))
             (let ((new-score (rollout)))
               (let loop ((curr root))
                 (cond
                   ((equal? curr (get-root tree))
                     (set! new-tree (update-tree new-tree root (list (node-get-pt curr) (+ (node-get-n curr) 1) 0 0 (node-get-children curr))))
                     (build-tree2 count (get-root new-tree) new-tree))                        
                   (else
                     (set! new-tree (update-tree new-tree curr (list (node-get-pt curr) (+ (node-get-n curr) 1)
                     (/ (+ new-score (node-get-score curr) (node-get-n curr))) 
                     (UCB (/ (+ new-score (node-get-score curr) (node-get-n curr))) (+ (node-get-n curr) 1) (node-get-n (node-get-parent curr tree)))
                     (node-get-children curr))))
                     (loop (node-get-parent curr tree))))))))  
          (else
            (let ((new-tree tree))              
              (let loop ((moves (get-moves (car root))))
                (if (not (null? moves))
                (begin (set! new-tree (cons (list (car moves) 1 1 9999999 '()) new-tree))
                (loop (cdr moves)))))              
                
                (set! new-tree (update-tree new-tree root (list (node-get-pt root) (node-get-n root) (node-get-score root)
                (node-get-UCB root) (get-moves (car root)))))
              
              (let ((new (list (node-get-pt root) (+ (node-get-n root) 1) (node-get-score root) 
              (UCB (node-get-score root) (+ (node-get-n root) 1) (node-get-n (node-get-parent root tree))) (node-get-children root))))
                (build-tree2 (+ count 1) (get-root new-tree) (update-tree new-tree root new)))))))    
      (else
        (build-tree2 count (highest-UCB (node-get-children root) tree) tree)))))

(define highest-UCB
  (lambda (lst tree)
    (highest-UCB2 lst tree (car lst))))

(define highest-UCB2
  (lambda (lst tree best)
    (cond
      ((null? lst) (pt-get-node tree best))
      (else
        (if (> (pt-get-UCB tree (car lst)) (pt-get-UCB tree best))
            (highest-UCB2 (cdr lst) tree (car lst))
            (highest-UCB2 (cdr lst) tree best))))))

(define pt-get-node
  (lambda (tree pt)
    (cond
      ((null? tree) '())
      ((equal? (node-get-pt (car tree)) pt) (car tree))
      (else (pt-get-node (cdr tree) pt)))))

(define pt-get-UCB
  (lambda (tree pt)
    (cond
      ((null? tree) '())
      ((equal? (node-get-pt (car tree)) pt) (node-get-UCB (car tree)))
      (else (pt-get-UCB (cdr tree) pt)))))
  
      
                                              
(define rollout
  (lambda ()
    (rollout2 robot goal "robot" 0)))

(define rollout2
  (lambda (curr-robot curr-goal turn count)
    (cond
      ((= count rollout-depth) (score curr-robot curr-goal))
      ((equal? curr-robot curr-goal) 0)
      ((equal? turn "robot")
       (let* ((moves (cons robot (get-moves curr-robot))) (move (random-elem moves)))
         (rollout2 move curr-goal "goal" (+ count 1))))
      ((equal? turn "goal")
       (let* ((moves (cons goal (get-moves curr-goal))) (move (random-elem moves)))
         (rollout2 curr-robot move "robot" (+ count 1)))))))

(define get-moves
  (lambda (pt)
    (adjacentv pt)))
         
 ;----------tested and working---------------   
      
(define random-elem
  (lambda (lst)
    (if (not (null? lst))
      (let* ((len (length lst)) (index (random len)))
        (list-ref lst index)))))
                  
(define update-tree
  (lambda (tree old new)
  (cond
    ((null? tree) '())
    ((equal? (car tree) old)
     (cons new (update-tree (cdr tree) old new)))
    (else
     (cons (car tree) (update-tree (cdr tree) old new))))))
                                                                                          
(define score
  (lambda (pt1 pt2)
    (+ (abs (- (car pt1) (car pt2))) (abs (- (cadr pt1) (cadr pt2)))))) 
    
(define node-get-parent
  (lambda (node tree)
   (cond
     ((null? tree) '())
     ((member (node-get-pt node) (node-get-children (car tree))) (car tree))
     (else (node-get-parent node (cdr tree)))))) 
    
(define leaf?
  (lambda (node)
    (if (equal? (node-get-children node) '())  #t #f)))
    
(define UCB
  (lambda (w n big-n)
     (+ w (* C (sqrt (/ (log big-n) n))))))
    
(define node-get-score
  (lambda (node)
    (car (cdr (cdr node)))))
    
(define node-get-UCB
  (lambda (node)
    (car (cdr (cdr (cdr node))))))
      
(define node-get-n
  (lambda (node)
    (cond 
      ((equal? node '()) 13)
      (else (car (cdr node))))))
    
(define node-get-pt
  (lambda (node)
    (car node)))
         
(define node-get-children
  (lambda (node)
    (car (cdr (cdr (cdr (cdr node)))))))
      
