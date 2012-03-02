;; Used internally by start-dfs

(defun dfs (node children relations visited)
  (cond ((and (member node visited) (equal children relations)) visited)
        ((null children) (cons node visited))
        (T (dfs node (cdr children) relations 
          (cond ((equal node (caar children))
                  (dfs (cdar children) relations relations (cons node visited)))
                ((equal node (cdar children))
                  (dfs (caar children) relations relations (cons node visited)))
                (T (cons node visited)))))))

;; Runs a depth first search on the graph represented by the relations. 
;; Start is the node where the search is started.

(defun start-dfs (start relations)
  (remove-duplicates (dfs start relations relations '())))

;; Return an association list containing all entries in the nodes list as keys. 
;; Each entry has a value of the value parameter.

(defun repeated-alist (nodes value)
  (cond ((null nodes) '())
        (T (acons (car nodes) value (repeated-alist (cdr nodes) value)))))

;; Checks if all entries in nodes list have the value two
;; in the association list alist.

(defun are-all-nodes-two (nodes alist)
  (or (null nodes)
      (and (= (cdr (assoc (car nodes) alist)) 2) 
           (are-all-nodes-two (cdr nodes) alist))))

;; Counts the number of relations leaving and going to each node.
;; Counters needs to be an association list containing all nodes 
;; as keys with the values set to 0.

(defun count-nodes (relations counters)
  (cond ((null relations) counters)
        (T (count-nodes (cdr relations) 
          (let ((counters2 (acons (caar relations) (+ (cdr (assoc (caar relations) counters)) 1) counters)))
            (acons (cdar relations) (+ (cdr (assoc (cdar relations) counters2)) 1) counters2))))))

;; Finds the nodes that are referenced in the relations list. May return duplicates.

(defun find-nodes (relations) 
  (cond ((null relations) '())
        (T (cons (caar relations) (cons (cdar relations) (find-nodes (cdr relations)))))))

;; Checks if the relations passed form a cycle. It return only true
;; if there is exactly one cycle with no unnecessary relations.

(defun is-connected (relations)
  (let ((nodes (sort (remove-duplicates (find-nodes relations)) '<)))
    (and (equal nodes (sort (start-dfs (car nodes) relations) '<))
         (are-all-nodes-two nodes (count-nodes relations (repeated-alist nodes 0))))))

;; Used internally by find-cycles

(defun relation-combination (used relations)
  (cond ((null relations) (if (is-connected used) (list used) '()))
        (T (append (relation-combination used (cdr relations))
                   (relation-combination (cons (car relations) used) (cdr relations))))))

;; Returns all cycles that can be found in the relations

(defun find-cycles (relations) 
  (relation-combination '() relations))

(let ((a '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1)))
      (b '((1 . 2) (2 . 3) (3 . 1) (4 . 5)))
      (c '((1 . 2) (2 . 3) (3 . 4) (4 . 1) (1 . 3) (2 . 4)))
      (d '((1 . 2) (2 . 3) (3 . 1)))
      (nodes '(1 2 3 4 5)))
        (print (if (and
          (equal (sort (remove-duplicates (find-nodes b)) '<) nodes)
          (equal (sort (start-dfs 1 a) '<) nodes)
          (equal (sort (start-dfs 2 a) '<) nodes)
          (equal (sort (start-dfs 3 a) '<) nodes)
          (equal (sort (start-dfs 4 a) '<) nodes)
          (equal (sort (start-dfs 5 a) '<) nodes)
          (equal (sort (start-dfs 1 b) '<) '(1 2 3))
          (equal (sort (start-dfs 4 b) '<) '(4 5))
          (equal (sort (start-dfs 1 c) '<) '(1 2 3 4))
          (equal (sort (start-dfs 2 c) '<) '(1 2 3 4))
          (equal (repeated-alist '(1 2 3) 1337) '((1 . 1337) (2 . 1337) (3 . 1337)))
          (are-all-nodes-two '(1 2 3) '((1 . 2) (1 . 3) (2 . 2) (3 . 2) (3 . 0)))
          (not (are-all-nodes-two '(1 2 3) '((1 . 2) (1 . 3) (2 . 2) (3 . 1) (3 . 2))))
          (are-all-nodes-two nodes (count-nodes a (repeated-alist nodes 0)))
          (is-connected a)
          (not (is-connected b))
          (not (is-connected c)) 
          (equal (find-cycles a) 
              '(((5 . 1) (4 . 5) (3 . 4) (2 . 3) (1 . 2))))
          (equal (find-cycles b)
              '(((3 . 1) (2 . 3) (1 . 2))) )
          (equal (find-cycles c)
              '(((1 . 3) (4 . 1) (3 . 4)) ((2 . 4) (1 . 3) (4 . 1) (2 . 3)) ((2 . 4) (3 . 4) (2 . 3))
                ((2 . 4) (4 . 1) (1 . 2)) ((2 . 4) (1 . 3) (3 . 4) (1 . 2))
                ((1 . 3) (2 . 3) (1 . 2)) ((4 . 1) (3 . 4) (2 . 3) (1 . 2))))
          (equal (find-cycles d)
              '(((3 . 1) (2 . 3) (1 . 2))))) 
          
          "Graph test successful" "Graph test failed")))
          
              
