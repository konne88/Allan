;; The set of allan relations 

(defvar *r*)
(setq *r* '(= < > d di o oi m mi s si f fi))

;; The p-matrix

;; Entries for the = relation have been skipped, in this case the relation
;; stays the same

(defvar *pmatrix*)
(setq *pmatrix* 
      '((< (= < > d di o oi m mi s si f fi) (< o m d s) < < (< o m d s) < (< o m d s) < < (< o m d s) <)
        ((= < > d di o oi m mi s si f fi) > (> oi mi d f) > (> oi mi d f)  > (> oi mi d f) > (> oi mi d f) > > >)
        (< > d (= < > d di o oi m mi s si f fi) (< o m d s) (> oi mi d f) < > d (> oi mi d f) d (< o m d s))
        ((< o m di fi) (< oi mi di si) (= d di o oi s si f fi) di (o di fi)(oi di si)
        (o di fi)(oi di si)(o di fi) di (oi di si) di)
        (< (> oi mi di si) (o d s) (< o m di fi) (< o m) (= d di o oi s si f fi) < (oi di si) o (di fi o) (d s o) (< o m))
        ((< o m di fi) > (oi d f) (< oi mi di si)(= d di o oi s si f fi)(> oi mi)(o di fi) > 
        (oi d f) (> oi mi) oi (oi di si))
        (< (> oi mi di si) (o d s) < < (o d s) < (= f fi) m m (d s o) <)
        ((< o m di fi) > (oi d f) > (oi d f) (= s si) > (d f oi) > mi mi)
        (< > d (< o m di fi) (< o m) (oi d f) < mi s (= s si) d (< o m))
        ((< o m di fi) > (oi d f) di (o di fi) oi (o di fi) mi (= s si) si oi di)
        (< > d (oi mi di si) (o d s) (> oi mi) m > d (> oi mi) f (= f fi))
        (< (> oi mi di si) (o d s) di o (oi di si) m (oi di si) o di (= f fi) fi)))

;; The p relation, returning a list even for single results

(defun list-p (x y)
	(let ((pv (p x y)))
		(cond ((atom pv) (list pv))
		      (T pv))))

;; The p relation, returning an atom for single results

(defun p (x y)
  (cond ((EQUAL x '=) y)
        ((EQUAL y '=) x)
        (T (getindex y (cdr *r*) (getindex x (cdr *r*) *pmatrix*)))))

;; Used internally by p

(defun getindex (index referenz inhalt)
  (cond ((null referenz) nil)
        ((null inhalt) nil)
        ((equal index (car referenz)) (car inhalt))
        (T (getindex index (cdr referenz)(cdr inhalt)))))


;; Return true if a list has only one element

(defun one (l)
	(null (cdr l)))

;; Returns the P of two relations, single values are represented as a list
;; x and y need to be lists

(defun uniq-big-p(x y)
  (remove-duplicates (big-p x y)))

;; Returns the P of two relations, single values are represented as a list
;; x and y need to be lists. The return may contain duplicates.

(defun big-p (x y)
  (cond ((AND (one x)(one y)) (list-p (car x) (car y)))
        ((one x) (append (big-p x (list (car y))) (big-p x (cdr y))))
	((one y) (append (big-p (list (car x)) y) (big-p (cdr x) y)))
	(T (append (big-p (list (car x)) y) (big-p (cdr x) y)))))

;; Returns the intersection of two lists of allan relations

(defun list-intersection (L1 L2)
  (cond
   ((null L1) nil)
   ((member (first L1) L2) 
    (cons (first L1) (list-intersection (rest L1) L2)))
   (t (list-intersection (rest L1) L2))))

;; Allan relation inversion

(defun invert-relation (r)
  (cond ((equal r '=) '=)
        ((equal r '<) '>)
	((equal r '>) '<)
	((equal r 'm) 'mi)
        ((equal r 'd) 'di)
        ((equal r 'o) 'oi)
        ((equal r 's) 'si)
        ((equal r 'f)find-cycles 'fi)
	((equal r 'mi) 'm)
        ((equal r 'di) 'd)
        ((equal r 'oi) 'o)
        ((equal r 'si) 's)
        ((equal r 'fi) 'f)))

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

;; Finds the nodes that are referenced in the relations list.

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
  
(print '(B C (f fi =)))
(print '(A C (oi mi d > si)))
(print '(A B (= < >)))

(print (uniq-big-p '(= < >) '(f fi =)))

(print (list-intersection (uniq-big-p '(= < >) '(f fi =)) '(oi mi d > si)))

(print (cdar '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1))))

(let ((a '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1)))
      (b '((1 . 2) (2 . 3) (3 . 1) (4 . 5)))
      (c '((1 . 2) (2 . 3) (3 . 4) (4 . 1) (1 . 3) (2 . 4)))
      (d '((1 . 2) (2 . 3) (3 . 1))))
	(print (find-nodes c))

	(print (start-dfs 1 a))
	(print (start-dfs 2 a))
        (print (start-dfs 3 a))
        (print (start-dfs 4 a))
        (print (start-dfs 5 a))
        (print (start-dfs 1 b))
        (print (start-dfs 4 b))
        (print (start-dfs 1 c))
        (print (start-dfs 2 c))

	(print (repeated-alist '(1 2 3) 1337))

	(print (are-all-nodes-two '(1 2 3) '((1 . 2) (1 . 3) (2 . 2) (3 . 2) (3 . 0))))
        (print (are-all-nodes-two '(1 2 3) '((1 . 2) (1 . 3) (2 . 2) (3 . 1) (3 . 2))))

	(print (count-nodes a (repeated-alist '(1 2 3 4 5) 0)))

	(print (is-connected a))
        (print (is-connected b))
        (print (is-connected c)) 

	(print (find-cycles a))
        (print (find-cycles b))
        (print (find-cycles c))
        (print (find-cycles d))
)


