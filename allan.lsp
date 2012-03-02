;; Allens Zeitlogik
;; ----------------

;; Die Relationenmenge 

(defvar *r*)
(setq *r* '(= < > d di o oi m mi s si f fi))

;; Die P-Matrix

;; Die Einträge für = wurde ausgelassen, da in diesem Fall die
;; Beziehung immer bestehen bleibt.

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

;; Die p-Relation

(defun list-p (x y)
	(let ((pv (p x y)))
		(cond ((atom pv) (list pv))
		      (T pv))))

(defun p (x y)
  (cond ((EQUAL x '=) y)
        ((EQUAL y '=) x)
        (T (getindex y (cdr *r*) (getindex x (cdr *r*) *pmatrix*)))))

(defun getindex (index referenz inhalt)
  (cond ((null referenz) nil)
        ((null inhalt) nil)
        ((equal index (car referenz)) (car inhalt))
        (T (getindex index (cdr referenz)(cdr inhalt)))))



;; --- Noch zu definieren! Uebungsaufgabe!

;; Die P-Relation (übertragen auf Mengen)

(defun one (l) 
	(null (cdr l)))

(defun uniq-big-p(x y)
  (remove-duplicates (big-p x y)))

; pass only lists!
(defun big-p (x y)
  (cond ((AND (one x)(one y)) (list-p (car x) (car y)))
        ((one x) (append (big-p x (list (car y))) (big-p x (cdr y))))
	((one y) (append (big-p (list (car x)) y) (big-p (cdr x) y)))
	(T (append (big-p (list (car x)) y) (big-p (cdr x) y)))))

;; Die Schnittmenge

(defun list-intersection (L1 L2)
  (cond
   ((null L1) nil)
   ((member (first L1) L2) 
    (cons (first L1) (list-intersection (rest L1) L2)))
   (t (list-intersection (rest L1) L2))))

;; Invertieren einer Relation

(defun invert-relation (r)
  (cond ((equal r '=) '=)
        ((equal r '<) '>)
	((equal r '>) '<)
	((equal r 'm) 'mi)
        ((equal r 'd) 'di)
        ((equal r 'o) 'oi)
        ((equal r 's) 'si)
        ((equal r 'f) 'fi)
	((equal r 'mi) 'm)
        ((equal r 'di) 'd)
        ((equal r 'oi) 'o)
        ((equal r 'si) 's)
        ((equal r 'fi) 'f)))

(defun dfs (node children relations visited) ; (print (list node children relations visited))
  (cond ((and (member node visited) (equal children relations)) visited)
        ((null children) (cons node visited))
        (T (dfs node (cdr children) relations 
	  (cond ((equal node (caar children))
		  (dfs (cdar children) relations relations (cons node visited)))
		((equal node (cdar children))
		  (dfs (caar children) relations relations (cons node visited)))
		(T (cons node visited)))))))

(defun start-dfs (node relations)
  (remove-duplicates (dfs node relations relations '())))

(defun repeated-alist (nodes value)
  (cond ((null nodes) '())
        (T (acons (car nodes) value (repeated-alist (cdr nodes) value)))))

(defun are-all-nodes-two (nodes alist)
  (or (null nodes)
      (and (= (cdr (assoc (car nodes) alist)) 2) 
           (are-all-nodes-two (cdr nodes) alist))))

(defun count-nodes (relations counters)
  (cond ((null relations) counters)
        (T (count-nodes (cdr relations) 
          (let ((counters2 (acons (caar relations) (+ (cdr (assoc (caar relations) counters)) 1) counters)))
	    (acons (cdar relations) (+ (cdr (assoc (cdar relations) counters2)) 1) counters2))))))

(defun is-connected (nodes relations)
  (and (equal (sort nodes '<) (sort (start-dfs (car nodes) relations) '<))
       (are-all-nodes-two nodes (count-nodes relations (repeated-alist nodes 0)))))

(print '(B C (f fi =)))
(print '(A C (oi mi d > si)))
(print '(A B (= < >)))

(print (uniq-big-p '(= < >) '(f fi =)))

(print (list-intersection (uniq-big-p '(= < >) '(f fi =)) '(oi mi d > si)))

(print (cdar '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1))))

(let ((a '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1)))
      (b '((1 . 2) (2 . 3) (3 . 1) (4 . 5) (5 . 4)))
      (c '((1 . 2) (2 . 3) (3 . 4) (4 . 1) (1 . 3) (2 . 4))))
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

	(print (is-connected '(1 2 3 4 5) a))
        (print (is-connected '(1 2 3 4 5) b))
        (print (is-connected '(1 2 3 4) c))


;	(print (are-all-nodes-two '(1 2 3 4 5) (count-nodes a (repeated-alist '(1 2 3 4 5) 0))))
)



; (let ((a '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 1))))
; 	(is-connected '(1 2 3 4 5) a)
; )
