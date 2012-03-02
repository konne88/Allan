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
  (cond ((null L1) nil)
        ((member (first L1) L2) 
          (cons (first L1) (list-intersection (rest L1) L2)))
        (T (list-intersection (rest L1) L2))))

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
  
(print '(B C (f fi =)))
(print '(A C (oi mi d > si)))
(print '(A B (= < >)))

(print (uniq-big-p '(= < >) '(f fi =)))

(print (list-intersection (uniq-big-p '(= < >) '(f fi =)) '(oi mi d > si)))

