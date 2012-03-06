(load "split-sequence.lsp")
(load "allan.lsp")
(load "graph.lsp")

;-------------------------------------------------------------------------------
; read-csv: Reads a table from a csv-file to a nested list
 
(defun read-csv (file)   
   
   (setq result '())
   
   (with-open-file (stream file) ;; open the file specified
      (read-line stream nil)
      (do ((line (read-line stream nil) 	;;; initialize LINE with with read-line
                 (read-line stream nil))) ;;; the increment step (result gets stored in LINE)
          ((null line)) 			;;; termination condition (read-line returns NIL on EOF)
 
	        ;;; split the read string on a semicolon using the split-sequence function
	      (setq result
            (append result (list (split-sequence:SPLIT-SEQUENCE #\Semicolon line )))
         )
      )
      
      result
   )
)



;-------------------------------------------------------------------------------
; node-pairs-rec: Recursive part of the node-pair creation from the raw csv-table

(defun node-pairs-rec (x)
   (cond
      ((NULL x) NIL)
      ( T
         (cons
            (list*
               (parse-integer (caar x))
               (parse-integer (cadar x)))
            (node-pairs-rec (cdr x))
         )
      )
   )
)



;-------------------------------------------------------------------------------
; node-pairs: Creates dotted pairs from the relations specified in relations.csv

(defun node-pairs ()
   (node-pairs-rec (read-csv "relations.csv"))
)



;-------------------------------------------------------------------------------
; read-relations-rec: recursive part of the read-relations function

(defun read-relations-rec (x)
   (cond
      ((null x) nil)
      ( T
         (cons
            (list
               (parse-integer (caar x))
               (parse-integer (cadar x))
               (caddar x))
            (read-relations-rec (cdr x))
         )
      )
   )   
)



;-------------------------------------------------------------------------------
; read-relations: Reads the edges and their relations from relations.csv.
 
(defun read-relations ()
   (read-relations-rec (read-csv "relations.csv"))
)



;-------------------------------------------------------------------------------
; relations: Reads the edges and their relations from relations.csv and compresses
;            the list. Each edge will have a single entry of the form
;            (node_A node_B (rel_1 rel_2 ... rel_n)).

(defun relations ()
   
   (setq lines (read-relations ))
   (setq pairs (remove-duplicates (node-pairs )))
   (setq result '())
   
   (loop for pair in pairs do
      (setq rels '())
      (loop for line in lines do
         (cond
            ((equal (list (car pair) (cdr pair))
                    (list (car line) (cadr line)))
               (setq rels (cons (string-relation-to-token (caddr line)) rels)))
         )
      )
      (setq result
         (append
            result
            (list
               (append (list (car pair) (cdr pair)) (list rels))
            )
         )
      )
   )
   
   (kill-duplicates result)
)



;-------------------------------------------------------------------------------
; kill-duplicates: removes duplicates in the sense of the EQUAL function from the
;                  top level of a list

(defun kill-duplicates (x)
   (setq result '())
   
   (loop for elem in x do
      (cond
         ((not (real-member elem result))
            (setq result
               (append
                  result
                  (list elem)
               )
            ))
      )
   )
   result
)



;-------------------------------------------------------------------------------
; real-member: Checks if a given element is a member in the sense of the EQUAL
;              function of a given list. Only searches the top level
;              (real-member '(a b c) '((a b c) (d e) f g)) => T

(defun real-member (e x)
   (cond
      ((null x) nil)
      ( T
         (or
            (equal e (car x))
            (real-member e (cdr x))
         )
      )
   )
)

; Used by simplify-relations
(defun simplify-relation (r) 
  (cons (car r) (cadr r)))

; Return the same edges as in rs but without the allen relations
(defun simplify-relations (rs) 
  (cond ((null rs) '())
        (T (cons (simplify-relation (car rs)) (simplify-relations (cdr rs))))))

; Used by unsimplify-relations
(defun unsimplify-relation (simple rs)
  (cond ((null rs) '())
        ((and (equal (car simple) (caar rs))
              (equal (cdr simple) (cadar rs))) (caddar rs))
        ((and (equal (car simple) (cadar rs)) 
              (equal (cdr simple) (caar rs))) (invert-relations (caddar rs)))
        (T (unsimplify-relation simple (cdr rs)))))

; Returns the edges of simple but with the according allen relations
; from rs attached
(defun unsimplify-relations (simple rs)
  (cond ((null simple) '())
        (T (cons (list (caar simple) (cdar simple) (unsimplify-relation (car simple) rs)) 
                 (unsimplify-relations (cdr simple) rs)))))

; Takes a list of 3 connected edges and transforms them in the following way
; Returns a list with 3 items (a b c) so that a= A->B b= B->C and c= A->C (dot notation)
(defun normalize-relations (rs)
  (let ((rels (remove-duplicates (list (caar rs) (cdar rs) (caadr rs) (cdadr rs))))) 
    (list (cons (car rels) (cadr rels)) 
          (cons (cadr rels) (caddr rels))
          (cons (car rels) (caddr rels)))))

; Checks if the simplified cycle is consistent
; Used by check-relations
(defun check-cycles(cycls rs) 
  (cond ((null cycls) T)
        (T (let ((rels (unsimplify-relations (normalize-relations (car cycls)) rs)))
          (and (are-relations-consistent (caddar rels) (caddar (cdr rels)) (caddar (cddr rels)))
               (check-cycles (cdr cycls) rs))))))

; Checks if the passed allen relations are consistent
(defun check-relations (rs)
  (check-cycles (find-cycles (simplify-relations rs)) rs))

; Used internally by intersect-execution-relations
(defun intersect-execution-relation (execs r)
  (cond ((null execs) (caddr r))
        ((and (equal (caar execs) (car r))
              (equal (cadar execs) (cadr r)))
                (list-intersection (caddar execs) (caddr r)))
        ((and (equal (caar execs) (cadr r))
              (equal (cadar execs) (car r)))
                (list-intersection (invert-relations (caddar execs)) (caddr r)))
        (T (intersect-execution-relation (cdr execs) r))))

; Intersect all allen relations of the exectuion plan with
; the receipy plan's relations
(defun intersect-execution-relations (execs rs) 
  (cond ((null rs) '())
        (T (cons (list (caar rs) (cadar rs) (intersect-execution-relation execs (car rs)))
                 (intersect-execution-relations execs (cdr rs))))))

; Check that the execution relations are consistent with the
; allen relations defined in the recipe 
(defun check-execution-consistency (execs rs)
  (check-relations (intersect-execution-relations execs rs)))

(print (if (let ((rel '((1 2 (= <)) 
                        (2 3 (= >)) 
                        (3 4 (o oi)) 
                        (4 2 (fi))
                        (4 5 (<))))
                 (exe '((1 2 (<))
                        (3 4 (f o)) 
                        (2 4 (f <)) 
                        (5 4 (=)))))
  (let ((simp (simplify-relations rel)))
    (and 
      (equal simp '((1 . 2) (2 . 3) (3 . 4) (4 . 2) (4 . 5)))
      (equal (unsimplify-relations (cons '(2 . 1) simp) rel) (cons '(2 1 (= >)) rel))
      (equal (normalize-relations (car (find-cycles simp))) '((2 . 3) (3 . 4) (2 . 4)))
      (check-relations rel)))
      (equal (intersect-execution-relations exe rel)
             '((1 2 (<)) (2 3 (= >)) (3 4 (o)) (4 2 (fi)) (4 5 ())))
      (not (check-execution-consistency exe rel)))
  "Main test successful" "Main test failed"))
