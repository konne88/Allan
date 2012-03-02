(load "split-sequence.lsp")

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
               (setq rels (cons (caddr line) rels)))
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



