(load "split-sequence.lsp")
 
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

(defun node-pairs ()
   (node-pairs-rec (read-csv "relations.csv"))
)

(print (node-pairs ))   