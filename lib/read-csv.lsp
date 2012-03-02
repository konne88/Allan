;;; get split-sequence from http://www.cliki.net/SPLIT-SEQUENCE
(load "lib/split-sequence.lisp")
 
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