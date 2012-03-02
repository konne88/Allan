(load "lib/read-csv.lsp")

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
   