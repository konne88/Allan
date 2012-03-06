(load "graph-tools.lsp")

;-------------------------------------------------------------------------------
; allen-relation: Determines all possible relations that hold between two
;                     given time-spans. Times are passed in the form
;                     500, 1000, 1930, 2125, etc.

(defun allen-relation (sx ex sy ey)
   (cond
       ((< ex sy)                           '< )
       ((< ey sx)                           '> )
       ((and (= sx sy) (= ex ey))           '= )
       ((= ex sy)                           'm )
       ((= ey sx)                           'mi)
       ((and (< sx sy) (> ex sy) (< ex ey)) 'o )
       ((and (< sy sx) (> ey sx) (< ey ex)) 'oi)
       ((and (> sx sy) (< ex ey))           'd )
       ((and (> sy sx) (< ey ex))           'di)
       ((and (= sx sy) (< ex ey))           's )
       ((and (= sx sy) (< ey ex))           'si)
       ((and (> sx sy) (= ex ey))           'f )
       ((and (> sy sx) (= ey ex))           'fi)
    )
)

; Test Cases
(print (list 'allen-relation-test (and (equal (allen-relation 1 2 3 6) '<)
            (equal (allen-relation 1 3 3 6) 'm)
            (equal (allen-relation 1 4 3 6) 'o)
            (equal (allen-relation 3 5 3 6) 's)
            (equal (allen-relation 3 6 3 6) '=)
            (equal (allen-relation 4 6 3 6) 'f)
            (equal (allen-relation 5 8 3 6) 'oi)
            (equal (allen-relation 6 8 3 6) 'mi)
            (equal (allen-relation 7 8 3 6) '>)
            (equal (allen-relation 4 5 3 6) 'd)
            (equal (allen-relation 3 6 3 5) 'si)       
            (equal (allen-relation 3 6 4 5) 'di)       
            (equal (allen-relation 3 6 4 6) 'fi))))  
            




;-------------------------------------------------------------------------------
; parse-time: Parses a time from "18:00" to 1800 or from "08:00" to 800

(defun parse-time (a)
   (setq result 0)
   (setq result (+ result (parse-integer (string (char a 4)))))
   (setq result (+ result (* (parse-integer (string (char a 3))) 10)))
   (setq result (+ result (* (parse-integer (string (char a 1))) 100)))
   (setq result (+ result (* (parse-integer (string (char a 0))) 1000)))
)



;-------------------------------------------------------------------------------
; get-end: Returns the end time for a given step number from a raw step list
;   input: stepno: The step number. formatted as number.
;          steplist: The raw list of steps as returned by read-csv

(defun get-end (stepno steplist)
   (cond
      ((null steplist) nil)
      ((and
         (equal stepno (parse-integer (caar steplist)))
         (equal "Ende" (cadar steplist)))
         (parse-time (caddar steplist)))
      ( T
         (get-end stepno (cdr steplist))
      )
   )
)



;-------------------------------------------------------------------------------
; read-times: Reads and parses the times from the raw step list. Also associates
;             start and end times with the individual steps.

(defun read-times (steplist)
   (read-times-sub steplist steplist)
)



(defun read-times-sub (steplist joinlist)
   (cond
      ((null steplist) nil)
      ((equal "Beginn" (cadar steplist))
         (cons
            (list
               (parse-integer (caar steplist))
               (list
                  (parse-time (caddar steplist))
                  (get-end (parse-integer (caar steplist)) joinlist)))
            (read-times-sub (cdr steplist) joinlist)
          )
       )
    )
)



;-------------------------------------------------------------------------------
; step-relations: Derives the relations from the steps given in times.csv

(defun step-relations ()
   (step-relations-sub
      (unordered-perms
         (read-times
            (read-csv "times.csv")
         )
      )
   )
)



(defun step-relations-sub (x)
   (cond
      ((null x) nil)
      ( T
         (cons
            (list
               (caaar x)
               (caadar x)
               (list
                  (allen-relation
                    (car (cadaar x))
                    (car (cdr (cadaar x)))
                    (car (car (cdadar x)))
                    (car (cdr (car (cdadar x))))
                  )
               )
            )
            (step-relations-sub (cdr x))
         )
      )
   )
)



;-------------------------------------------------------------------------------
; unordered-perms: Creates unordered pairwise permutations of a list        

(defun unordered-perms (x)
   (unordered-perms-sub x x)
)  



(defun unordered-perms-sub (x y)
   (cond
      ((null x) nil)
      ( T
         (append
            (unordered-perms-sub-sub (car x) y)
            (unordered-perms-sub (cdr x) (cdr y))
         )
      )
   )
)



(defun unordered-perms-sub-sub (a x)
   (append
      (cond
         ((equal a (car x)) nil)
         ( T
            (list (list a (car x)))))
      (cond
         ((null (cdr x)) nil)
         ( T
            (unordered-perms-sub-sub a (cdr x))
         )
      )
   )
)

;-------------------------------------------------------------------------------
; Test Cases

(print (list 'get-end-test (and (equal 845 (get-end 2 '(("1" "Beginn" "08:00")
                                                   ("2" "Beginn" "08:15")
                                                   ("4" "Beginn" "08:30")
                                                   ("2" "Ende" "08:45"))))
                                (null (get-end 1 '(("1" "Beginn" "08:00")
                                                   ("2" "Beginn" "08:15")
                                                   ("4" "Beginn" "08:30")
                                                   ("2" "Ende" "08:45")))))))
                                                   
(print (list 'step-relations-test (step-relations)))

(print (list 'unordered-perms-test (equal '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) (unordered-perms '(1 2 3 4)))))
   



(print (list 'read-times-test (equal '((1 (800 815)) (2 (815 845)) (4 (830 900)))
                 (read-times '(("1" "Beginn" "08:00")
                     ("2" "Beginn" "08:15")
                     ("4" "Beginn" "08:30")
                     ("2" "Ende" "08:45")
                     ("1" "Ende" "08:15")
                     ("4" "Ende" "09:00"))
                   ))))  


; do it
(print (check-execution-consistency (step-relations) (relations)))

