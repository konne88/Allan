;-------------------------------------------------------------------------------
; possible-relation: Determines all possible relations that hold between two
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
(print (and (equal (allen-relation 1 2 3 6) '<)
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
(equal (allen-relation 3 6 4 6) 'fi)))       