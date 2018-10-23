(defun createList (number)
	(if (>= number 0)
	  (list '+ (mod number 2))
	  (list '- (mod number 2)))
	)
(print (createList 20))


(print 
  (funcall 
  (lambda (lst1 lst2 lst3)
  	(list (car lst1) (car lst2) (car lst3))
	)
	(list 1 2 3) (list 4 5 6) (list 7 8 9)))



(defun mergeLists (lst1 lst2 lst3)
  (append lst1 lst2 lst3)
  )

(print (mergeLists (list 1 2 3) (list 4 5 6) (list 7 8 9)))
