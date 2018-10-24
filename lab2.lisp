(defun rplc (lst old new)
    (if
        (null lst)
            nil
            (if 
                (= (car lst) old)
                    (cons new (rplc (cdr lst) old new))
                    (cons (car lst) (rplc (cdr lst) old new))
            )
    )
)

(print (rplc (list 1 3 4 5 6 2 3) 3 9))

;;--------------------------------------------------------------------------------

(defun gap-insertion-sort (array predicate gap)
  (let ((length (length array)))
    (if (< length 2) array
      (do ((i 1 (1+ i))) ((eql i length) array)
        (do ((x (aref array i))
             (j i (- j gap)))
            ((or (minusp (- j gap))
                 (not (funcall predicate x (aref array (1- j)))))
             (setf (aref array j) x))
          (setf (aref array j) (aref array (- j gap))))))))
 
(defconstant +gaps+
  '(9841 3280 1093 364 121 40 13 4 1)
  "The best sequence of gaps, according to Donald E. Knuth.")
 
(defun shell-sort (array predicate &optional (gaps +gaps+))
    (assert 
        (eql 1 (car (last gaps))) (gaps)
        "Last gap of ~w is not 1." gaps
    )
    (dolist (gap gaps array)
        (gap-insertion-sort array predicate gap)
    )
)

(print (shell-sort #(5 3 8 1 9 3 4 7) #'>))

;;-------------------------------------------------------------------

(defun set-nth (list n val)
    (if (> n 0)
      (cons (car list)
            (set-nth (cdr list) (1- n) val))
      (cons val (cdr list))
    )
)

(defun swap (seq one two)
    (setq tmp (nth one seq))
    (setq seq(set-nth seq one (nth two seq)))
    (setq seq (set-nth seq two tmp))
    seq
)

(defun cocktail-sort-from-start (seq from to)
    (if (= to from)
        seq
        (progn
            (setq currentElement (nth from seq))
            (setq nextElement (nth (+ from 1) seq))
            (if (> currentElement nextElement)           
                (setq seq (swap seq from (+ from 1)))
            )
            (cocktail-sort-from-start seq (+ from 1) to)
        )
    )
)

(defun cocktail-sort-from-end (seq from to)
    (if (= to from)
        seq
        (progn
            (setq currentElement (nth to seq))
            (setq nextElement (nth (- to 1) seq))
            (if (< currentElement nextElement)           
                (setq seq (swap seq to (- to 1)))
            )
            (cocktail-sort-from-end seq from (- to 1))
        )
    )
)

(defun cocktail-sort (seq start end)
    (if (= start end)
        seq
        (progn
            (setq seq (cocktail-sort-from-start seq start end))
            (setq seq (cocktail-sort-from-end seq start end))
            (setq end (- end 1))
            (setq start (+ start 1))
            (print (list start end))
            (cocktail-sort seq start end)
        )
    )
)

(setq seq '(5 4 3 2 1))

(print seq)
(print (cocktail-sort seq 0 4))