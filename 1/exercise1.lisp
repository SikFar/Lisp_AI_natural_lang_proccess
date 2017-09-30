;; 1 List Processing

;;(a)
(caddr '(apple orange pear lemon))

;;(b)
(caadr '((apple orange) (pear lemon)))

;;(c)
(caaddr '((apple) (orange) (pear)))


;;(d)
;;'((apple orange) (pear lemon))
(cons (cons 'apple (cons 'orange nil)) (cons (cons 'pear (cons 'lemon nil)) nil))


;;'((apple) (orange) (pear))
(cons (cons 'apple nil) (cons (cons 'orange nil) (cons (cons 'pear nil) nil)))


;;(e)
(defvar foo '(apple lemon orange pear banana pineapple))

;;e1)
(cadr (reverse foo))
(car (rest (reverse foo)))

;;e2)
(defun next-to-last (lst)
	(if (equal (length lst) 2)
		(car lst)
		(next-to-last (cdr lst))))
(next-to-last foo)


;; 2 Interpreting Common Lisp
;;What is the purpose of the following function?
;;The purpose of the following function is to count nr of elements in the list.

;;How does it achieve its goal?
;;It achieves it's goal by running recursive calls as long as the current list in
;;the recursice thread is not empty. In the case where the if test returns false,
;;it will start counting on the way back.

;;The "rest" function only works with lists as parameter, so we know that
;;the parameter foo has to be list in order to run the function foo without
;;returning any errors. It does not have to be a list with number either.

;;3 Variable Assignment


;;(a)
(let ((foo (list 0 42 2 3)))
	(setf (car foo) 42)
	(first foo))

;;(b)
(let* ((keys '(:a :b :c))
       (values '(0 1 2))
       (pairs (pairlis keys values)))
  (rplacd (assoc :b pairs) 42)
  (rest (assoc :b pairs)))


;;(c)
(let ((foo (make-hash-table)))
	(setf (gethash 'meaning foo) 41)
	(setf (gethash 'meaning foo) 42)
	(gethash 'meaning foo))

;;(d)
(let ((foo (make-array 5)))
	(setf (aref foo 2) 42)
	(aref foo 2))


;;4 Recursion and Iteration

;;(a)
;;Tail-recursive
(defun count-member-a1 (symbol list)
	(defun intern (cnt list)
		(if list
			(if (equal (car list) symbol)
				(intern (+ 1 cnt) (cdr list))			
				(intern cnt (cdr list)))
			cnt))
	(intern 0 list))
(count-member-a1 '1 '(1 2 3 4 6 2 1))


;; "plane" recursive
(defun count-member-a2 (symbol list)
	(if list
		(if (equal (car list) symbol)
			(+ 1 (count-member-a2 symbol (cdr list)))
			(+ 0 (count-member-a2 symbol (cdr list))))
		0))
(count-member-a2 '1 '(1 2 3 4 6 1 2 1))


;;(b)

(defun count-member-b (symbol list)
	(let ((counter 0))
		(loop for item in list 
			if (equal symbol item) do
			(incf counter))
	counter))

(count-member-b '1 '(1 2 3 4 6 1 1 2 1))



;;5 Reading a Corpus File: Basic Counts


(defun tokenize (string)
	(loop
	for start = 0 then (+ space 1)
	for space = (position #\space string :start start)
	for token = (subseq string start space)
	unless (string= token "") collect token
	until (not space)))

(defun trim-left-right (token)
	(setq token (string-right-trim ".,-_!#¤%&/()=?'" token))
	(setq token (string-left-trim ".,-_!#¤%&/()=?'" token))
	token
)
(defun tokenize2 (string)
	(loop
	for start = 0 then (+ space 1)
	for space = (position #\space string :start start)
	for token = (trim-left-right (subseq string start space)) ;; Here is the change
	unless (string= token "") collect token
	until (not space)))


;;(a)
;;Tokenize takes a string a parameter and returns a list with each word
;;(token) in that string as elements. And with-open-file reads a file line by
;;line, and sends each read line as parameter to tokenize.

;;(b)
(defvar *corpus* 
	(with-open-file (stream "brown1.txt" :direction :input)
	(loop
		for line = (read-line stream nil)
		while line
		append (tokenize2 line))))

(length *corpus*)

;;The length(size) og the corpus is 23132 elements.



;;(c)
;;Current strategy doesn't ignore non-alphabetic characters and treats them as
;;words. Also some tokens have initial or trailing punctuation marks, which is
;;not right. But some tokens need their non-alphabetic character in order to 
;;make sense. I feel like names, should be a single token, and not
;;multiple tokens. And also in some cases non-alphabetic character should
;;be a token, instead of latching on to a word.

;;(d)
(defun hash-it (corpus)
	(let ((table (make-hash-table :test 'equal)))
		(loop for token in corpus do
			(if (not (gethash token table))
			(setf (gethash token table) (count-member-b token corpus))
			))
	table))

;;Commentet out so that it doesn't keep runninf for each eval.
(defvar *table* (hash-it *corpus*))
(print (hash-table-count *table*))
;;6311 unique wordtypes are there in *corpus*


;;(f)
(defun trim-left-right (token)
	(setq token (string-right-trim ".,-_!#¤%&/()=?'" token))
	(setq token (string-left-trim ".,-_!#¤%&/()=?'" token))
	token
)
(defun tokenize (string)
	(loop
	for start = 0 then (+ space 1)
	for space = (position #\space string :start start)
	for token = (trim-left-right (subseq string start space)) ;; Here is the change
	unless (string= token "") collect token
	until (not space)))
;;
(print(loop for key being the hash-keys of *table*
        using (hash-value value)
	maximize value into max-value
	finally (return (list max-value key))))

;;The new number for unique token in corpus is 5251
;;The most common token in the navie approach is (1596 "The")
;;In the new redefined, it is also (1597 "The")







