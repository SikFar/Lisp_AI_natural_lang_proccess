;; 1 Theory: Word–context vector space models
;; (a) https://github.com/paleblued0t/uio/blob/master/inf4820/2/a/2a.lisp
;; The current context is defined with Bag of Words, where
;; The features we extract for a given occurrence of a word 
;; will simply consist of all other words co-occurring within 
;; the same sentence. Using this approach as inspiration we
;; can also define another similar context. Instead of putting
;; every word that appears in the same sentence as X, we put N
;; closest words to X and put those in the "bag" instead. 
 

;; 2 Creating the vector space

;; (a)
(defstruct vs 
	matrix
	similarity-fn
)


;; (b)
;;The matrix structure I chose to go with is a hash-table with hash-tables in it.
;;That means each word in words.txt is a key in the matrix, and the value for that
;;word (key) is the feature-vector, another hash-table. The reason that I went
;;for this approach is that hash-tables is a fast and space-efficient way of
;;vectorizing features, i.e. turning arbitrary features into indices in a vector
;;or matrix. It works by applying a hash function to the features and using their
;;hash values as indices directly, rather than looking the indices up in an
;;associative array. Also having the approach of using a combination of some sort
;;with array, list or hash-tables, would make the code look like a mess.


(defun normalize-token (word)
	(trim-left-right (string-downcase word))
)


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
	unless (string= token "") collect (normalize-token token)
	until (not space)))

(setq stop-list
	'("a" "about" "also" "an" "and" "any" "are" "as" "at" "be" "been"
	"but" "by" "can" "could" "do" "for" "from" "had" "has" "have"
	"he" "her" "him" "his" "how" "i" "if" "in" "is" "it" "its" "la"
	"may" "most" "new" "no" "not" "of" "on" "or" "she" "some" "such"
	"than" "that" "the" "their" "them" "there" "these" "they" "this"
	"those" "to" "was" "we" "were" "what" "when" "where" "which"
	"who" "will" "with" "would" "you"))


(defun read-words (words-file)
  (with-open-file (stream words-file)
    (loop for word = (read-line stream nil)
          while word
          collect (car (tokenize2 word)))))

(defun remove-stop-words (sentence)
	(loop for word in stop-list do (setq sentence (remove word sentence :test #'equal)))
	sentence)

(defun read-corpus (corpus-file)
  (with-open-file (stream corpus-file :direction :input)
	(loop
		for line = (read-line stream nil)
		while line
		collect (remove-stop-words (tokenize2 line)))))

(defun write-to-matrix (word sentence matrix)
	(if (member word sentence :test #'equal)

		(loop for w in sentence do
		(if (gethash w (gethash word matrix))
  		  	(setf (gethash w (gethash word matrix)) (+ (gethash w (gethash word matrix)) (count w sentence)))
    			(setf (gethash w (gethash word matrix)) (+ 0 (count w sentence))))))
	matrix)

(defun  read-corpus-to-vs (corpus words)
	(let (	(vs (make-vs :matrix (make-hash-table :test #'equal)))
		(words (read-words words))
		(corpus (read-corpus corpus)))
		
		;Starting off/initializing with filling the hash-table matrix with words from words.txt as key and empty hash-tables as values
		(loop for word in words
			do (setf (gethash word (vs-matrix vs)) (make-hash-table :test #'equal)))

		;;The actual filling of the matrix
		(loop for word in words
			do (let () (loop for sentence in corpus do (progn
					(setf (vs-matrix vs) (write-to-matrix word sentence (vs-matrix vs)))))))	
	vs))

(defparameter space (read-corpus-to-vs "brown2.txt" "words.txt"))



;; (c)
;; This task doesn not say anything about how the retrieved feature vector needs to be, just to retrieve it 
(defun get-feature-vector (vector-space word)
	(gethash word (vs-matrix vector-space)))


;;(d)
(defun print-features (vector-space word k)
	(let ( (features (loop for key being the hash-keys of (get-feature-vector space word) using (hash-values value ) collect (cons key value))))
		 (setf features (sort features (lambda (x y)
				(> (cdr x) (cdr y)))))
		(loop for item in (subseq features 0 k) do
		(format t "~S - ~S ~%" (car item)(cdr item)))))


;;3 Vector operations
;; (a)
(defun euclidean-length (fv)
	(let ((el 0))
		(loop for key being the hash-keys of fv
		using (hash-values value)
		do (setq el (+ el (* value value))))
		(setq el (sqrt el))))



;;(b)
(defun length-normalize-vs (vector-space)
	(let ()
		(loop for key being the hash-keys of (vs-matrix vector-space)
		using (hash-values value)
		do 	(let ((vector-length (euclidean-length (get-feature-vector space key)))) 
				(loop for key2 being the hash-keys of value
				using (hash-values value2)
				do 	(let ()
						(setf (gethash key2 value) (/ (gethash key2 value) vector-length))))))))


(length-normalize-vs space)


;;(c)
(defun dot-product (vector1 vector2)
	(let ((dot-prod 0))
		(loop for key1 being the hash-keys of vector1
		using (hash-values value)
          	do	(if (gethash key1 vector2)
			(setf dot-prod (+ dot-prod (* (gethash key1 vector2) (gethash key1 vector2))))))
	dot-prod))

;;(print (dot-product (get-feature-vector space "potato") (get-feature-vector space "potato")))

(setf (vs-similarity-fn space) #'dot-product)


;;(d)

(defun word-similarity (vs word1 word2)
		(print (dot-product (get-feature-vector vs word1) (get-feature-vector vs word2))))


(word-similarity space "university" "college")
(word-similarity space "college" "bread")
(word-similarity space "food" "bread")











