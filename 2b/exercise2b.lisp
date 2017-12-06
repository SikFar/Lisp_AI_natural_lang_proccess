;;	SEE FURTHER DOWN FOR OBLIGATORY EXERCISE 2B
;;	SEE FURTHER DOWN FOR OBLIGATORY EXERCISE 2B
;;	SEE FURTHER DOWN FOR OBLIGATORY EXERCISE 2B
;;	SEE FURTHER DOWN FOR OBLIGATORY EXERCISE 2B
;; 	
;; 	I HAVE USED CLISP AS COMPILER


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
	(matrix (make-hash-table :test #'equal))
	similarity-fn
  	(proximity-matrix (make-hash-table :test #'equal))
	(classes (make-hash-table :test #'equal))
	(class-centroids (make-hash-table :test #'equal))
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
	(let (	(vs (make-vs))
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





;; (c)
;; This task doesn not say anything about how the retrieved feature vector needs to be, just to retrieve it 
(defun get-feature-vector (vector-space word)
	(gethash word (vs-matrix vector-space)))


;;(d)
(defun print-features (vector-space word k)
	(let ( (features (loop for key being the hash-keys of (get-feature-vector vector-space word) using (hash-values value ) collect (cons key value))))
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
		do 	(let ((vector-length (euclidean-length (get-feature-vector vector-space key)))) 
				(loop for key2 being the hash-keys of value
				using (hash-values value2)
				do 	(let ()
						(setf (gethash key2 value) (/ (gethash key2 value) vector-length))))))vector-space))



(defparameter space (length-normalize-vs (read-corpus-to-vs "brown2.txt" "words.txt")))

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


;;(word-similarity space "university" "college")
;;(word-similarity space "college" "bread")
;;(word-similarity space "food" "bread")



;;OBLIGATOTY EXERCISE (2B)

;;1
;;(a)

(defun get-proximity (vs word1 word2)
	(if (gethash word2 (vs-proximity-matrix vs))
		(if (gethash word1 (gethash word2 (vs-proximity-matrix vs))) 
			(gethash word1 (gethash word2 (vs-proximity-matrix vs))))))





(defun compute-proximities (vspace)
	(loop for key1 being the hash-keys in (vs-matrix vspace)
		do (let ()
			(setf (gethash key1 (vs-proximity-matrix vspace)) (make-hash-table :test #'equal))
			(loop for key2 being the hash-keys in (vs-matrix vspace) do
				(if (get-proximity vspace key1 key2)
					(setf 	(gethash key2 (gethash key1 (vs-proximity-matrix vspace)))
						(get-proximity vspace key1 key2))
					(setf 	(gethash key2 (gethash key1 (vs-proximity-matrix vspace)))
						(dot-product (get-feature-vector vspace key1) (get-feature-vector vspace key2)))))))
	vspace)

(setf space (compute-proximities space))


;;(print (get-proximity space "college" "university"))
;;(print (get-proximity space "university" "college"))


;;(b)

(defun find-knn (vspace word &optional (k 5))
	(let ( (neighbours (loop  ;;for i from 1 to k
				  for key being the hash-keys of (gethash word (vs-proximity-matrix vspace)) using (hash-values value)
				  if (not (equal key word)) collect (cons key value))))
		
		;;The sorting happens under
		(setf neighbours (sort neighbours (lambda (x y) (> (cdr x) (cdr y)))))
		(print (subseq neighbours 0 k))
		(subseq neighbours 0 k)))
(find-knn space "egypt")
;;(find-knn space "salt" 1)

;;2 Implementing aRocchio classifer
;;(a)

(defun read-classes (vs classes-file)
;;Read the file into a list blindly to proccess later, aware it is not the most effectice way.
  (with-open-file (stream classes-file :direction :input)
    (setf tmp (loop
                    for word = (read stream nil nil)
                    while word
                    collect word)))

  ;Once a list is created i run the fine tuning under.
  (loop while (not (equal tmp '()))
        do (progn
             (setf (gethash (normalize-token (caar tmp)) (vs-classes vs)) (map 'list #'normalize-token (cadar tmp)))
             (setf tmp (cdr tmp)))
        ))


(read-classes space "classes.txt")


;;For testing purposes

(defun print-classes (vs)
  (loop for key being the hash-keys of (vs-classes vs)
        using (hash-value value)
        do (format t "~S - ~S " key value))
  )


;;(b)
;;Had to make new lengt-normalize function since oblig 2a version only worked with co-occurence matrix and took vs as arg
;;Which would mean I had to change alot of my code, it only took me two seconds to set this up instead.
(defun length-normalize-hash (hash-tab)
	(let ((vector-length (euclidean-length hash-tab))) 
		(loop for key being the hash-keys of hash-tab using (hash-values value) do
			(setf (gethash key hash-tab) (/ value vector-length)))
	hash-tab))


(defun compute-class-centroids (vspace)
		(loop for class being the hash-keys in (vs-classes vspace) using (hash-values class-members) do
			(let (	(class-members-length (length class-members))
				(class-centroid-vec (make-hash-table :test #'equal)))

				(loop for class-member in class-members do
					(let ((class-member-fv (get-feature-vector vspace class-member)))
						(loop for key being the hash-keys of class-member-fv using (hash-values value) do
							(if (gethash key class-centroid-vec)
								(setf (gethash key class-centroid-vec) (+ (gethash key class-centroid-vec) value))
								(setf (gethash key class-centroid-vec) value)))))

			(loop for key being the hash-keys in class-centroid-vec using (hash-values value) do 
				(setf (gethash key class-centroid-vec) (/ value class-members-length)))

			(setf class-centroid-vec (length-normalize-hash class-centroid-vec))
			(setf (gethash class (vs-class-centroids vspace)) class-centroid-vec))))


(compute-class-centroids space)




;; (c)
(defun rocchio-classify (vspace)
	;;Gets all the unlabeled words and loop trough them
	(let ((unknown-members (gethash "unknown" (vs-classes vspace))))
		(loop for unknown-member in unknown-members do
			;;For each unknown word we fin the highest dot-product with class-centroids vectors. And update the classes accordingly
			(let ( (dot-prods (loop for class being the hash-keys of (vs-class-centroids vspace) using (hash-values centroid-vec) 
					while (not (equal class "unknown"))
					collect (cons class (dot-product (get-feature-vector vspace unknown-member) centroid-vec)))))
				(setf dot-prods (sort dot-prods (lambda (x y) (> (cdr x) (cdr y)))))
				(format t "~S : ~S - ~S ~%" unknown-member (caar dot-prods) (cdar dot-prods))
				(setf (gethash (caar dot-prods) (vs-classes vspace)) (append (gethash (caar dot-prods) (vs-classes vspace)) (list unknown-member)))))
	vspace))

(rocchio-classify space)


;;d)
;;The reason for me using that data type is because dot-product was already
;;written and good to go. That way noe extra work needed to be done in order to
;;find the highest score. But the centroid, being the sum of a set of 
;;feature-vectors, will be much less sparse than individual feature-vectors, so 
;;it might be better to use a dense datastructure for it instead of a hash-table.


;; 3 Classification theory
;;(a)
;;While Rocchio works with training data prior to the sorting of unlabeled words
;;kNN find the k nearest words. the results is always based on the training 
;;data, be it Rocchio or kNN. They have the same training data, just a different
;;training process. And Rocchio is supervised, while kNN is unsupervised alg.
;;That is the main differene between them.
;;    In Rocchio classification, the "result" is based on how the training data
;;is processed. Which is an already written overview of words belonging to a class. 
;;So when sorting the unlabeled word, one just have to use the dotproduct of the
;;centroids and feature vectors. While in kNN you simply have to look at the
;;k number of feature vectors with the highest similarity score (dot product)
;;and choose the one's with the highest score.
;;    Rocchio is much more timeconsuming because of the process of handling the
;;training data, compared to kNN which doesnt proccess/manipulates the training data.
;;But outputing the result is much more time consuming for kNN than Rocchio, because
;;that happens at same time as the training.

;;(b)
;;While Rocchio works with a given training data, meaning the work is supervised
;;k-means works unsupervised, a blind algorithm only told what to do, not what to
;;look for.
;;They both use centroids, while Rocchio centroid is unchangeable, the kNN
;;centroid adaptive/updates while looking for and comparing highest score.

;;(c)
;;If we wanted to evaluate Rocchio classification we would have to have to
;;have access to some sort of solution test, for comparison reason obviously.
;; 	How well the classifier performed could be determined by concepts such as
;;'precision', 'accuracy' and 'recall' (by comparing the test dataset with
;; evaluation dataset, which contains targetlabels that are correct) - all of 
;;which say something about the classification performance, but used alone they
;;may have various drawbacks. One may also use combined measures involving e.g.
;;both precision and recall. This approach can also be extended to multi-class
;;classification problems.





