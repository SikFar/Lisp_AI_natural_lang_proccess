;(1)
(print (length (cons 42 (cons (cons 42 nil) nil))))
;(a) FERDIG
; 2 elements. It will return 2
(print (length '(1 '(1 nil))))
(print '(1 '() 1))
;(B)
;They all have in common that you can set them up with many dimensions.
;But when you have something like transmission and emission matrix that ends up being very sparse,
;and need to store values with keys , you'd rather work with hash-maps since they scale much better and store values with keys.
;And with arrays you have to initialize it with a certain size and dimension, which is a unknown parameters
;when you are working with algs like HMM. Hash-maps dont need to worry about size and dimensions, since you it grow as when it
;needs to. List are ineffeciently when it comes to large dataset and array are restricted to numeric index. Hash-table are very effective 
;to store large dataset and the index can be anything. The emission matrix is sparsely populated since the vector space model is so 
; high dimensional and the occurence of non-zero values are low. if set up properlu hash-maps can solve this problem

;c)
; non-destructive
(defun ditch (item lst)
	;(loop for i in lst if (not (equal i item)) collect i)
	(if lst
		(if (equal item (car lst))
			(ditch item (cdr lst))
			(cons (car lst) (ditch item (cdr lst)))			
		)
		'()
	)
)
(print (ditch 'c '(a b c d e c)))
(print (ditch 'f '(a b c '(f) e c)))

; A non-destructive function allocates new memory for the list generated.
(defun ditch (item lst)
	(loop for i from 0 to (- (length lst) 1) do 
		(if (equal (nth i lst) item) 
			(if (= i 0) 
				(setf lst (cdr lst))
				(setf (nth i lst) (cdr lst))
				
			)
		)
		(setf lst ())
	)
)


(print (ditch 'c '(a b c d e c)))
(print (ditch 'f '(a b c '(f) e c)))

;In my first solution I removed the item from the list and created a new list, that non-destructively.
;Since it is a loop the base case is when you reach the end of list. In the other version im replacing 
;the item with nil in same list, rather than creating a new list.


;2
;(a)FERDIG
; You have windows based context, which is the x closest words to the word of interest, both left and right.
; Then you have BoW, which is all the co-occuring words, ignoring the linear ordering. The third is the 
; grammatical context, the grammatical relation to other words. With window based our context is limited
; by a parameter which, which can be impractical if you need to look at the bigger picture. BoW can be either
; sentence based og documents based which is "more" context based than looking at n closest words. Grammer
; based context is the best solution since it look at the relations words have to each other. 


; (b)FERDIG
; (i) o, because rocchio is centroid based. Meaning it will calculate a centroid for each class, and
; assign the new object to the class the centroid which is closest belongs to. 
; (ii) x, because 1NN means a 1 Nearest Neighbour. The alg is know as knn which assigns new objects to the class
; the majority of the its k neighbours belong to. And the one closest neigbour to ? is X.

;(c)

;3
;(a)FERDIG
; P(wi|wi-n+1... wi-1) = C(wi-n+1...wi) / C(wi-n+1...wi-1)
; bi-gram: P(wi|wi-1) = C(wi-1 wi)/C(wi-1)
; tri-gram: P(wi|wi-2 wi-1) = C(wi-2...wi)/C(wi-2 wi-1)

;(b)
; The problem with sparseness and naive n-gram language model is that model/formula gives zero probabilty for words
; or sentences that is not a part of training model. Smoothing is based on the idea that you want to smooth out the model by
; elemanig hard values like 0. Laplace smoothin is when you assume all observerations/sequence occure at least once.
; P(wi|wi-1) = C(wi-1 wi)+1 / C(wi-1) + V where V is the size of the vocabulary.


;d) FERDIG
; The assumptions says that the probability for given that t follows <s> is equal to the probability of t.


;(e)FERDIG
; The Viterbi algorithm computes the tag seguence that has the highest probability given a sequence of oberservations/sentence,
; by dynamically computing and storing the route of tag that output the highest probability. It does so by checking which tag for 
; the next state gives the highest probability and storing the backpointer. The probability is calculated by using the transition
; probability emission probability and the maximum probabilty for the route so far. Each cell representens the state.
; The complexity is O(L^2*N) where L is length of the state set, and N is the length of the input sequence.	
; Another method for finding the most probable tag sequence is using hidden markov model. Which is using the transition probabilty
; and the emission probabilty with n-grams and then basically deciding which possible combinations of tag set that gives the 
; input sequence has the highest probability trough brute-force. Viterbi removes the brute force part and instead finds the sequence
; by following the route that gives the highest probability. 


;4
; (a) FERDIG
; (i) yes
; (ii) yes
; (iii) no


;(b)

;(c) FERDIG
; Yes you can end up infinite loop with the rule NP --> PP NP or VP --> PP VP. No its not suitable, because the cfg need to be in 
; cnf in order to be suitable for the cky parse. Meaning every rule needs to be in either non terminals pointing two non terminals
; or non terminal pointig to a terminal.

;(d) 
; Local ambiguity means that for many substring there is more than one way of deriving the category. In simplified words, it's
; for example when the same sentence have different meanings/ways to understand them. (ii) has a local ambigutiy. The sentence kim
; adores snow in oslo, means both that kim adores the snow which is in oslo, but also that kim is in oslo and generally adores snow.
;  

;(e)
; The difference between active and passive edges is that for passive edges are items that are completed(typically indicated by a
; dot at the end of the RHS of a rule, while active edges are not completed (typically indicated by a dot place so that every rule
; before the dot is observed and confirmed. The active edge holds a "promise" that says that is based on what it has observed it expect
; to see some other rule respectfully.


;5
;(a)
; No, This is not a PCFG because the sum of all the rules proability for a given category say NP or VP, should be 1.0. But its  not.
; The sum of VP is 1.1, V is 0.2 and P is 0.2. The should all be 1.0. 

; (b)
; probability: 0.0001152. 













