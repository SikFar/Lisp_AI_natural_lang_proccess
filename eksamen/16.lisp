;1
;a
;(i)
(print (cons 1 (cons 2 (cons 3 nil))))
;(ii) ((1) (2 3))
(print (cons (cons 1 '()) (cons (cons 2 (cons 3 '()))'())) )



:b
(defun ditch (item lst)
	(if (car lst)
		(if (equal item (car lst))
			(ditch item (cdr lst))
			(cons (car lst) (ditch item (cdr lst)))		
		)
		'()
		
	)
)
(print (ditch 'c '(a b '(c) c d e c)))
(print (ditch 'c '(a b c d e c)))

; I initially used loop to solve this problem, and the base case in a loop can be
; many scenarios, but mine was simple: end of list

; In recursion it is when the current list item is nil or '()

; c
(defun ditch (item lst)
	(loop for x in lst if (not (equal x item)) collect x)
)


(print (ditch 'c '(a b '(c) c d e c)))
(print (ditch 'c '(a b c d e c)))


; (d)
; The common strength is that you can store values and and also in many dimensions, but
; array and list is not as adaptable and easy to work with as hash-maps. Because 
; array and list by nature do not store values with a key-value algorithm. So with
; that in mind hash-maps would be a better choice than list or array. Also the mapping
; will be very sparse, and hash-map handles empty entries automatic compared to list 
; and arrays.

; 2
; (a)
; (i)
; A semantics space is a vector space model where points represent words, dimensions
; represent context of use, and distance in the space represent semantic similarity.

; (ii)
; 1. We can measure the Euclidean length between points of interest. The formula for
; Eculidean length is d(a|,b|) = sqrt(sum i=1->n(ai-bi)^2). Where a and b are vectors
; or in our case feature vectors. You then end up with new vector and find the norm of
; that vector by this formula: sqrt(sum i=1->n xi^2)). The shortcoming for this formula
; is the length bias, and you can solve that by normalizing each vector so that their 
; unit length is equals to 1.
; 2.Calculate the cosine of the vector of interest. You do so by divide the dot-product
; with the length of the vectors. And with unit length as length for the vectors, 
; you save computing time. No shortcomings with this formula.

; (b)
; (i)
; Answer: kNN.
; Reason: The kNN alg does not use centroid, but rather work with the k nearest neighbours of 
; each objects. Meaning an object belongs to the class the majority of its k neares beighbours
; belongs to. This gives us a non-linearly boundaries that seperates the data. The higher k we
; the more likely we are to not overfit. The bigger the datset though, the longer compute time.


; (ii)
; Answer: Rocchio
; Reason: Mainly because the data is seperable by linearly boundaries. In these cases the R alg.
; thrives. The R alg computes centroids based on the average of the objects, and a new object 
; is assigned to the closest centroid. The reason it is not fig 1a
; is because the size of the classes, it is non-linearly seperable, and the centroids would be placed
; so the results would wrong. The R alg. also implicitly assumes that the classes are spheres with
; with similiar radii.

; (c)
; In order to choose the right classifier for a dataset, you first have to determine wether the data is
; spars or dense, distrubuted or "sorted", and maybe also the "shape" of the objects. The answer to these
; parameter will help decide which alg to choose. When it comes to evaluating the result of a classifier,
; you can for example have a test or evaluation data, that is correctly labeled. 


; 3
; (a)
; n-gram; p(w1...wn) = || P(wi|wi-n+1...w1-1) = Count(wi-n+1,..., wi)/Count(wi-n+1,...,wi-1)
; P(never) = 2/32, P(never|use) = 0/3 = undefined, but if you actually mean P(use|never), then
; its P(use|never) = 2/2 = 1. 
; <S> and </S> is there not only to tell when a sentence starts and end, but also because it tells us
; which word is more like to be at beginning of a sentence and which word is more like to be at the end.


; (b)
; By using the chain rule, that goes like P(wN 1) = ||N i=1 P(wi|w i-1 1). 
; Long n-grams eventually ends up giving you the probability of the idividualt single sentences not the
; probability of word or sequence, the data will becomee very sparse. We can assume that we dont need 
; the whole history to find the probability  of a word, but only the least history. We can then find an
; aproximate probabilty which is why its a simplifying assumption.


; (c)
; Its because n-gram models are dependen on the training data, and zero count like this will result
; in an estimated probabilty of 0. Since "I never use the passive" never occurs in the training data.
; The general idea behind smoothing is to elimiminate hard values. Reassign some of the probability mass
; of frequent event to less frequent events.

; (d)
; They can both solve P(O) for a set of observations, but while HMM can also solve P(T,O) for set of
; tags and observations, the language model can not. They bot use the same probabilistic model, but HMM
; gives better and robust results because it also uses the transition probabilites (MLE for the tags)
; together with the emission probabilites (MLE for oberservations). Yes you can, like I said earlier.
; The issue with underflow (solved by using log probabilities) and sparsity/zero valued probabilites
; (solved by using laplace smoothing) is common for both HMM and language model using n-grams.

; (e)
; Its becuause the viterbi alg is an alg where you will recursively re-use the solutions of the
; sub-problems. Such alg is programmed in a dynamic approach. The Viterbi algorithm will calculate the
; max P(S|O) by recursively decide which path from </s> to <s> give the highes probability.
; 


; 4

;(a)
; 6
; (b)
; Local ambiguity: for many substrings, more than one way of deriving the same category.
; Yes the ovals indicate local ambiguity. The CKY shows the different categorys that represent the sub-
; string respectively.

; (c)
; A rule is CNf when its either a non terminal pointing at two non-terminals like so; nt -> nt1,nt2 or
; when a non terminal is pointing at a terminal like so: nt -> t. In a cfg 

: (e)
; You have the passive edge, whih represent a complete item or by instantiated the RHS of the rule. The other
; is active edge, which indicates a incomplete chart item or partially complete rhs rule. It is the passive
; edge we use in the CKY. The other edge (active edge) is a promise. That says, based on the category it has 
; "seen" it will expect to see the correpsponding next category. 


























  
