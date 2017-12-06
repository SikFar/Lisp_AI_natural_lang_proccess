;1)
;a)FERDIG
;P(wi|wi-n+1...wi-1) = C(wi-n+1...wi)/C(wi-n+1...wi)
;bi: P(wi) = C(wi)/C(wi)
;uni: P(wi) = C(wi-2 wi-1 w1)/C(wi-2 wi-1)

;b)FERDIG
; Smoothing is important because when your languge model is based Maximum likelihood estimate, you will get zero based values
; when you try find the probabilty of sentence that have words that hasnt been observed by the model before. So in order to 
; gives this oberservation a fighting chance we apply a smoothing, for example add-one which in the MLE assumes the word
; or sub-string has been observed at least one time. 

;c)FERDIG
;The viterbi algorithm is dynamic algorithm that finds the most probable sequence of hidden states, called the viterbi path,
; for a given oberservation/sentence. Especially used in the contect of markov information and hidden markov models.
; The viterbi trellis is a matrix, in which each cell represent the highest probability for the given state x. The complexity
; of the algorithm is (N*L^2)

;2 FERDIG
;a)
;Eucliedan distance; By measuring the euclidean distance between the feature vectors we can find which the semantic similarity between words.
;The cosine: Instead of measuring the distance we can find the proxmity by measuring the cosine for two vectors we can also find the semantic similary. The result 
;will always be between (0,1). Where 1 is equal.

;normalization: For the euclidean distance adding normalization gives better results, because the frequency in the feature vectors dependes on the corpus,
;which can give "unfair" results. Normalization gives the feature vector a unit length of 1 and therefore every vector a fighting chance.
;For the cosine solution, converting the feature vector to have normalized units, turns the cosine to the dot product.  

;c) FERDIG
;The decision boundaries are boundaries that decides which class an object belongs to depending on the algorithm and which side of the boundarie the objects is.

;(d)FERDIG
;The rocchio classifier calculates and uses centroids to represent classes, and its decision boundaries are equidistant from the centroid, meaning boundaries are
;linear. So Rocchio classifier performs best when the objects in the class arent disconnected and in ideal shape(oval in 2d, sphere in  3d). Antoher downside with 
;rocchio is that it does not take into consideratio the traning data, when assigning the new object a class. The kNN classifier unlike Rocchio do not work with 
;centroids, but rather the the objects itself. The algorithm assign new objects to class the majority of its neighbours belong to. You can also change the algorithm 
;to be percentage based, soft class, rather than specific values, hard based. Having to low k, will lead to overfitting and having to high k will lead to underfitting.
;The decision boundaries are based on voronoi tessellation


;3
;b)FERDIG
;Yes it does, those are VP -> VP PP and NP -> NP PP. The problem with these rules is that the top-down parser can end up in a infinite loop because the keep referring 
;to themselves. It prevents, because CKY works only with grammar that are in CNF. And an example of this grammar not being in CNF is VP -> V. In order to be in CNF
;each rule has to be either a non terminal pointing at two non terminal, or a nonterminal pointing at a terminal. VP->V fails to follow that rule.

;c)FERDIG
;The role of the CKY is to address and map the local ambiguity in a sentence/oberservation. Meaning, with CKY you can get the category derivation(s) that gives a
;string og sub-string. In generalised chart parsing, the difference between active and passive edge is that active edge tells us that the derivation is not yet
;complete. An edge is a rule instantiation over a substring of input. This is indicated by the dot on the RHS of the rule in the edge, where the dot indicates degree
;of completion. The fundemental rule says that two edges are adjecent when the passive edge/the second edge starts where the active/first edge ends.

;d)
;It because a PCFG can create many trees for a sentence/input/oberservation, and we want find the tree that gives the maximum probability for that given sentence.
;The tree shows the grammatical rules that builds the sentence.

;4
;a)
;High order functiona are functions that take other functions as arguments or return functions

;b)
(defun flat (lst)
	(print lst)
	(cond 
		((not lst)
		'())
		((listp (car lst))
		(append (flat (car lst)) (flat (cdr lst))))
		((not (listp (car lst))) (cons (car lst) (flat (cdr lst))))
	)
)

;(setq foo '(a b (c) (((d) (e)) (f (g (h))) i)))
;(print (flat foo))




(setq foo 42)
(defun foo (x)
#'(lambda (y)
(if (>= y x)
y
x)))

(let ((bar (foo foo))
(foo (* foo 2)))
(print(funcall bar foo)))

; 
