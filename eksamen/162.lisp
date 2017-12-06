;1
;a)
;i)
(print (cons 1 (cons 2 (cons 3 '()))))
;ii)
(print (cons (cons 1 '()) (cons (cons 2 (cons 3 '()))'()) ) )

;b)
(defun ditch ()
	
)

;d)FERDIG
;With lists you have what is called association lists, where the elements in the list are pairs of key and values. Unfortunately list dont work well, with large
;datasets. Also they are not easy to scale. Array is indexbased storing of information, not keybased, and even though the size of the array can be altered, it has to 
;be defined when being initialize. Which is information you dont have when working with matrixes like emission in HMM. Hash map is the best choice because, the are
;key-value based storage of inormation, the scale up and down automatic. And is also very good with sparseness. Which in emission matrix there will be alot sparsety, 
;becaused of all the zero based values we get from our definition context.

;2
;a)
;i)

;ii)NESTEN FERDIG, SKRIV FORMELENE
;Semantic similary can be meausered either with Eucilidean distance between two vectors(spatial distance), or the cosine between them. The euclidean distance is 
;calculated based on frequency, and depending on the corpus, it does not always qualify to represent word similiraty. To solve this, we convert the values in the
;feature vector so that the unit vector is 1. When working with cosine we are intereset in the angle between the two vectors. The value we get back will be between [0,1], where 1 is equals. Because of the formula for cos, we can simplyit by converting the vectors to have the unit length of 1. 

;b)
;i)
;kNN: Essentually beause i can see that the descision boundaries will be non-linear, which is the case for kNN. kNN assigns new objects to the class the majority of
;its neighbours belong to. Knn can also have soft class, meaning the decision is based on percentage of the neighbours rather than majority. 
;ii)
;Rocchio; Essentually for the same reason, but in this case the decision boundaries will be linear. The Rocchio is centroid based, so instead of assigning the class
;to the closest neighbour, assign to the closest cetroid.

;c)
;We can evaluate binary classes using by the precision, recall and accuracy, which we can find by using using the values false positive(pt), true positie(tp), false 
;negative(fn) and true negative(tn). For multiple classes we can use macro-averaging and micro averaging. Macro-averageing is to find the average of recall,
;accuracy and precision for all the classes. Micros is finding the average of PT, TP, FN and TN and finding recall, precision and accuracy from that. objects that are
;well distributed make ut hard to find the TP, FN FP and TN. so fig a, would be difficult.


;3
;a)FErDIG
;p(wi|wi-n+1...w-1) = C(wi-n+1...wi)/C(wi-n+1...wi-1)
;P(never) = 2/32 
;p(never/use) = 0/3
;We use them to both mark the end and beginning of a sentence, but also because we might be interested in the probability of a word being the start word and end word.


;b)NESTE FERDIG
;By using chain rule from basic probabilities theroy: P(s) = || P()
;Because then we basically find the probabilties of a word being a part of string(s), while we're actually interesed in the probability of the words that build a
;certain string. That we can find the probabilty of a sentence if we look at probabilities of recent history.

;c)FERDIG
;Because the string or sub-strings of it is not in our corpus/training model and we have not applied smoothing. So the probebilty of the sentence will be 0
;The idea behind smoothing is that we want to give every sentence a fighting chance. Because the training model is based on the corpus and maximum likelihood estimate
;it will fail for input observations is has not trained on and return the possibility of 0.

;d)FERDIG
;The commonalaties is that they both use n-gram MLE probabilities to derive the probabilitie for a sentence, and they both need smoothing to be applied. The difference
;is that the language model uses words or substring to find the probability for a sentence, while HMM uses the hidden tags as well, called transision model. Yes it is
 
;e)NESTEN FERDIG
;It does so by solving one sub-problem at a time, and using the result from last sub-problem to solve next sub-problem. The Viterbi algorithm will find the 
;sequence of hidden tags that gives the highes probability for a given inpute sentence.

;4
;a)6

;b)
;For many substring, there can be many categories to derive that substring. Yes the ovals. CKY parse can identfy and plot which categories/grammatical rules
;creates a given string og substring

;c)FERDIG
;The rules is one of two forms, either a non terminal pointing at two non terminals, like nt -> nt1 nt2 or a non terminal pointing at a terminal, like nt -> t

;d)FERDIG

;e)
;The passive edge indiciates a complete item on the chart, with a dot at end of the RHS of the rule. While an active edge tells us which item is incomplete. This is 
;shown with to placed so that every category on LFH of the dot is observered, and every category on the RHS is expected to be found. The passive edge, since its 
;complete and holds no promises. The active edges in the generalized parser tells us, based on the passive edge which edge we can expect and look for, but also which
;rule builds this sub-string.








