;; 1 Theory: Hidden Markov Models

;;(a)
;; (i)
;; P(,|RB) = C(RB,,)/C(RB) = 2/3 = 0.666666
;; P(NNP|RB) = C(RB,NNP)/C(RB) = 0/3 = 0
;; P(POS|RB) = C(RB,POS)/C(RB) = 0/3 = 0
;; P(NN|RB) = C(RB,NN)/C(RB) = 0/3 = 0
;; P(VBZ|RB) = C(RB,VBZ)/C(RB) = 0/3 = 0
;; P(VBG|RB) = C(RB,VBG)/C(RB) = 0/3 = 0
;; P(VBN|RB) = C(RB,VBN)/C(RB) = 0/3 = 0
;; P(RB|RB) = C(RB,RB)/C(RB) = 0/3 = 0
;; P(.|RB) = C(RB,.)/C(RB) = 1/3 = 0.3333333

;; (ii)
;; P(wi|ti) = C(ti,wi)/C(ti)
;; P(move|NNP) = C(NNP|move)/C(NNP) = 0/1 = 0
;; P(move|NN) = C(NN,move)/C(NN) = 1/1 = 1
;; P(well|RB) = C(RB,well)/C(RB) = 1/3 = 0.3333333

;; (b)
;; The idea for smoothing is to remove noise (in our case zero-probability)
;; from data set, allowing important patterns to stand out. In the general HMM
;; there are plenty of unobserved data. These words should still have a fairly
;; high emission probability from the P tag. With smoothing we are ensure that
;; zero count items do dont disappear down into zero-probability sinkhole.
;; Laplace smoothing increments all counts by on.
;;
;; PL(POS|NN) = C(POS NN)+1/C(POS) + V = 2/11 = 0.18 

;;(c)
;; The Viterbi algorithm is a dynamic programming algorithm for finding the
;; most likely sequence of hidden states – called the Viterbi path – that
;; results in a sequence of observed events, especially in the context of
;; Markov information sources and hidden Markov models. The trellis is a matrix
;; in which each cell contains the highest probability the a given state is x,
;; knowing that we have seen oberservations. The complexity of this algorithm is
;; O(Tx|S|^2).


;; 2 REPRESENTING OUR TAGGER		KOMMER TILBAKE TIL DENNE
;;    KOMMER TILBAKE TIL DENNE
;; (a)
;; The transitions matrix is simply a hash-table. My reasoning
;; for this is that the assignment text specified the use of numeric ids
;; for states, and I find the final (i.e. computed) matrix unlikely to be
;; sparse. This ensures fast look-ups and (presumably) not a whole lot of
;; redundant space consumption.
;; As for for the data structure for the emission component the approach 
;; I found natural to use was using hash-tables for each state, with
;; words as keys and counts/probabilities as values. Besides, this approach
;; spared me a whole lot of extra work as compared to the 2D array with
;; numeric ids for words too.

(defstruct hmm
	(states (make-hash-table :test #'equal))
	n
	(transitions (make-hash-table :test #'equal))
	(emissions (make-hash-table :test #'equal)))

;; (b)
(defun transition-probability (hmm prev-state next-state)
  (gethash next-state (gethash prev-state (hmm-transitions hmm))))


(defun emission-probability (hmm state word)
  (let ((probability (gethash word (gethash state (hmm-emissions hmm)))))
    (if probability
        probability
        (/ 1 1000000))))


(defun state2id (hmm state-label)

)



