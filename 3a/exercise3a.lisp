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
;; for this is that I feel like hash-tables are easier to work with and
;; also more adaptive. Even though the assignment text specified the use of
;; numeric ids for states, and the final (i.e. computed) matrix will unlikely to
;; be sparse I still decided to work with hash-tables because I have a better
;; understanding of how to work with them in lisp, and another upside with
;; hash-tables is that they ensures fast look-ups and scale very good.
;; As for for the data structure for the emission component the approach
;; I found natural to use was also hash-tables. With words as keys and
;; counts/probabilities as values. Besides, this approach spared me a whole
;; lot of extra work as compared to the 2D array with numeric ids for words too.

(defstruct hmm
	(states (make-hash-table :test #'equal))
	(n 0)
	(transitions (make-hash-table :test #'equal))
	(emissions (make-hash-table :test #'equal))
	(state-count (make-hash-table :test #'equal)))

;; (b)
(defun transition-probability (hmm prev-state next-state)
  (if (gethash next-state (gethash prev-state (hmm-transitions hmm)))
		(gethash next-state (gethash prev-state (hmm-transitions hmm)))
		(/ 1 1000000)
	)

)


(defun emission-probability (hmm state word)
  (let ((probability (gethash word (gethash state (hmm-emissions hmm)))))
    (if probability
        probability
        (/ 1 1000000))))


(defun state2id (hmm state)
	(if (not (equal state nil))
		(let ((states (hmm-states hmm)) (id (hash-table-count (hmm-states hmm))))
			(if (not (gethash state states))
				(progn
					(setf (gethash state states) id)
					(setf (gethash id (hmm-transitions hmm)) (make-hash-table :test #'equal))
					(setf (gethash id (hmm-emissions hmm)) (make-hash-table :test #'equal))
					(setf (gethash id (hmm-state-count hmm)) 0)
					(incf (hmm-n hmm))
					)
				)
		(gethash state states))
	)
)


;; 3 Reading the Training Data

(defun tokenize (string)
	(loop
		for start = 0 then (+ space 1)
		for space = (position #\Tab string :start start)
		for token = (subseq string start space) ;; Here is the change
		collect token
		until (not space)))


(defun collect-and-add-state (hmm token)
	(state2id hmm (cadr token))
	token)



(defun add-transition (hmm state0 state1)
	(let ((prev-id (state2id hmm state0)) (next-id (state2id hmm state1)))
		(incf (gethash next-id (gethash prev-id (hmm-transitions hmm)) 0))))


(defun add-emission (hmm label observation)
  (let ((emissions (gethash (state2id hmm label) (hmm-emissions hmm))))
  	(incf (gethash observation emissions 0))))

(defun increase-state-count (hmm state)
	(incf (gethash (state2id hmm state) (hmm-state-count hmm))))

(defun count-transitions-emissions (hmm sentences)
	(let ((prev-state nil) (next-state nil))
		(loop for sentence in sentences with i = 0 do
		      (let ((observation (car sentence)))

		        ;; For each end line two transitions are added; the first one
		        ;; is (label of line before end line)- (label of end line), and
		        ;; the second one is (label of end line)->"</s>".
		        ;; Under, checking if the line a start line, end line of middle line

		        (cond	((eq i 0) ; The line is a start line.
		            		(progn
				              (setf prev-state "<s>")
				              (setf next-state (cadr sentence))
											(incf i)
											(increase-state-count hmm prev-state)
											(increase-state-count hmm next-state)

											(add-transition hmm prev-state next-state)
											(setf prev-state next-state)
										))
		        			((equal "" observation) ; The line is an end line.
				            (progn
				              (setf next-state "</s>")
				              (add-transition hmm prev-state next-state)
											(increase-state-count hmm next-state)
											(setf i 0)
										))
									(t (progn ; The line is not a start line.
			              (setf next-state (cadr sentence))
										(add-transition hmm prev-state next-state)
										(increase-state-count hmm next-state)
										(setf prev-state next-state))))
						(add-emission hmm prev-state observation)))))


(defun read-corpus (training-file nr-states)
	(let ((hmm (make-hmm)) (lines '()))
		(with-open-file (stream training-file)
			(state2id hmm "<s>")
			(state2id hmm "</s>")
			(loop for line = (read-line stream nil nil)
				while line do collect (collect-and-add-state hmm (tokenize line)) into lines
				finally
					(progn
						(count-transitions-emissions hmm lines))))
	hmm))

;(setf eisner (read-corpus "eisner.tt" 2))
;(print (transition-probability eisner (state2id eisner "<s>") (state2id eisner "H")))
;(print (transition-probability eisner (state2id eisner "C") (state2id eisner "H")))
;(print (transition-probability eisner (state2id eisner "H") (state2id eisner "</s>")))
;(print (emission-probability eisner (state2id eisner "C") "3"))


;(print (gethash (state2id eisner "H") (hmm-state-count eisner)))

(defun compute-transitions-or-emission (count state-id hmm)
	(/ count (gethash state-id (hmm-state-count hmm))))


(defun train-hmm (hmm)
	(loop for first-state being the hash-keys of (hmm-transitions hmm)
				using (hash-value next-states)
				do (loop for next-state being the hash-keys of next-states
        using (hash-value count)
        do (setf (gethash next-state (gethash first-state (hmm-transitions hmm))) (compute-transitions-or-emission count first-state hmm))))

	(loop for state being the hash-keys of (hmm-emissions hmm)
				using (hash-value words)
				do (let ()
					(loop for word being the hash-keys of words
        	using (hash-value count)
        	do (let ()
					(setf (gethash word (gethash state (hmm-emissions hmm))) (compute-transitions-or-emission count state hmm))))))

		hmm)

(defun max-prob (hmm L trellis i s o start-id end-id)
	(let ((probs '()))
		(loop for s-mark from 1 to (1- L)  unless (or (eq s-mark start-id) (eq s-mark end-id)) do
				(setf probs (append probs (list (* (aref trellis (1- i) s-mark) (transition-probability hmm s-mark s) (emission-probability hmm s o))))))
	(reduce #'max probs)))

(defun arg-max(hmm L trellis i s start-id end-id)
	(let ((probs '()) (max 0))
		(loop for s-mark from 1 to (1- L)  unless (or (eq s-mark start-id) (eq s-mark end-id)) do
			(let ((current-prob (* (aref trellis (1- i) s-mark) (transition-probability hmm s-mark s))))
				(if (< max current-prob)
					(setf max s))))
					max))

(defun max-prob-last (hmm L N trellis start-id end-id)
	(let ((probs '()))
		(loop for s from 0 to (1- L)  unless (or (eq s start-id) (eq s end-id)) do
				(setf probs (append probs (list (* (aref trellis (1- N) s) (transition-probability hmm s (state2id hmm "</s>")))))))
	(reduce #'max probs)))

(defun arg-max-last (hmm L N trellis start-id end-id)
	(let ((probs '()) (max 0))
		(loop for s from 0 to (1- L) unless (or (eq s start-id) (eq s end-id)) do
			(let ((current-prob (* (aref trellis (1- N) s) (transition-probability hmm s (state2id hmm "</s>")))))
				(if (< max current-prob)
					(setf max s))))
	max))



(defun viterbi (hmm input-seq)
	(let ((trellis (make-array (list (length input-seq) (hmm-n hmm)) :initial-element 0))
				(backpointer (make-array (list (length input-seq) (hmm-n hmm))))
				(N (length input-seq))
				(L (hmm-n hmm))
				(start-id (state2id hmm "<s>"))
				(end-id (state2id hmm "</s>")))

			(loop for s from 0 to (1- L) unless (or (eq s start-id) (eq s end-id)) do
				(progn
					(setf (aref trellis 0 s) (* (transition-probability hmm start-id s) (emission-probability hmm s (car input-seq))))
					(setf (aref backpointer 0 s) 0)))


			(loop for i from 1 to (1- N) do
				(loop for s from 0 to (1- L) unless (or (eq s start-id) (eq s end-id)) do
					(setf (aref trellis i s) (max-prob hmm L trellis i s (nth i input-seq) start-id end-id))
					(setf (aref backpointer i s) (arg-max hmm L trellis i s start-id end-id))))


			(setf (aref trellis (1- N) end-id) (max-prob-last hmm L N trellis start-id end-id))
			(setf (aref backpointer (1- N) end-id) (arg-max-last hmm L N trellis start-id end-id))


			(setf res (loop for i from 0 to (1- N) do
				collect (let ((interest (loop for s from 0 to (1- L) unless (or (eq s start-id) (eq s end-id)) do
					collect (aref trellis i s)))) (position  (reduce #'max interest) interest))))

			(loop for res-id in res do
				collect (loop for state being the hash-keys of (hmm-states hmm) using (hash-value id) do
					(if (equal id res-id) (return state))))))

;(print (log 5))

;(setf eisner (train-hmm (read-corpus "eisner.tt" 2)))
;(print (viterbi eisner '("1" "1" "3" "3" "3" "3" "1" "1" "1" "1")))
(setf wsj (read-corpus "wsj.tt" 45))
(setf wsj (train-hmm wsj))
;(print (viterbi wsj '("No" "," "it" "was" "n't" "Black" "Monday" ".")))

(in-package :common-lisp-user)

(defun evaluate-hmm (hmm file)
  (with-open-file (stream file :direction :input)
    (loop
        with total = 0 with correct = 0
        with forms with states
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (subseq line 0 tab)
        for state = (and tab (subseq line (+ tab 1)))
        while line
        when (and form state) do
          (push form forms)
          (push state states)
        else do
          (loop
              for gold in (nreverse states)
              for state in (viterbi hmm (nreverse forms))
              do (incf total)
              when (string= gold state) do (incf correct))
          (setf forms nil) (setf states nil)
        finally (return (float (/ correct total))))))


(print (evaluate-hmm wsj "test.tt"))

;;2b)
;; Log probabilities are better because we run the risk of
;; encountering underflow with standard probabilities (i.e extremely small numbers).
;; Underflow happens when a floating point number requires more bits than what is available
;; in the computer's registers, which messes up what the number should
;; really be. To implement this one could either store log probabilities
;; in the transitions/emissions components, or compute the log probabilities
;; when retrieved from said components for further use. If one uses log
;; probabilities one must also remember to _add_ them instead of multiplicating
;; them. Unfortunatley changing the probabilities to logarithmic probabilities did not improve
;; my results. Somewhere in my code something isn't optimal, I just could not find it though.
;; I think the same thing doesn't give me optimal accuracy.
