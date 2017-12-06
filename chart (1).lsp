;;; Hey, emacs, this file is -*- Mode: common-lisp; -*- ... got that?
;; 1 Theory: PCFG Maximum Likelihood Estimation
;; S --> NP VP        2       C(S --> NP VP )/C(S) = 2/2 = 1.0
;; VP --> VBD NP PP   1       C(VP --> VBD NP PP)/C(VP) = 1/2 = 0.5
;; VP --> VBD NP NP   1       C(VP --> VBD NP NP)/C(VP) = 1/2 = 0.5
;; NP --> NNP         4       C(NP --> NNP)/C(NP) = 4/6 = 0.6666666667
;; NP --> DT NN       2       C(NP --> DT NN)/C(NP) = 2/6 = 0.33333337
;; PP --> P NP        1       C(PP --> P NP)/C(PP) = 1/1 = 1
;; NNP --> FRODO      2       C(NNP --> FRODO)/C(NNP) = 2/4 = 0.5
;; NNP --> SAM        2       C(NNP --> SAM)/C(NPP) = 2/4 = 0.5
;; DT --> THE         2       C(DT --> THE)/C(NPP) = 2/2 = 1
;; NN --> RING        2       C(NN --> RING)/C(NN) = 2/2 = 1
;; P --> TO           1       C(P --> TO)/C(P) = 1/1 = 1
;; VBD --> SENT       2       C(VBD --> SENT)/C(VBD) = 2/2 = 1


;; 2 Training a PCFG from Treebank Data

(in-package :common-lisp-user)

;(defparameter *svn-directory*
;  (make-pathname :directory "~/lib/teaching/uio.inf4820.2017/public/3b"))

(defstruct grammar
  (rules (make-hash-table :test #'equal))
  (lexeme (make-hash-table :test #'equal))
  (cat-count (make-hash-table :test #'equal))
  ;;
  ;; fill in the rest of what is needed: a place to store rules and lexemes at
  ;; least, possibly also indices to make finding rules or lexemes easier
  ;;
  )

(defstruct rule
  lhs
  rhs
  (probability 1))

(defstruct lexeme
  category
  (probability 1))

;;
;; a minimum count (i.e. raw frequency) required for inclusion of rules in the
;; grammar; increasing this value will make the grammar smaller and faster to
;; process, maybe at the cost of grammatical coverage of rare constructions.
;;
(defparameter *rule-frequency-threshold* 0)

(defun rules-starting-in (category grammar)
  ;;
  ;; return a list containing all grammar rules with `category' as the first
  ;; thing on the right hand side (i.e. the first category after the arrow)
  ;;
    (let ((every-rule (grammar-rules grammar)) (rules '()))
      (loop for rhs being the hash-keys of every-rule using (hash-value rule) do
        (let ()
          (if (equal category (car rhs))
            (setf rules (append rules (list rule))))))
    rules))

(defun get-lexemes (word grammar)
  ;;
  ;; return a list of lexemes (from the global grammar) for the given word
  ;;
  (let ((every-lexeme (grammar-lexeme grammar)) (lexemes '()))
    (loop for w being the hash-keys of every-lexeme using (hash-value lexeme) do
      (let ()
        (if (equal (car w) word)
          (setf lexemes (append lexemes (list lexeme))))))
  lexemes))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun add-lexeme-to-grammar (grammar rule)
    (if (not (gethash (cdr rule) (grammar-lexeme grammar)))
      (setf (gethash (cdr rule) (grammar-lexeme grammar)) (make-hash-table :test #'equal)))
    (if (not (gethash (car rule) (grammar-cat-count grammar)))
      (setf (gethash (car rule) (grammar-cat-count grammar)) 1)
      (incf (gethash (car rule) (grammar-cat-count grammar))))
    (let ((lexeme (gethash (car rule) (gethash (cdr rule) (grammar-lexeme grammar)))))
      (if lexeme
        (setf (lexeme-probability lexeme)
              (+ 1 (lexeme-probability lexeme)))
        (progn
          (setf lexeme
                (make-lexeme))
          (setf (lexeme-category lexeme)
                (car rule))))
      (setf (gethash (car rule) (gethash (cdr rule) (grammar-lexeme grammar))) lexeme))
  (list (car rule))

)

(defun process-lexeme (grammar rule)
  (let ((rules '()))
    (cond
      ((not rule) '())
      ((equal 'symbol (type-of (car rule)))
          (append (list (car rule)) (process-lexeme grammar (cdr rule))))
      ((some #'listp (car rule))
        (list (process-lexeme grammar (car rule)) (process-lexeme grammar (cdr rule))))
      ((and (listp (car rule)) (= 2 (length (car rule))))
        (append (list (add-lexeme-to-grammar grammar (car rule))) (process-lexeme grammar (cdr rule)))))))

(defun add-rule-to-grammar (grammar rule)
  (setf rule (reverse rule))
  (if (not (gethash (flatten (cdr rule)) (grammar-rules grammar)))
    (setf (gethash (flatten (cdr rule)) (grammar-rules grammar)) (make-hash-table :test #'equal)))
  (if (not (gethash (car rule) (grammar-cat-count grammar)))
    (setf (gethash (car rule) (grammar-cat-count grammar)) 1)
    (incf (gethash (car rule) (grammar-cat-count grammar))))

  (let ((rules (gethash (car rule) (gethash (flatten (cdr rule)) (grammar-rules grammar)))))
    (if rules
      (setf (rule-probability rules)
            (+ 1 (rule-probability rules)))
      (progn
        (setf rules
              (make-rule))
        (setf (rule-lhs rules);; SET LHS
              (car rule))
        (setf (rule-rhs rules);; SET RHS
              (flatten (cdr rule)))))
    (setf (gethash (car rule) (gethash (flatten (cdr rule)) (grammar-rules grammar))) rules))
  (car rule))

(defun process-rule (grammar rule)
  (setf rule (remove nil rule))
  (cond
    ((not rule) '())
    ((and (listp (car rule)) (listp (cadr rule)) (not (some #'listp (car rule))) (some #'listp (cadr rule)))
        (process-rule grammar
          (append
            (list (car rule))
            (append
              (list (list (add-rule-to-grammar grammar (cadr rule))))
              (cddr rule)))))
    ((and (cdr rule) (listp (car rule))(not (some #'listp (car rule))))
      (list (add-rule-to-grammar grammar rule)))
    ((and (listp (car rule)) (some #'listp (car rule)))
        (process-rule grammar (append (list (process-rule grammar (car rule))) (cdr rule))))
    (t (car rule))))

(defun cal-probs (grammar)
  (let ((rules (grammar-rules grammar))
        (lexemes (grammar-lexeme grammar)))

    (loop for key being the hash-keys of rules using (hash-value value) do
      (loop for category being the hash-keys of value using (hash-value rule) do
        (let ()
          (setf (rule-probability rule) (/ (rule-probability rule) (gethash category (grammar-cat-count grammar))))
          (if (= 0 (log (rule-probability rule)))
            (setf (rule-probability rule) (/ 1 100000))
            (setf (rule-probability rule) (log (rule-probability rule)))))))
    (loop for word being the hash-keys of lexemes using (hash-value value) do
      (loop for category being the hash-keys of value using (hash-value lexeme) do
        (let ()
          (setf (lexeme-probability lexeme) (/ (lexeme-probability lexeme) (gethash category (grammar-cat-count grammar))))
          (if (= 0 (log (lexeme-probability lexeme)))
            (setf (lexeme-probability lexeme) (/ 1 100000))
            (setf (lexeme-probability lexeme) (log (lexeme-probability lexeme)))))))))

(defun process-treebank (grammar treebank)
  (let ((start '()))
    (loop for rule in treebank do
      (if (not (listp rule))
        (progn
          (process-rule grammar (list (list rule) 'START))
          (setf start (append start (list rule))))
        (setf start (append start (list (process-rule grammar (reverse (process-lexeme grammar rule))))))))
    ;; Below, This is where I handle the start rules. i.e S --> NP VP
    (process-rule grammar (reverse start))))


(defun read-grammar (treebank-file)
  ;;
  ;; this function reads in a treebank file, records the rules and lexemes seen
  ;; and, using Maximum Likelihood Estimation, calculates and stores (in the
  ;; grammar) log probabilities for each rule and lexeme, and finally returns
  ;; the grammar.
  ;;
    (let ((grammar (make-grammar)))
      (with-open-file (stream treebank-file)
        (loop for line = (read stream nil nil)
          while line do (process-treebank grammar line)))
    (cal-probs grammar)
    grammar)
  )


(setf toy (read-grammar "toy.mrg"))
(print (get-lexemes "flies" toy))
;(print (rules-starting-in 'NP toy))
;(setf wsj (read-grammar "wsj.mrg"))
;(print  (get-lexemes "flies" wsj))
;(print (length (rules-starting-in 'NP wsj)))


;;;
;;; from here onwards, we provide most of the code (and generous comments),
;;; only requiring you to complete one function: fundamental-rule().  read
;;; through the rest of the code and make sure you understand how it implements
;;; the generalized chart parser we discussed in the lectures.
;;;

;;;
;;; the parse chart we use is a two-dimensional array indexed by string
;;; positions.  we use the second dimension to indicate whether we are indexing
;;; by start or end positions, and whether the edge is passive or active i.e.:
;;;
;;;   chart[i,0] is for passive edges starting at i,
;;;   chart[i,1] is for passive edges ending at i,
;;;   chart[i,2] is for active edges starting at i; and
;;;   chart[i,3] is for active edges ending at i
;;;

(defun chart-cell (from to chart &optional activep)
  ;;
  ;; given a start and end vertex (i.e. sub-string .from. and .to. indices),
  ;; retrieve the relevant chart edges (defaulting to passive edges only)
  ;;
  (loop
      for edge in (append
                   (aref chart from 0) (and activep (aref chart from 2)))
      when (= (edge-to edge) to) collect edge))

(defun passive-edges-from (index chart)
  ;;
  ;; for a given chart vertex (aka string from position), retrieve all the
  ;; passive edges from the chart that start at that vertex
  ;;
  (aref chart index 0))

(defun active-edges-to (index chart)
  ;;
  ;; for a given chart vertex (aka string to position), retrieve all the
  ;; active edges from the chart that end at that vertex
  ;;
  (aref chart index 3))

(defun chart-adjoin (edge chart)
  ;;
  ;; given the way we have organized our chart, inserting a new edge requires
  ;; adding it by both its from and to positions in two `rows' of our
  ;; chart implementation.
  ;;
  (let ((offset (if (passive-edge-p edge) 0 2)))
    (push edge (aref chart (edge-from edge) (+ offset 0)))
    (push edge (aref chart (edge-to edge) (+ offset 1)))))

(defstruct edge
  ;;
  ;; edges record their span and category, the daughters they have seen (in the
  ;; .daughters. slots) and the daughters they still require (.unanalyzed.).
  ;; the .alternates. slot holds other edges with the same span and category.
  ;; during forest construction, .probability. holds the (log) probability of
  ;; the associated rule.  The Viterbi function updates this to be the maximum
  ;; probability of the subtree represented by this edge.  the .cache. slot is
  ;; used in viterbi() to avoid recalculations.
  ;;
  from to category
  daughters unanalyzed
  alternates
  probability
  cache)

(defun edge-to-tree (edge)
  ;;
  ;; expands .edge. to a tree, recursing over daughters (but not alternates)
  ;;
  (if (edge-daughters edge)
    (cons (edge-category edge)
          (loop
              for daughter in (edge-daughters edge)
              collect (edge-to-tree daughter)))
    (edge-category edge)))

(defun passive-edge-p (edge)
  ;;
  ;; passive edges have seen all their daughters
  ;;
  (null (edge-unanalyzed edge)))

(defstruct agenda
  ;;
  ;; our agenda, for this exercise, is just a simple stack, but that could be
  ;; changed to implement another agenda strategy
  ;;
  contents
  popped)

(defun agenda-push (edge agenda)
  (push edge (agenda-contents agenda)))

(defun agenda-pop (agenda)
  (setf (agenda-popped agenda) (pop (agenda-contents agenda))))

(defun parse (input grammar)
  ;;
  ;; finally, our implementation of the the generalized chart parser
  ;;
  (let* ((agenda (make-agenda))
         (n (length input))
         (chart (make-array (list (+ n 1) 4) :initial-element nil)))

    ;;
    ;; create a `lexical' edge (one without daughters that is passive from the
    ;; start) for each word of the input sequence.  then add passive edges for
    ;; each possible word category to the  agenda.
    ;;
    (loop
        for i from 0
        for word in input
        for lexemes = (get-lexemes word grammar)
        for daughters = (list (make-edge :from i :to (+ i 1) :category word
                                         :probability 0.0))
        do
          ;;
          ;; if we have not seen all the words in training, fail immediately;
          ;; no point waisting time in filling a chart that can never complete.
          ;;
          (if (null lexemes)
            (return-from parse nil)
            (loop
                for lexeme in (get-lexemes word grammar)
                for edge = (make-edge :from i :to (+ i 1)
                                      :category (lexeme-category lexeme)
                                      :daughters daughters
                                      :probability (lexeme-probability lexeme))
                do (agenda-push edge agenda))))

    ;;
    ;; the main parser loop: explore all possible edge combintions
    ;;
    (loop
        for edge = (agenda-pop agenda)
        while edge do
          (cond
           ((passive-edge-p edge)
            ;;
            ;; for passive edges, we first try and pack into an existing edge
            ;; in the chart.  if there are no equivalent edges in the chart
            ;; yet, add this .edge., apply the fundamental rule, then predict
            ;; new edges and add them to the agenda also.
            ;;
            (unless (pack-edge edge chart)
              (chart-adjoin edge chart)
              (loop
                  for active in (active-edges-to (edge-from edge) chart)
                  do (fundamental-rule active edge agenda))
              (loop
                  with from = (edge-from edge) with to = (edge-to edge)
                  for rule in (rules-starting-in (edge-category edge) grammar)
                  for new = (make-edge :from from :to to
                                       :category (rule-lhs rule)
                                       :daughters (list edge)
                                       :unanalyzed (rest (rule-rhs rule))
                                       :probability (rule-probability rule))
                  do (agenda-push new agenda))))
           (t
            ;;
            ;; we do not attempt ambiguity packing on active edges, but instead
            ;; just add the edge to the chart and apply the fundamental rule.
            ;;
            (chart-adjoin edge chart)
            (loop
                for passive in (passive-edges-from (edge-to edge) chart)
                do (fundamental-rule edge passive agenda)))))

    ;;
    ;; the agenda is now empty, check for a passive edge that spans the input
    ;; and has a category equal to our start symbol.  seeing as there is only
    ;; one start symbol, and given the assumptions we make about equivalence
    ;; within each chart cell, there can be at most one such edge.
    ;;
    (loop
        for edge in (chart-cell 0 (length input) chart)
        when (eq (edge-category edge) (grammar-start grammar))
        return edge)))

(defun fundamental-rule (active passive agenda)
  ;;
  ;; the fundamental rule of chart parsing: given one active and one passive
  ;; edge (known to be adjacent already), check for compatibility of the two
  ;; edges and add a new edge to the agenda when successful.
  ;;
  (when (equal (edge-category passive) (first (edge-unanalyzed active)))
    (agenda-push
     (make-edge :from (edge-from active) :to (edge-to passive)
                :category (edge-category active)
                :daughters (append (edge-daughters active) (list passive))
                :unanalyzed (rest (edge-unanalyzed active))
                :probability (edge-probability active)) agenda)) )

(defun viterbi (edge)
  ;;
  ;; a recursive implementation of the Viterbi algorithm over packed forests
  ;;

  (or (edge-cache edge)
      (setf (edge-cache edge)
        (if (edge-daughters edge)
          (loop
              initially
                (setf (edge-probability edge)
                  (+ (edge-probability edge)
                     (loop
                         for daughter in (edge-daughters edge)
                         sum (edge-probability (viterbi daughter)))))
              for alternate in (edge-alternates edge)
              for probability = (edge-probability (viterbi alternate))
              when (> probability (edge-probability edge))
              do
                (setf (edge-probability edge) probability)
                (setf (edge-daughters edge) (edge-daughters alternate))
              finally (return edge))
          edge))))

(defun pack-edge (edge chart)
  ;;
  ;; if there is more than one way to derive a particular category for a
  ;; particular span, pack all alternatives into the first such edge we found.
  ;;
  (when (passive-edge-p edge)
    (loop
        ;;
        ;; look for a passive edge with the same span and category; there can
        ;; be at most one.
        ;;
        for host in (passive-edges-from (edge-from edge) chart)
        when (and (= (edge-to host) (edge-to edge))
                  (equal (edge-category host) (edge-category edge)))
        do
          ;;
          ;; if we found an equivalent edge in the chart, add the new .edge.
          ;; to our host, unless that would create a cycle, in which case,
          ;; discard our new edge.  return the `host', indicating no more
          ;; processing is necessary on this edge.
          ;;
          (unless (daughterp host edge)
            (push edge (edge-alternates host)))
          (return host))))

(defun daughterp (host edge)
  ;;
  ;; test whether .host. is (transitively) embedded as a daughter below .edge.,
  ;; to avoid creating cycles in the packed forest.
  ;;
  (loop
      for daughter in (edge-daughters edge)
      thereis (or (eq daughter host) (daughterp host daughter))))

(defun evaluate (file grammar &key &baseline (limit 10))
  ;;
  ;; read a test file, extracting gold trees and using their leaves as input
  ;; to our parser, for any sentence <= .limit. (for efficiency).  then compute
  ;; ParsEval scores to compare between the tree from the parser and the gold
  ;; tree, after first stripping our dummy start node
  ;;
  (with-open-file (stream file)
    (loop
        with inputs = 0 with analyses = 0
        with tcorrect = 0 with tfound = 0 with tgold = 0
        for gold = (read stream nil nil)
        while gold do
          (let* ((leaves (leaves gold))
                 (n (length leaves)))
            (when (<= n limit)
              (incf inputs)
              (let* ((start (get-internal-run-time))
                     (parse (parse leaves grammar))
                     (end (get-internal-run-time))
                     (tree (when parse (edge-to-tree (if baseline parse (viterbi parse)))))
                     ;;
                     ;; discard the top-level node, which is the start symbol
                     ;;
                     (tree (when (consp tree) (first (rest tree)))))
                (multiple-value-bind (correct found gold) (parseval tree gold)
                  (format
                   t "~a. [~a] |~{~a~^ ~}| (~,2fs) P=~,2f R=~,2f~%"
                   inputs n leaves
                   (/ (- end start) internal-time-units-per-second)
                   (if (zerop found) 0 (/ correct found)) (/ correct gold))
                  (when parse
                    (incf analyses)
                    (incf tcorrect correct)
                    (incf tfound found))
                  (incf tgold gold)))))
        finally
          (let* ((precision (if (zerop tfound) 1 (/ tcorrect tfound)))
                 (recall (/ tcorrect tgold))
                 (fscore (/ (* 2 precision recall) (+ precision recall))))
            (format
             t "== ~a input~p; ~,2f% coverage; P=~,2f R=~,2f F1=~,2f~%"
             inputs inputs (/ analyses inputs) precision recall fscore)
            (return (float fscore))))))

;; 3 Parser Evaluation
;; (a)
(defun leaves (tree)
  (if (consp tree)
      (loop for node in (rest tree)
            append (leaves node))
      (list tree)))

;; (b)
(defun get-pos (item in-lst)
  (position item in-lst :test #'equal)
)

(defun decompose-tree (tree originial-leaves)
 (let ( (first-word-index (get-pos (car (leaves tree)) originial-leaves))
        (last-word-index (get-pos (car (last (leaves tree))) originial-leaves))
        (bracket '()))
    (if (and tree (some #'listp tree))
      (cond
        ((and (not (stringp (car tree))) (not (listp (car tree))))
          (progn ;(print "kommer hit1")(print tree)
          (append (setf bracket (append bracket (list (list (car tree) first-word-index last-word-index)))) (decompose-tree (cdr tree) originial-leaves))))
        ( (or (not (stringp (car tree))) (some #'listp tree))
        (progn ;(print "kommer hit2")(print tree)
          (append (decompose-tree (car tree) originial-leaves) (decompose-tree (cdr tree) originial-leaves))))
      )
      '()
    )
 )
)


(defun parseval (input-tree gold-tree)
  ;;Making sure input-tree and gold-tree is not empty by using the "and" function
  (let* ((input-tree (and input-tree (decompose-tree input-tree (leaves input-tree))))
         (gold-tree (and gold-tree (decompose-tree gold-tree (leaves gold-tree))))
         (correct-brackets (intersection input-tree gold-tree :test #'equal)))
        (print input-tree)
        (print gold-tree)
     ;(values (length correct) (length input-tree) (length gold-tree)))
     ;values did not work for me for some reason, it would only return the first value of values
     ;It might be because of the clisp compoler
    (list (length correct-brackets) (length input-tree) (length gold-tree))
  )
)

;;

(print (parseval '(START
       (S (NP (NNP "Frodo"))
        (VP (VBZ "eats")
         (NP (NP (NN "wasabi")) (PP (P "with") (NP (NNS "chopsticks")))))))
     '(START
       (S (NP (NNP "Frodo"))
        (VP (VP (VBZ "eats") (NP (NN "wasabi")))
         (PP (P "with") (NP (NNS "chopsticks"))))))))

;; input-tree
;; ((START 0 4) (S 0 4) (NP 0 0) (NNP 0 0) (VP 1 4) (VBZ 1 1) (NP 2 4) (NP 2 2) (NN 2 2) (PP 3 4) (P 3 3) (NP 4 4) (NNS 4 4))
;; gold-tree
;; ((START 0 4) (S 0 4) (NP 0 0) (NNP 0 0) (VP 1 4) (VP 1 2) (VBZ 1 1) (NP 2 2) (NN 2 2) (PP 3 4) (P 3 3) (NP 4 4) (NNS 4 4))

;; One can see that "(NP 2 4)" from input-tree is not suppose to be there, but
;; instead be "(VP 1 2)" and switch place with "(VBZ 1 1)". "(VP 1 2)" is the missing bracket.

;; (c)
;; By contributing with HMM (hidden markov model) we can avoid returning empty
;; trees because of the parsing a sentence with unknown words. By using HMM the
;; probability of successfully parsing a sentence trough a grammar increases,
;; since HMM finds the right category for the unknown word.

;; (d)
;; See the evaluate function

;; 4 Generalized Chart Parsing and Viterbi Decoding
;; (a)
;; For passive edges, we first try and pack into an existing edge
;; in the chart. If there are no equivalent edges in the chart
;; yet, add this .edge., apply the fundamental rule, then predict
;; new edges and add them to the agenda also. Given one active and one passive
;; edge (known to be adjacent already), check for compatibility of the two
;; edges and add a new edge to the agenda when successful.
;; The properties of the new edge is it's location, the words category,
;; all its daughters, and the lexeme-probality found by using Viterbi.

;; (b)
;; In the beginning we check wether this edge already has calculated its
;; probability, becuse the edge-alternates holds other edges that is very similar
;; the same as the current. And uses that trait to make calculations more
;; effective. The function returns the current edge if current-edge doesn't have
;; any daughters, because daughters are necessary to get full worthy probabilities.
;; So it goes trough every daughter and find it's viterbi, and find the sum of
;; all the viterbis. And then we go trough the probability of the
;; alternate-edges, and updates the current-edge probability if its bigger than
;; the probability that has been calculated from daughters. At the end it
;; return the edge with a probability.
