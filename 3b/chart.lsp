;;; Hey, emacs, this file is -*- Mode: common-lisp; -*- ... got that?

(in-package :common-lisp-user)

(defparameter *svn-directory*
  (make-pathname :directory "~/lib/teaching/uio.inf4820.2016/public/3b"))

(defstruct grammar
  ;;
  ;; _fix_me_
  ;; fill in the rest of what is needed: a place to store rules and lexemes at
  ;; least, possibly also indices to make finding rules or lexemes easier
  ;;
  )

(defstruct rule
  lhs rhs (probability 1))

(defstruct lexeme
  category (probability 1))

;;
;; a minimum count (i.e. raw frequency) required for inclusion of rules in the
;; grammar; increasing this value will make the grammar smaller and faster to
;; process, maybe at the cost of grammatical coverage of rare constructions.
;;
(defparameter *rule-frequency-threshold* 0)

(defun rules-starting-in (category grammar)
  ;;
  ;; _fix_me_
  ;; return a list containing all grammar rules with `category' as the first
  ;; thing on the right hand side (i.e. the first category after the arrow)
  ;;
  )

(defun get-lexemes (word grammar)
  ;;
  ;; _fix_me_
  ;; return a list of lexemes (from the global grammar) for the given word
  ;;
  )

(defun read-grammar (file)
  ;;
  ;; _fix_me_ 
  ;; this function reads in a treebank file, records the rules and lexemes seen
  ;; and, using Maximum Likelihood Estimation, calculates and stores (in the
  ;; grammar) log probabilities for each rule and lexeme, and finally returns
  ;; the grammar. 
  ;;
  )

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

(defun evaluate (file grammar &key (limit 10))
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
                     (tree (when parse (edge-to-tree (viterbi parse))))
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

(defun parseval (tree gold)
  ;;
  ;; _fix_me_
  ;; ParsEval compares trees by extracting all constituents (identified by start
  ;; and end positions, and category) from each tree and counting the overlap
  ;; (correct) as well as the total constituents in each tree.
  ;;
  )

(defun leaves (tree)
  ;;
  ;; _fix_me_
  ;; extract the leaf nodes (i.e. the surface string) from a tree
  ;;
  )
