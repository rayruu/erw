(in-package :cl-user)

;;;;
;;;; this model solution for Problem Set (3a) in INF4820 (2016) makes use of
;;;; the #+ and #- so-called reader macros, which allow parameterization of
;;;; code at the level seen by the reader, a bit like #if in the preprocessor
;;;; language of C or C++.  for general background, please see:
;;;
;;;;   http://www.lispworks.com/documentation/HyperSpec/Body/02_dhq.htm
;;;;
;;;; by default, the code will compile in non-debuggin mode, i.e. expressions
;;;; prefixed by #+:debug are ignored, and those prefixed with #-:debug used.
;;;; this configuration corresponds to more accurate and more efficient use of
;;;; of log-probabilities.  to enable the debugging version (which will yield
;;;; results as shown in intermediate sample functions calls on the assignment
;;;; sheet), enable the :debug feature as follows:
;;;;
;;;;   (pushnew :debug *features*)
;;;;
;;;; then, re-compile all code and observe how much slower eveything runs.
;;;;

;;;;
;;;; (1) Theory: Hidden Markov Models
;;;;
;;;; (a)
;;;; only two of the tags are actually seen after ‘RB’, viz. ‘,’ (the comma),
;;;; and ‘.’ (the period).  thus P(,|RB) = 2/3; and P(.|RB) = 1/3; 
;;;; all other transition probabilities are zero.
;;;;
;;;; as for emission probabilities, ‘move’ is never seen with state ‘NNP’, thus
;;;; P(move|NNP) = 0; P(move|NN) = 1/1; P(well|RB) = 1/3.
;;;;
;;;; (b)
;;;; smoothing is important to make statistical modeling more robust to what is
;;;; called data sparseness, i.e. the fact of life that no matter the size of
;;;; our training data, many legitimate events may not be seen during training.
;;;; to avoid giving zero probabilities to these events, smoothing shifts some
;;;; of the probability mass from seen to unseen events.  Laplace smoothing is
;;;; arguably the simplest form of smoothing, were actual counts (in training)
;;;; are incremented by a small constant, often 1.  when estimating transition
;;;; probabilities, in Laplace smoothing, the denominator is incremented by the
;;;; size of the ‘vocabulary’, in this case the number of distinct tags: nine.
;;;; thus, P(,|RB) = (2+1)/(3+9) = 1/4; P(.|RB) = 1/6; and for all other tags 
;;;; ‘t’, P(t|RB) = 1/12.  P(POS|NN) = (0+1)/(1+V) = 1/10.
;;;;
;;;; (c)
;;;; the Viterbi algorithm is a dynamic programming algorithm, which means it
;;;; records partial calculations for re-use during an iterative process.  each
;;;; cell in the trellis records the highest probability that the sequence
;;;; could be in the corresponding state, given the prefix of the input up to
;;;; this time point.  since we use Viterbi to find the most probable path, we
;;;; do not need to record every possible path to any state, just the most
;;;; transition for each of the states at the preceding time point.  therefore
;;;; the complexity of the algorithm is O(NTT), i.e. linear in the length of the
;;;; observation sequence and quadratic in the number of states (i.e. the size
;;;; of the tag set).
;;;;



;;;
;;; _fix_me_
;;; adjust to the actual path into your local copy of the INF4820 SVN tree:
;;
;(defparameter *svn-directory*
;  (make-pathname :directory "~"))

;;;
;;; (2a)
;;; in selecting data structures for the parameters of the HMM, we observe that
;;; the state set is comparatively small and fixed (in fact, assumed to be know
;;; when read-corpus() is called); the set of observations (e.g. words of the
;;; language), however, is large and open-ended.  thus, we map states (strings)
;;; to consecutive numeric identifiers and represent the transition matrix as a
;;; two-dimensional array.  for observations, on the other hand, we embed hash
;;; tables (using string-valued observations as keys) in a vector (using state
;;; identifiers as keys).
;;;

(defstruct hmm
  tags
  (n 0)
  transitions
  emissions)

(defun state2id (hmm tag)
  ;;
  ;; look up the numeric identifier for (the string-valued) .tag. and allocate
  ;; a new identifier for previously unseen strings.
  ;;
  (let ((n (position tag (hmm-tags hmm) :test #'string=)))
    (unless n
      (setf (hmm-tags hmm) (nconc (hmm-tags hmm) (list tag)))
      (setf n (hmm-n hmm))
      (incf (hmm-n hmm)))
    n))

(let ((default #+:debug 1/1000000 #-:debug (log 1/1000000)))
  (defun transition-probability (hmm previous current)
    ;;
    ;; give a tiny amount of probability to unseen transitions, building on the
    ;; assumption that the transition matrix has been initialized with .nil.
    ;;
    (or (aref (hmm-transitions hmm) previous current) default))

  (defun emission-probability (hmm state form)
    ;;
    ;; use the same tiny default probability in lieu if actual smoothing
    ;;
    (gethash form (aref (hmm-emissions hmm) state) default)))

(defun read-corpus (file &optional (n 100))
  (with-open-file (stream file :direction :input)
    (loop
        with n = (+ n 2)
        with hmm = (make-hmm)
        with transitions = (make-array (list n n) :initial-element nil)
        with emissions = (make-array n :initial-element nil)
        initially
          ;;
          ;; the make-hmm() constructor does not allocate the transition and
          ;; emission matrices; hence, these are created in the initialization
          ;; of the loop() and deposited in the HMM (that is returned) in the
          ;; finalization.
          ;;
          (loop
              for i from 0 to (- n 1)
              do (setf (aref emissions i) (make-hash-table :test #'equal)))
        ;;
        ;; read one line of input at a time, break it up into the observation
        ;; (surface form) and state (tag), map the state (string) to a numeric
        ;; identifier, and bind a local variable to the emission hash table for
        ;; that very state.  empty lines correspond to sentence breaks, and we
        ;; use the end-of-sentence marker as the state for these (once we have
        ;; seen all tags, we will arrange for state </s> to be at index n - 1).
        ;; count frequencies for all surface forms (on non-empty lines) and
        ;; record state bi-grams, including to the </s> state on empty lines.
        ;;
        for previous = (state2id hmm "<s>") then current
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (subseq line 0 tab)
        for current = (if tab (state2id hmm (subseq line (+ tab 1))) (- n 1))
        for map = (aref emissions current)
        while line
        when (and form (not (string= form ""))) do 
          (incf (gethash form map 0))
        do
          (if (aref transitions previous current)
            (incf (aref transitions previous current))
            (setf (aref transitions previous current) 1))
        ;;          
        ;; as we advance past the end of a sentence (and have incremented the
        ;; transition count into </s>), reset to the start of sentence state.
        ;;          
        when (= current (- n 1)) do (setf current (state2id hmm "<s>"))
        finally
          ;;
          ;; at this point, we have seen n - 1 states, i.e. everything but the
          ;; special end-of-sentence marker; in the loop() above, we have hard-
          ;; coded its numeric state index, so we still need to see to it that
          ;; </s> actually maps to that identifier.
          ;;
          (state2id hmm "</s>")
          (setf (hmm-transitions hmm) transitions)
          (setf (hmm-emissions hmm) emissions)
          (return hmm))))

(defun train-hmm (hmm)
  (loop
      with transitions = (hmm-transitions hmm)
      with n = (hmm-n hmm)
      for i from 0 to (- n 2)
      ;;
      ;; to normalize raw frequencies to relative frequencies (using maximum
      ;; likelihood estimation), count how often the current (.i.) state has
      ;; been seen in the training data, i.e. the sum of transitions from this
      ;; state into any of the other states.
      ;;
      for total 
      = (loop
            for j from 0 to (- n 1)
            sum (or (aref transitions i j) 0))
      do
        ;;
        ;; this is straightforward: transition probabilities P(s_j|s_i)
        ;;
        (loop
            for j from 1 to (- n 1)
            for count = (aref transitions i j)
            when count do 
              (setf (aref transitions i j) 
                #+:debug (/ count total) 
                #-:debug (log (/ count total))))
        ;;
        ;; and, likewise, emission probabilities P(form|s_i)
        ;;
        (loop
            with map = (aref (hmm-emissions hmm) i)
            for form being each hash-key in map
            using (hash-value count)
            when count do 
              (setf (gethash form map) 
                #+:debug (/ count total) 
                #-:debug (log (/ count total)))))
  hmm)

(defparameter *eisner*
  (let ((file (merge-pathnames
               (make-pathname :name "eisner" :type "tt") *svn-directory*)))
    (train-hmm (read-corpus file 2))))

(defparameter *wsj*
  (let ((file (merge-pathnames
               (make-pathname :name "wsj" :type "tt") *svn-directory*)))
    (train-hmm (read-corpus file 45))))

(defun viterbi (hmm input)
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element nil))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;
    ;; the first `row' (at index 0) in our Viterbi matrix corresponds to the
    ;; first observation (whereas the index 0 in our HMM is reserved for the
    ;; start-of-sentence state).  initialize transitions from start state.
    ;;
    (loop
        with form = (first input)
        for state from 1 to (- n 2)
        do
          (setf (aref viterbi state 0)
            (#+:debug * #-:debug +
             (transition-probability hmm 0 state)
             (emission-probability hmm state form)))
          (setf (aref pointer state 0) 0))
    ;;
    ;; the main loop of the Viterbi algorithm: fill in the two matrices in one
    ;; left-to-right (in terms of the observation sequence) iteration
    ;;
    (loop
        for form in (rest input)
        for time from 1 to (- l 1)
        do
          (loop
              for current from 1 to (- n 2)
              do
                (loop
                    for previous from 1 to (- n 2)
                    for old = (aref viterbi current time)
                    for new 
                    = (#+:debug * #-:debug +
                       (aref viterbi previous (- time 1))
                       (transition-probability hmm previous current)
                       (emission-probability hmm current form))
                    ;;  
                    ;; maximize the Viterbi probability of the current state
                    ;; and time point, also maintaining the back-pointers.
                    ;;
                    when (or (null old) (> new old)) do
                      (setf (aref viterbi current time) new)
                      (setf (aref pointer current time) previous))))
    ;;
    ;; finally, fill in the matrix with transitions into the final state
    ;;
    (loop
        for previous from 1 to (- n 2)
        for old = (aref viterbi (- n 1) (- l 1))
        for new = (#+:debug * #-:debug +
                   (aref viterbi previous (- l 1))
                   (transition-probability hmm previous (- n 1)))
        when (or (null old) (> new old)) do
          (setf (aref viterbi (- n 1) (- l 1)) new)
          (setf (aref pointer (- n 1) (- l 1)) previous))
    ;;
    ;; read out the most probably state sequence, working right-to-left this
    ;; time, i.e. starting from the final state.
    ;;
    (loop
        with tags = (hmm-tags hmm)
        with final = (aref pointer (- n 1) (- l 1))
        with result = (list (elt tags final))
        for i from (- l 1) downto 1
        for state = (aref pointer final i) then (aref pointer state i)
        do (push (elt tags state) result)
        finally (return result))))

(viterbi *wsj* '("No" "," "it" "was" "n’t" "Black" "Monday" "."))
;;; => ("UH" "," "PRP" "VBD" "NNP" "NNP" "NNP" ".")

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

#+:null
(let ((file (merge-pathnames
             (make-pathname :name "test" :type "tt") *svn-directory*)))
  (evaluate-hmm *wsj* file))


;;;;
;;;; (4b)
;;;; the Viterbi algorithm involves products of many small probabilities, which
;;;; will lead to very small numbers.  when these numbers are smaller than the
;;;; floating point representation in the implementation environment allows, it
;;;; will cause numeric underflow.  to avoid this, we ‘transpose’ probabilities
;;;; into log space, taking advantage of log(AB) = log(A)+log(B).  working in
;;;; log space not only avoids underflow problems, it can also be faster, as it
;;;; used to be true on common machine architectures for addition to be cheaper
;;;; to compute than multiplication.
;;;;
