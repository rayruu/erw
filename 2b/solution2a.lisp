;;; Solutions to Obligatory Exercise (2a). INF4820; Fall 2016.

;;; First, here are some comments on the choice of data types for the various
;;; slots included in the `vs' structure:
;;;
;;; In this solution we implement the feature vectors as hash tables. This
;;; allows us to only store the non-zero values (which is useful since the
;;; vectors are very sparse). We also use a hash-table for storing
;;; these feature vectors. In other words, our co-occurrence matrix is
;;; a hash-table of hash-tables.

;;;
;;; Examples of usage:
#||
(defparameter vspace 
  (length-normalize-vs
    (read-corpus-to-vs "brown2.txt" "words.txt")))

(euclidean-length (get-feature-vector vspace "congo"))

(print-features vspace "congo" 20)

(word-similarity vspace "africa" "america")
||#




;;;;    1 a)
;;;; ------------
;;;;

;;; We could have defined our context to be a window of, for example,
;;; 2 words before and after the target word. We could also have given
;;; different weights to different words in the context depending on
;;; their distance from the target. Another straightforward variation
;;; would have been to record ngrams in addition to single words,
;;; i.e. sub-sequences of consecutive words. If we had applied deeper
;;; linguistic processing like parsing, we could also have included
;;; features that record the grammatical relations that the target
;;; words hold to other words within the sentences.
;;;
;;; One short-coming in our current approach is also how we identify
;;; words; if we had included some more linguistic analysis for
;;; finding lemmas or stems we might have been able to make better use
;;; of our data (since we would have had fewer unique word types).




;;;;     2 a)
;;;; ------------

(defstruct vs
  (matrix (make-hash-table :test #'equal))
  (similarity-fn #'dot-product)
  ;;The following slot will be used in exercise 2b:
  classes
  proximity-matrix
  (string-map (make-hash-table :test #'equal))
  id-map)


   

;;;;     2 b)
;;;; ------------

(defparameter *stop-list*
   '("a" "about" "also" "an" "and" "any" "are" "as" "at" 
     "be" "been" "but" "by" "can" "could" "do" "for" "from" 
     "had" "has" "have" "he" "her" "him" "his" "how" "i" "if" 
     "in" "is" "it" "its" "la" "may" "most" "new" "no" "not" 
     "of" "on" "or" "she" "some" "such" "than" "that" "the" 
     "their" "them" "there" "these" "they" "this" "those" "to" 
     "was" "we" "were" "what" "when" "where" "which" "who" 
     "will" "with" "would" "you"))

(defparameter *stopwords*
    (let ((hash (make-hash-table :test #'equal)))
      (mapcar #'(lambda (w) (setf (gethash w hash) T))
	      *stop-list*)
      hash))

(defun stop-word-p (w)
  "Predicate that checks whether a word is on the stop-list."
  (gethash w *stopwords*))

(defun normalize-token (string)
 "Text normalization, to be applied to individual tokens."
 (string-downcase 
  (string-trim " ,.!?(){}[]-+@&\";:'*#" string)))

(defun tokenize (string)
  "Splits a sequence into tokens, filters stop-words and normalizes."
  (loop 
      for start = 0 then (+ space 1)
      for space = (position #\space string :start start)
      for token = (normalize-token (subseq string start space))
      unless (or (string= token "") 
		 (stop-word-p token))
      collect token
      until (not space)))

(defun read-corpus-to-vs (corpus wordlist)
  "Returns a vector space model based on the tokens in the corpus."
  (let ((vs (make-vs)))
    ;; read in the list of words to be modeled:
    (with-open-file (stream wordlist :direction :input)
      (loop
	 with matrix = (vs-matrix vs)
	 for word = (read stream nil nil)
	 while word 
	 ;; create a feature vector for each word:
	 do (setf (gethash (normalize-token word) matrix)
		  (make-hash-table :test #'equal))))
    
   ;; record bag-of-words features from the corpus, 
   ;; updating the counts in the feature vectors:
    (with-open-file (stream corpus :direction :input)
      (loop
	 for line = (read-line stream nil nil)
	 while line
	 do
	 ;; nested loop to (1) find target words, and (2) for each
	 ;; target extract features and update its feature vector:
	   (loop
	      with tokens = (tokenize line)
	      for token in tokens
	      for i from 0
	      for feat-vect = (get-feature-vector vs token)
	      when feat-vect ;; target word?
	      do 
		(loop 		      
		   for feature in tokens
		   for j from 0
		   ;;; we don't count a token occurrence as a feature of itself:
		   unless (= i j)
		   do (incf (gethash feature feat-vect 0))))))
    vs))

;;; For discussion on the choice of data structure, see comments at the top of
;;; the file.


;;;;     2 c)
;;;; ------------

(defun get-feature-vector (vs word)
  "Retrieves the feature vector for a given word/string."
  (gethash word (vs-matrix vs)))


;;;;     2 d)
;;;; ------------

(defun print-features (vs word n)
  "Prints a ranked list of n context features for a given word."
  (let ((sorted
	 (loop 
	    with vector = (get-feature-vector vs (normalize-token word))
	    for feat being the hash-keys of vector
	    using (hash-value val)
	    collect (cons val feat) into values
	    finally (return (sort values #'> :key #'first)))))
    ;; print the top n features:
    (loop 
       for i from 1 to n 
       for (val . feat) in sorted
       do (format t "~&~a ~a~%" feat val))))


;;;;     3 a)
;;;; ------------

(defun squared-sum (hash)
  "Computes the squared sum of a feature vector"
  (loop
      for i being the hash-values of hash
      sum (expt i 2)))

(defun euclidean-length (hash)
  "Computes the Euclidean norm of a feature vector."
  (sqrt (squared-sum hash)))


;;;;     3 b)
;;;; ------------

(defun length-normalize-vector (hash)
  "Destructively modifies a vector to have unit length."
  (loop 
      with length = (euclidean-length hash)
      for j being the hash-keys of hash
      using (hash-value n)
      do (setf (gethash j hash)
	   (/ n length)))
  hash)

(defun length-normalize-vs (vs)
  "Normalizes all vectors in a vector space to have unit length."
  (loop
     for vec being the hash-values of (vs-matrix vs)
     do (length-normalize-vector vec))
  vs)


;;;;    3 c)
;;;; ------------

(defun dot-product  (hash1 hash2)
  "Computes the inner product of two feature vectors."
  (loop 
      for id1 being the hash-keys of hash1
      for val2 = (gethash id1 hash2)
      when val2
      sum (* (gethash id1 hash1) val2)))

;; Note how the dot-product function only considers active / non-zero features.



;;;;    3 d)
;;;; ------------

(defun word-similarity (vs w1 w2)
  "Computes the similarity of two word strings in the space."
  (let ((v1 (get-feature-vector vs (normalize-token w1)))
	(v2 (get-feature-vector vs (normalize-token w2))))
    (when (and v1 v2)
      (funcall (vs-similarity-fn vs) v1 v2))))
