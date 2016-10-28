;;; IMPORTANT; The code in this file builds on the functions from the provided
;;; solution to exercise 2a. In other words we assume you have already loaded
;;; the definitions given in `solution2a.lsp'.

;;; Initializing the vector space:
#||
(defparameter vspace 
  (length-normalize-vs
    (read-corpus-to-vs "brown2.txt" "words.txt")))
||#




;; 1 A) Background:
;;
;; A sensible solution here could be to represent the proximity matrix
;; as a hash-table of hash-tables (or some other associative look-up
;; structure, like a-lists). One could then impose some canonical
;; ordering of the keys (i.e. words) for example using
;; `string>'. Another possibility would be to only use a single
;; hash-table and let the keys be a list of the two ordered words
;; (making sure to specify `#'equal' as the test in
;; `make-hash-table').
;;
;;
;; In this sample solution however, we instead implement an abstract
;; data type for symmetric matrices, taking the opportunity to
;; showcase some more advanced Common Lips techniques (like the use of
;; `defsetf'). Note that this solution goes quite a few steps beyond
;; what is expected from your own submissions (and is also much longer
;; than the straightforward hash-table solution outlined above).
;;
;; Based on a linear array, `symat' implements a maximally compact
;; representation of symmetrical matrices. The implementation takes
;; advantage of the fact that a given two-dimensional array can
;; actually be "linearized" as a one-dimensional array, usually
;; referred to as a "row-major order" representation.

(defstruct (symat
	    ;; internal constructor used by make-symat:
	    (:constructor internal-make-symat (dimensions table)))
  dimensions
  table)

;; Redefine the default make-* constructor: 
(defun make-symat (dim)
  (internal-make-symat dim (make-array (/ (* dim (1+ dim)) 2))))

(defun symat-ref (sm i j)
  "Accessor function for the symat data type."
  (let ((i (min i j))
	(j (max i j)))  
   ;; Access is computed based on (a) the row-major order index
   ;; minus (b) the redundant values, corresponding to the sum of 
   ;; positive integers up to i:
    (aref (symat-table sm)
	  (- (+ j (* (symat-dimensions sm) i)) ;; (a)
	     (/ (* i (1+ i)) 2)))))            ;; (b)


(defun set-symat-ref (sm i j v)
  "Destructive modifier function to be used by setf."
  (let ((i (min i j))
	(j (max i j)))
    (setf (aref (symat-table sm)
	      (- 
	       (+ j (* (symat-dimensions sm) i))
	       (/ (* i (1+ i)) 2)))
      v)))

;; Make setf work with symat-ref:
(defsetf symat-ref set-symat-ref) 

(defmethod print-object ((sm symat) stream)
  "symat pretty-printing for the Lisp REPL."
  (print-unreadable-object (sm stream :type t :identity t)
    (format stream "[dim: ~d]" (symat-dimensions sm))))



;; Finally, below we add some functionality for mapping between
;; strings (words) and numerical ids. We'll later use the ids for
;; indexing the proximity matrix. We'll use the hash-table in the
;; vs-slot `string-map' for storing the mapping from strings to ids,
;; and the slot `id-map' for storing an array recording the reverse
;; mapping from numerical identifiers back to strings.

(defun word-count (vs)
  "Looks up the number of words in the vector space."
  (hash-table-count (vs-matrix vs)))

(defun map-strings-to-ids (vs)
  ;; We'll use an array for mapping from numerical ids to strings
  ;; (words), stored in the vs-slot `id-map':
  (setf (vs-id-map vs) (make-array (word-count vs)))
  
  ;; We'll use the hash-table in the slot `string-map' for mapping
  ;; strings to ids. Here we assign the mappings:
  (loop 
     with string-map = (vs-string-map vs)
     with id-map = (vs-id-map vs)
     for word being the hash-keys of (vs-matrix vs)
     for i from 0
     do 
       (setf (gethash word string-map) i)
       (setf (aref id-map i) word)))

(defun id2string (id vs)
  (aref (vs-id-map vs) id))

(defun string2id (string vs)
  (gethash string (vs-string-map vs)))




;;; 1 A)

(defun compute-proximities (vs)
  "Computes and stores a proximity matrix for a given vs structure."
  (let* ((n (word-count vs))
	 (prox (make-symat n))
         (sim-fn (vs-similarity-fn vs)))

    ;; Initilize the mapping between words and numerical ids, as used
    ;; for accessing the proximity matrix:
    (map-strings-to-ids vs)

    (loop 
       for i below n
       for vec-i = (get-feature-vector vs (id2string i vs))
       do (loop 
	     for j from i below n 
	     for vec-j = (get-feature-vector vs (id2string j vs))
	     do (setf (symat-ref prox i j)
		      (funcall sim-fn vec-i vec-j))))
			      
    (setf (vs-proximity-matrix vs) prox)))


(defun get-proximity (vs word1 word2)
  "Retrieves the proximity (by look-up) for a given pair of words."
  (symat-ref (vs-proximity-matrix vs) 
             (string2id (normalize-token word1) vs)
             (string2id (normalize-token word2) vs)))



;;; 1 B)

(defun find-knn (vs word &optional (k 5))
  "Return the k nearest neighbors for a given word in the v space."
  (let ((knn))
    (loop 
        with prox = (vs-proximity-matrix vs)
        with i = (string2id (normalize-token word) vs)
        for j below (word-count vs)
        unless (= i j)
        collect (cons j (symat-ref prox i j))
        into neighbors
        finally (setq knn
                  (subseq (sort neighbors #'> :key #'cdr)
                          0 k)))
    (mapcar #'(lambda (n) (id2string (first n) vs)) knn)))


;; Example call sequence:
;;
;; (compute-proximities space)
;;
;; (find-knn vspace "egypt")
;; ==> 
;; ("congo" "germany" "europe" "italy" "america")
;;
;;(find-knn vspace "butter" 10)
;; ==>
;;("salt" "sauce" "eggs" "pepper" "mustard" "milk" "water" "toast" "meat")



;;; 2 A)

;; A note on how we store the class information: Each class label is
;; used as a key in a hash-table, where for each class we store a
;; property list with two keys, :members and :centroid.

(defun read-classes (vs file)
  "Read class file and store the information in the vs structure."
  (let ((classes (make-hash-table)))
    (with-open-file (stream file)
      (loop 
          for list = (read stream nil nil)
          for class = (first list)
          for members = (mapcar #'normalize-token (second list))
          while list
          do (setf (gethash class classes)
               (list :members members))
          finally (setf (vs-classes vs) classes)))))



;;; 2 B)

;; In addition to the function `compute-class-centroids' we define the
;; helper functions `retrieve-vectors', `vector-average', 
;; `sum-vectors', `get-class-centroid' and `get-class-members'.

(defun sum-vectors (&rest vectors)
  (let ((sum (make-hash-table :test #'equal)))
    (dolist (vec vectors)
      (maphash #'(lambda (dim val)
		   (incf (gethash dim sum 0) val))
	       vec))
    sum))
          
(defun vector-average (&rest vectors)
  "Computes a centroid for an arbitrary number of vectors."
  (let ((n (length vectors))
        (sum (apply #'sum-vectors vectors)))
    (maphash #'(lambda (dim val)
                 (setf (gethash dim sum) (/ val n)))
             sum)
    sum))

(defun retrieve-vectors (vs words)
  "Return a set of feature vectors for a given set of words."
  (mapcar #'(lambda (w) (get-feature-vector vs w)) words))

(defun get-class-centroid (name vs)
  (getf (gethash name (vs-classes vs)) :centroid))

(defun get-class-members (name vs)
  (getf (gethash name (vs-classes vs)) :members))
     

(defun compute-class-centroids (vs)
 "Compute and store the average-vectors for each class."
 (loop 
    with classes = (vs-classes vs)      
    for label being the hash-keys of classes
    using (hash-value class)
    for words = (get-class-members label vs)
    for vectors = (retrieve-vectors vs words)
    for centroid = (length-normalize-vector 
		    (apply #'vector-average vectors))
    ;;; For each class label, store the centroid in the 
    ;;; property list we created in `read-classes':
    do (setf (gethash label classes)
	     (append (list :centroid centroid) class))))


;;; 2 C)


(defun rocchio-classify (vs)
 "Classify all words labeled :unknown according to centroid distance."
 (loop 
    with sim-fn = (vs-similarity-fn vs)
    with classes = (vs-classes vs)
    with unknown-words = (get-class-members :unknown vs)
    with unknown-vectors = (retrieve-vectors vs unknown-words)
    with named-centroids = 
      (loop 
	 for label being the hash-keys of classes 
	 unless (eq label :unknown)
	 collect (cons label (get-class-centroid label vs)))
    for word in unknown-words
    for vec in unknown-vectors
    ;;; Find the class label with the closest centroid 
    ;;; for each unknown (unlabeled) word. 
    collect 
      (loop with max-label with max-sim = 0
	 for (label . center) in named-centroids
	 for sim = (funcall sim-fn vec center)
	 when (> sim max-sim)
	 do (setq max-label label max-sim sim)
	 finally (return (list word max-label max-sim)))))


;; Example call sequence:

#||

(progn 
 (read-classes vspace "classes.txt")
 (compute-class-centroids vspace)
 (rocchio-classify vspace))

||#

;; ==>
;;
;; (("fruit" :FOODSTUFF 0.3667868) ("california" :PERSON_NAME 0.29967904)
;;  ("peter" :PERSON_NAME 0.30886117) ("egypt" :PLACE_NAME 0.3074144)
;;  ("department" :INSTITUTION 0.5497112) ("hiroshima" :PLACE_NAME 0.2304558)
;;  ("robert" :PERSON_NAME 0.59566295) ("butter" :FOODSTUFF 0.3845482)
;;  ("pepper" :FOODSTUFF 0.36386278) ("asia" :PLACE_NAME 0.39868993)
;;  ("roosevelt" :TITLE 0.2597388) ("moscow" :PLACE_NAME 0.4841283)
;;  ("senator" :TITLE 0.35630104) ("university" :INSTITUTION 0.5805089)
;;  ("sheriff" :TITLE 0.22804837))



;;; 2 D) 
;;;
;;;
;;; The main difference between the feature vectors and the centroid vectors,
;;; is that the latter will be much more dense. Being the average of all the
;;; feature vectors in a class, a centroid will typically have a much higher
;;; proportion of active (non-nil) features. This also means that the
;;; hash-table representations might no longer be the optimal choice, and we
;;; might consider switching to arrays instead.
;;;
;;; Elaborating further:
;;;
;;; One solution here would be to use "dynamic" data structures,
;;; ie. changing our data structure depending on properties of the
;;; feature vectors. For example, we could decide to use hash-tables
;;; up to a certain level of density, and then switch to an
;;; array-representation for very dense vectors. For very sparse
;;; vectors, on the other hand, we could choose to use simple
;;; association lists.
;;;
;;; Now, if we wanted to use such dynamic and heterogeneous data types, our
;;; functions that operate on vectors would have to able to do the right thing
;;; depending on the underlying type of the vector arguments. For example, we
;;; would need to implement support for computing the `dot-product' for a pair
;;; of vectors where one is a hash-table and the other is an array.




;; 3 A)
;;
;; Keyword summary of the main differences between Rocchio classification
;; and kNN classification:
;;
;; Rocchio:
;; ----------------
;;
;; +Decision boundary: Linear.
;;
;; +Class representation: Centroid-based.
;;
;; +The classification decision: Objects are assigned to the class with
;; the nearest centroid vector
;;
;; +Disadvantages: Ignores details of the distribution of points within a
;; class and implicitly assumes that classes are equally sized
;; spheres. Only suitable for linearly separable classes.
;;
;; +Advantages: Test time independent of the number of training examples;
;; linear in the number of classes. Conceptually simple.
;;
;;
;;
;; kNN:
;; ----------------
;;
;; +Decision boundary: Non-linear.
;;
;; +Class representation: Exemplar-based.
;;
;; +The classification decision: Objects are assigned to the class of the
;; majority of the $k$ nearest neighbors.
;;
;; +Disadvantages: The parameter $k$ must be specified in advance, either
;; manually or by optimizing on held-out data. No real learning going on,
;; simply memorizes all training examples (an instance of so-called
;; memory-based learning). Large training comes with an efficiency
;; penalty during classification, as there are then more pairwise
;; similarities to compute and compare (test time is linear in the size
;; of the training set).
;;
;; +Advantages: Decision boundary is determined locally. Deals better
;; with classes that cover disconnected or non-spherical regions or other
;; irregular shapes. Applicable to non-linear classification
;; problems. Test time is independent of the number of classes, a
;; potential advantage for problems with many classes.


;; 3 B)
;;
;; Rocchio classification vs k-means clustering:
;;
;; While both methods aim to model categories in the data, they're
;; instances of supervised and unsupervised learning
;; respectively. Rocchio classification is an instance of supervised
;; learning using labeled training data: The class labels that we want to
;; learn to predict are pre-specified for each training example. In
;; contrast, k-means clustering is an instance of unsupervised learning
;; where the goal is to automatically discover groups in unlabeled
;; data. Both methods rely on a centroid-based representation of the
;; categories that they model, and both determine group membership based
;; on centroid distance. In this respect, k-means can be viewed as an
;; unsupervised variant of Rocchio.


;; 3 C)
;; 
;; In order to evaluate our Rocchio classifier above we would at least
;; need to have gold standard class labels for the test instances (the
;; :unknown words) telling us what the correct class assignments
;; should be. This would allow us to determine whether the various
;; decisions of our classifier are wrong or correct, and more
;; specifically whether they represent true positives / false
;; positives / true negatives / false negatives. On the basis of this
;; we could then compute evaluation scores like precision, recall and
;; the combined f-measure. Since we're dealing with a multi-class
;; problem we would also need to compute aggregated scores based on
;; macro- or micro-averaging. The first strategy would mean computing
;; precision, recall etc for each class and then average the scores
;; across the classes, whereas the second strategy would mean first
;; counting all the TP/FP/TN/FNs irrespective of the particular class
;; labels and the compute the global evaluation scores directly from
;; these. Since there are no large differences here with respect to
;; the sizes of the classes, we would expect the specific choice of
;; macro vs micro-averaging to not matter that much, but we would
;; nonetheless need more test data than what we have use here in order
;; to get reliable and representative scores.
