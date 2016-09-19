;;; Hey, emacs(1), this is -*- Mode: Common-Lisp; Coding: utf-8; -*-, got it?

(in-package :common-lisp-user)

;;;;
;;;; INF4820; Fall 2016.
;;;; Model Solutions for the Problems in Exercise (1)
;;;;



;;; 1 List processing

;;; [a] 
;;;
;;; We show a few different solutions for this first problem:
;;;
;;; (first (rest (rest '(apple orange pear lemon))))
;;;
;;; (car (cdr (cdr '(apple orange pear lemon))))
;;;
;;; (caddr '(apple orange pear lemon))
;;;
;;; (nth 2 '(apple orange pear lemon))
;;;
;;; (third '(apple orange pear lemon))


;;; [b] 
;;; Two examples:
;;;
;;; (nth 0 (nth 1 '((apple orange) (pear lemon))))
;;; 
;;; (first (first (rest '((apple orange) (pear lemon)))))


;;; [c] 
;;; Two examples:
;;;
;;; (first (nth 2 '((apple) (orange) (pear))))
;;;
;;; (first (first (rest (rest '((apple) (orange) (pear))))))


;;; [d] 
;;;    List a:
;;;    (cons
;;;     (cons 'apple (cons 'orange nil))
;;;     (cons (cons 'pear (cons 'lemon nil)) nil))
;;;
;;;    List b:
;;;    (cons
;;;     (cons 'apple nil)
;;;     (cons
;;;      (cons 'orange nil)
;;;      (cons (cons 'pear nil) nil)))


;;; [e] 
;;; 
;;; First, a `long' list of (kind of) arbitrary length:

(defparameter *foo* (loop for i from 1 to 43 collect i))

;;;
;;; A few variants, using various built-in list operations:
;;;
;;; (second (reverse *foo*))
;;;
;;; (nth (- (length *foo*) 2) *foo*)
;;;
;;; (first (last *foo* 2))
;;;
;;;
;;; Finally, we define a few procedures to do the same.
;;;
;;; using recursion:
(defun next-to-last (list)
  (if (null (rest (rest list)))
    (first list)
    (next-to-last (rest list))))

;;; using `loop':
(defun next-to-last-loop (list)
  (loop for l = list then (rest l)
	when (null (rest (rest l)))
	return (first l)))

;;; using `do':
(defun next-to-last-do (list)
  (do ((l list (rest l)))
      ((null (rest (rest l)))
       (first l))))



;;; 2 Interpreting Common Lisp

;;; The function (called `foo') returns the length of the given list argument
;;; (also called `foo').  The function traverses the list through recursive
;;; calls to itself, until reaching the end of the list.  Within the function
;;; body, `foo' is interpreted as a list in the first argument to the `if'
;;; test, and in the call to `rest'.  In the one remaining occurrence it is
;;; interpreted as a function (since it is there the first element of a list).



;;; 3 Variable assignment

;;; [a]
;;; 
;;; There are several possibilities here, but ???? could for example be
;;; replaced with one of these:
;;;
;;; (setf (first foo) 42)
;;; 
;;; (pop foo)
;;; 
;;; (push 42 foo)
;;; 
;;; (setf foo (rest foo))


;;; [b]
;;;
;;; (let* ((keys '(:a :b :c))
;;;        (values '(0 1 2))
;;;        (pairs (pairlis keys values)))
;;;   (setf (rest (assoc :b pairs)) 42)
;;;   (rest (assoc :b pairs)))


;;; [c]
;;; 
;;; (incf (gethash 'meaning foo))


;;; [d]
;;; 
;;; (setf (aref foo 2) 42)



;;; 4 Recursion and iteration
;;;
;;; [a] 
;;;
;;; recursive variant:
(defun count-member (sym list)
  (cond ((null list) 0)
	((eq sym (first list))
	 (+ 1 (count-member sym (rest list))))
	(t (count-member sym (rest list)))))

;;; tail-recursive variant (with a helper function):
(defun count-member-it (sym list)
  (count-it sym list 0))

(defun count-it (sym list sum)
  (if (null list)
      sum
    (count-it sym (cdr list) 
	      (if (eq sym (first list)) 
		  (+ 1 sum)
		sum))))

;; Note that we could also have defined the helper function `count-it'
;; internally in `count-member-it' using `labels'---a construction somewhat
;; similar in spirit to `let' but used for introducing local functions rather
;; than local variables.


;;; [b]
;;;
;;; iteration using dolist:
(defun count-member-do (sym list)
  (let ((n 0))
    (dolist (x list)
      (when (eq sym x)
	(incf n 1)))
    n))

;;; iteration using loop:
(defun count-member-loop (sym list)
  (loop
      for x in list
      count (eq sym x)))



;;; 5 Reading a Corpus File; Basic Counts
;;;
;;; [a] 
(defun tokenize (string)
  (loop 
      for start = 0 then (+ space 1)
      for space = (position #\space string :start start)
      for token = (subseq string start space)
      unless (string= token "") collect token
      until (not space)))

(defparameter *corpus*
  (with-open-file (stream "~/inf4820/1/brown1.txt" :direction :input)
    (loop
        for line = (read-line stream nil)
        while line append (tokenize line))))

;;;
;;; The expression returns a list consisting of every white-space–delimited
;;; token in the input file, each token being a string. 
;;;
;;; More precisly, the return value of `with-open-file' is the value of the
;;; last form in its body, i.e. the `loop'.  The return value of a `loop'
;;; depends on the use of accumulators (e.g. `collect' or `append') in its body.
;;; In this case, the `loop' returns the result of `append'ing the return
;;; values of `tokenize' for each line, i.e. a list of all tokens from the
;;; corpus. 


;;; [b] 
;;;
;;; (length *corpus*) ==> 23132


;;; [c] 
;;;
;;; Currently we are tokenizing only by splitting on white-space.  This
;;; strategy could easily be improved by performing various kinds of 
;;; normalization like
;;;
;;; - lower-casing all strings,
;;; - discarding punctuation (minimally at the end of the string, but possibly
;;;   also in other positions), 
;;; - maybe splitting on certain kinds of punctuation like hyphens and single
;;;   apostrophies. 
;;;
;;; This would mean that instead of a list of tokens such as ("He'll" "ask"
;;; "Peter's" "friend.") we would have ("he" "ll" "ask" "peter" "'s" "friend").
;;; In other words, we would get fewer unique word types, and that is typically
;;; a good thing: If we want to know how often the word "friend" occurs in the
;;; corpus, we probably also want to count its occurrences in the form of 
;;; "Friend", "friend!", etc.


;;; [d]
;;;
;;; a version with `loop':
(defparameter *types*
  (loop 
      with table = (make-hash-table :test #'equal)
      for token in *corpus*
      do (incf (gethash token table 0))
      finally (return table)))

;;; example of use of the table:
;;; (gethash "effective" *types*) ==> 3


;;; [e]
;;;
;;; (hash-table-count *types*) ==> 6311


;;; [f]
;;;
;;; To better normalize during tokenization, we implement some of the choices
;;; discussed under [5c] above, i.e. assume that any non-alphanumeric character
;;; should give rise to a token boundary.  This way, we will split off initial
;;; and trailing punctuation marks, as well as break at token-internal hyphens,
;;; slashes, and apostrophes.  This is more or less what NLP currently assumes
;;; is the de-facto standard for English tokenization, even though it begs the
;;; question of how to deal adequately with named entities that characterized
;;; by an idiosyncratic surface pattern, e.g. email addresses, URLs, or various
;;; time and date expressions (e.g. ‘12:15pm’ or ‘09/15/16’).  An intelligent
;;; tokenizer would need to know these patterns and maybe recognize them prior
;;; to stipulating token boundaries.  Be that as it may, once we have separated
;;; out punctuation marks as tokens of their own, it is easy to discard them in
;;; reading the corpus.  Again, this is quite a simplification, as oftentimes
;;; one might want to preserve (some) punctuation marks, because they provide
;;; important cues to interpretation (e.g. for quotes speech) or the distinction
;;; between a regular statement, a question, or a command.
;;;
(defun tokenize2 (string)
  (loop
      for start = 0 then (+ break 1)
      for break = (position-if-not #'alphanumericp string :start start)
      for token = (subseq string start break)
      unless (or (string= token "") (notany #'alphanumericp token))
      collect (string-downcase token)
      until (not break)))

#+:null
(defparameter *types*
  (loop 
      with table = (make-hash-table :test #'equal)
      for token
      in (with-open-file (stream "~/inf4820/1/brown1.txt" :direction :input)
           (loop
               for line = (read-line stream nil)
               while line append (tokenize2 line)))
      do (incf (gethash token table 0))
      finally (return table)))

;;; (hash-table-count *types*) ==> 4597

