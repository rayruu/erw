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

