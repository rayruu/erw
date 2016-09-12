(add-to-list 
 'load-path (format "%s/slime" (file-name-directory load-file-name)))
(require 'slime-autoloads)
;(setq slime-contribs '(slime-fancy))
(let ((acl (expand-file-name "~oe/inf4820/acl/alisp")))
  (setq inferior-lisp-program (if (file-exists-p acl) acl "/usr/bin/sbcl")))

(defun sbcl ()
  (interactive)
  (setq inferior-lisp-program "sbcl")
  (slime))

