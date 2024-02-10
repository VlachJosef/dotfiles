(require 'project)

(defun normalize-import (import)
  (let* ((normalized (s-replace " " "" (s-collapse-whitespace import)))
	 (prefixAndClassList (s-split "{" normalized)))
    (pcase (length prefixAndClassList)
      (`1 (cons import nil)) ;; return input as is
      (`2 (let* ((prefix (car prefixAndClassList))
		 (classList (s-split "," (s-replace "}" "" (nth 1 prefixAndClassList))))
		 (value))
	    (dolist (element classList value)
	      (setq value (cons (concat prefix element) value)))))
      (size nil)))) ; input is broken, it is not valid scala import, let's return empty list

(defun normalize-and-sort (input search-term)
  (let ((all-imports (delete-dups (sort (apply #'append (mapcar 'normalize-import input)) 'string<))))
    (cl-remove-if-not (lambda (import) (string-match search-term import)) all-imports)))


(defun symbol-or-selection-at-point ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (substring-no-properties (or (thing-at-point 'symbol) ""))))

(defun search-import (search-term)
  "Use ag to search lines in project starting with keyword import and containing text `search-term'
By default all import clauses are normalized, meaning that any import including `import selector clause'
enclosed in braces is broken down into its own import clause.
Prefix arguments:
   no arg  - normalize
   C-u     - normalize and copy current import to kill ring
   C--     - don't normalize
   C-- C-u - don't normalize and copy current import to kill ring"
  (interactive
   (list (read-from-minibuffer
          (format "[%s] %s" (project-name (project-current t)) "rg search for import: ")
          (symbol-or-selection-at-point))))
  (let ((identity2 (lambda (input search-term) input))
        (normalization-function)
	(copy-to-kill-ring))
    (pcase current-prefix-arg
      (`nil (setq normalization-function `normalize-and-sort)
	    (setq copy-to-kill-ring nil) `identity2)
      (`- (setq normalization-function (lambda (input search-term) input))
	  (setq copy-to-kill-ring nil))
      (`(,n . nil) (if (< 0 n)
		       (progn (setq normalization-function `normalize-and-sort)
			      (setq copy-to-kill-ring t))
		     (progn (setq normalization-function `identity2)
			    (setq copy-to-kill-ring t)))))
    (if (and (executable-find "rg") (executable-find "sort") (executable-find "uniq"))
	(let* ((default-directory (project-root (project-current t)))
	       (res-raw (shell-command-to-string (format "rg \"import.*%s\" --no-line-number --no-filename --no-heading | sort | uniq" search-term)))
	       (lines (split-string (s-replace "import " "" (s-trim res-raw)) "\n"))
	       (import (with-temp-buffer
			 (insert (mapconcat (lambda (elm) (s-trim-left elm)) lines "\n"))
			 (sort-lines nil (point-min) (point-max))
			 (completing-read "Select an import: " (funcall normalization-function (split-string (buffer-string) "\n") search-term) nil nil search-term))))
	  (if copy-to-kill-ring
	      (progn (kill-new (format "import %s" import))
		     (message "%s added to kill ring" import))
	    (scala-import-insert-import import)))
      (error "Commands 'rg', 'sort' and 'uniq' are required to use this command"))))

(defun kill-process-on-port (port)
  (interactive
   (list (read-from-minibuffer
          "kill process on port: "
          (symbol-or-selection-at-point))))
  (shell-command (format "kill -9 $(lsof -ti:%s)" port)))
