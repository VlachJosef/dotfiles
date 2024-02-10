(require 'hydra)

(defvar my-enabled-minibuffer-hydras nil
  "Enable hydra in minibuffer only for ripgrep command")

(defun current-language-keyword-symbol (minibuffer-content)
  (cond ((string-match "^\\#\\(def\\|val\\|new\\|class\\|trait\\|object\\|type\\|struct\\|enum\\|fn\\|impl\\)\\\\s\\([^[]*\\)" minibuffer-content)
         (cons (match-string 1 minibuffer-content) (match-string 2 minibuffer-content)))
        ((string-match "^\\#\\(.*\\)" minibuffer-content)
         (cons nil (match-string 1 minibuffer-content)))))

(defun insert-or-replace-word (language-keyword)
  (let* ((current-match-data (current-language-keyword-symbol (minibuffer-contents-no-properties)))
         (minibuffer-language-keyword-symbol (car current-match-data))
         (search-term (cdr current-match-data)))
    (delete-minibuffer-contents)
    (if (string= minibuffer-language-keyword-symbol (symbol-name language-keyword))
	(insert (format "#%s" search-term))
      (insert (format "#%s\\s%s[\\\\[(\\ :]?" language-keyword search-term)))))

(defun delete-word ()
  (save-excursion
    (move-beginning-of-line 1)
    (let ((defs '(new class object trait val def type)))
      (cond ((member (symbol-at-point) defs)
	     (progn (delete-region (beginning-of-thing 'symbol) (+ 1 (end-of-thing 'symbol)))
		    (move-end-of-line 1)))))))

(defhydra scala-minibuffer-search ()
  "
Search for _n_ new _c_ class _t_ trait _o_ object _v_ val _d_ def _y_ type _q_ quit"
  ("n" (insert-or-replace-word 'new) nil)
  ("c" (insert-or-replace-word 'class) nil)
  ("t" (insert-or-replace-word 'trait) nil)
  ("o" (insert-or-replace-word 'object) nil)
  ("d" (insert-or-replace-word 'def) nil)
  ("v" (insert-or-replace-word 'val) nil)
  ("y" (insert-or-replace-word 'type) nil)
  ("q" (delete-word) nil :color blue))

(defhydra rust-minibuffer-search ()
  "
Search for _s_ struct _t_ trait _e_ enum _f_ fn _i_ impl _q_ quit"
  ("s" (insert-or-replace-word 'struct) nil)
  ("t" (insert-or-replace-word 'trait) nil)
  ("e" (insert-or-replace-word 'enum) nil)
  ("f" (insert-or-replace-word 'fn) nil)
  ("i" (insert-or-replace-word 'impl) nil)
  ("q" (delete-word) nil :color blue))

(defun my-mini-hook ()
  (setq my-enabled-minibuffer-hydras nil)
  (let ((non-minibuffer-buffer (nth 1 (buffer-list))))
    (with-current-buffer non-minibuffer-buffer
      (when (eq this-command #'ripgrep-for-thing-at-point)
        (setq my-enabled-minibuffer-hydras t)
        (in-minibuffer-select-hydra)))))

(defun in-minibuffer-show-hydra()
  (interactive)
  (with-current-buffer (nth 1 (buffer-list))
    (when my-enabled-minibuffer-hydras
      (in-minibuffer-select-hydra))))

(defun in-minibuffer-select-hydra ()
  (cond
   ((derived-mode-p 'rust-ts-mode)
    (rust-minibuffer-search/body))
   ((derived-mode-p 'scala-ts-mode)
    (scala-minibuffer-search/body))))

(add-hook 'minibuffer-setup-hook 'my-mini-hook)

(define-key minibuffer-local-map (kbd "s-F") 'in-minibuffer-show-hydra)
