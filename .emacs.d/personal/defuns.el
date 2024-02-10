;;; Personal functions

(require 'cl-lib)
(require 's)

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
          (projectile-prepend-project-name "rg search for import: ")
          (projectile-symbol-or-selection-at-point))))
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
	(let* ((default-directory (projectile-project-root))
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

(define-key global-map (kbd "s-i" ) 'search-import)

;;(define-key global-map (kbd "C-x 4 s" ) `sbt-switch-to-active-sbt-buffer)

;; Taken from
;; https://www.emacswiki.org/emacs/KeyboardMacrosTricks
(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

;; Used by yasnippet generating Play json formatter - Json.format[Foo]
(defun downcase-first-letter-only (string)
  "Lower case first letter of provided string"
  (when (> (length string) 0)
    (let ((first-letter (substring string 0 1))
	  (rest-of-letters (substring string 1)))
      (format "%s%s" (downcase first-letter) rest-of-letters))))

(defun current-scala-symbol (minibuffer-content)
  (cond ((string-match "^\\#\\(def\\|val\\|new\\|class\\|trait\\|object\\|type\\|struct\\|enum\\|fn\\|impl\\)\\\\s\\([^[]*\\)" minibuffer-content)
         (cons (match-string 1 minibuffer-content) (match-string 2 minibuffer-content)))
        ((string-match "^\\#\\(.*\\)" minibuffer-content)
         (cons nil (match-string 1 minibuffer-content)))))

(defun insert-or-replace-word (scala-symbol)
  (let* ((current-match-data (current-scala-symbol (minibuffer-contents-no-properties)))
         (minibuffer-scala-symbol (car current-match-data))
         (search-term (cdr current-match-data)))
    (delete-minibuffer-contents)
    (if (string= minibuffer-scala-symbol (symbol-name scala-symbol))
	(insert (format "#%s" search-term))
      (insert (format "#%s\\s%s[\\\\[(\\ :]?" scala-symbol search-term)))))

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


(defun in-minibuffer-show-hydra()
  (interactive)
  (with-current-buffer (nth 1 (buffer-list))
    (cond
     ((derived-mode-p 'rust-mode)
      (rust-minibuffer-search/body))
     ((derived-mode-p 'scala-mode)
      (scala-minibuffer-search/body)))))

(defun mini-hook ()
  (let ((non-minibuffer-buffer (nth 1 (buffer-list))))
    (if (and
         (with-current-buffer non-minibuffer-buffer (derived-mode-p 'rust-mode))
         (string-match ".*Ripgrep.*" (minibuffer-prompt)))
        (progn
          (rust-minibuffer-search/body))
      (when (and
             (functionp 'sbt:find-root)
             (sbt:find-root)
             ;;(string-match ".*rg ?(app)*?:.*" (minibuffer-prompt)))
             (string-match ".*Ripgrep.*" (minibuffer-prompt)))
        (scala-minibuffer-search/body)))
    ))

(add-hook 'minibuffer-setup-hook 'mini-hook)
;; (remove-hook 'minibuffer-setup-hook 'mini-hook)

(defhydra run-mongo ()
  "
Run mongo: _r_ reset _s_ start _n_ start no-auth _e_ eof _t_ shell _q_ quit"
  ("r" (mongo "reset") nil)
  ("s" (mongo "start") nil)
  ("n" (mongo "start-no-auth") nil)
  ("e" (send-eof) nil)
  ("t" (switch-to-shell) nil)
  ("q" nil nil :color blue))

(defun switch-to-shell ()
  "Switch to shell with running mongo db"
  (let* ((sbt-root (sbt:find-root))
         (buffer-name (format "*shell* %s" sbt-root))
         (buffer (get-buffer buffer-name)))
    (if (and sbt-root buffer)
        (switch-to-buffer-other-window buffer)
      (message "Not in sbt project."))))

(defmacro with-shell-in-sbt-project (body)
  `(let* ((sbt-root (sbt:find-root))
          (buffer-name (format "*shell* %s" sbt-root))
          (buffer (get-buffer buffer-name)))
     (if sbt-root
         (progn
           (unless buffer
             (with-current-buffer (shell)
               (rename-buffer buffer-name)
               (comint-send-string (current-buffer) (concat "invoked-from-directory " sbt-root "\n"))
               (setq buffer (current-buffer))))
           (with-current-buffer buffer
             ,body))
       (message "Not in sbt project."))))

(defun send-eof ()
  (with-shell-in-sbt-project
   (comint-send-eof)))

(defun mongo (command)
  (interactive)
  (with-shell-in-sbt-project
   (comint-send-string (current-buffer) (concat sbt-root (format "run-mongo.sh %s" command) "\n"))))

(defun restclient-suppress-by-default ()
  (interactive)
  (pcase current-prefix-arg
    (`nil (define-key restclient-mode-map [remap restclient-http-send-current-suppress-response-buffer] 'restclient-http-send-current)
          (define-key restclient-mode-map [remap restclient-http-send-current] 'restclient-http-send-current-suppress-response-buffer)
          (message "rest-mode C-c command remapped."))
    (`(,n . nil) ;; run with C-u
     (define-key restclient-mode-map [remap restclient-http-send-current-suppress-response-buffer] nil)
     (define-key restclient-mode-map [remap restclient-http-send-current] nil)
     (message "rest-mode commands keys has been reseted."))))

(defvar restclient:last-calls-args nil)

(defun restclient:save-call (orig-fun &rest args)
  (setq restclient:last-calls-args args)
  (apply orig-fun args))

(defun restclient:repeat-last-call ()
  (interactive)
  (if (null restclient:last-calls-args)
      (message "No previous restmode call found.")
    (apply #'restclient-http-do restclient:last-calls-args)))

(advice-add 'restclient-http-do :around #'restclient:save-call)

(defun tut-toggle-between-scala-and-markdown()
  (interactive)

  (when (equal (buffer-name) "slides.html")
    (if (equal major-mode 'scala-mode)
        (progn
          (widen)
          (markdown-mode))
      (save-excursion
        (search-backward "```tut")
        (next-line)
        (let ((start (point)))
          (search-forward "```")
          (beginning-of-line)
          (narrow-to-region start (point))
          (scala-mode))))))

(define-key global-map (kbd "C-x m" ) `tut-toggle-between-scala-and-markdown)

(defun nxml-pretty-format ()
  (interactive)
  (let ((buffer-string (buffer-string))
        (point (point))
        (exit-code (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t "*xmllint errors*" t)))
    (cond ((zerop exit-code)
           (deactivate-mark)
           (nxml-mode))
          ((eq 1 exit-code)
           (when (s-blank? (buffer-string))
             (insert buffer-string)
             (goto-char point))))))

(defun no-test-line ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (flush-lines "^test/" (point-min) (point-max) t)
      (flush-lines "src/test/" (point-min) (point-max) t))))

(defun pre-process-kill-ring-element (element)
  (replace-regexp-in-string "^[[:space:]]+" ""
                            (replace-regexp-in-string "[[:space:]]+$" "" (substring-no-properties element))))

(defun preprocess-kill-ring ()
  (let ((result nil)
        (element nil))
    (dolist (element kill-ring)
      (progn
        (setq element (pre-process-kill-ring-element element))
        (when (not (or
                    (eq 0 (length element))
                    (string-match-p "[\r\n]+" element)))
          (setq result (cons element result)))))
    (reverse result)))

(defconst gform-environments
  '("http://localhost:9196/gform/formtemplates"
    "http://localhost:9396/gform/formtemplates"
    "https://www.development.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://www.qa.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://www.staging.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://test-www.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"))

(defvar gform-last-url (car gform-environments))

;; TODO Rewrite to macro
(defun resend-last-suppress-response-buffer (prefix-arg)
  (interactive "p")
  (when-let ((buffer (seq-some (lambda (buffer)
                                 (with-current-buffer buffer
                                   (when (and (stringp mode-name)
                                              (or (string= mode-name "JSON")
                                                  (string= mode-name "REST Client")))
                                     buffer)))
                               (buffer-list))))

    (with-current-buffer buffer
      (pcase mode-name
        ("JSON"
         (upload-template prefix-arg))
        ("REST Client"
         (set-buffer-multibyte nil)
         (restclient-http-send-current-suppress-response-buffer)
         (set-buffer-multibyte t)
         (message "Uploading rest-client buffer %s" (buffer-name buffer)))))))

(defun resend-last (prefix-arg)
  (interactive "p")
  (when-let ((buffer (seq-some (lambda (buffer)
                                 (with-current-buffer buffer
                                   (when (and (stringp mode-name)
                                              (or (string= mode-name "JSON")
                                                  (string= mode-name "REST Client")))
                                     buffer)))
                               (buffer-list))))

    (with-current-buffer buffer
      (pcase mode-name
        ("JSON"
         (upload-template prefix-arg))
        ("REST Client"
         (set-buffer-multibyte nil)
         (restclient-http-send-current)
         (set-buffer-multibyte t)
         (message "Uploading rest-client buffer %s" (buffer-name buffer)))))))

(defun upload-template-or-handlebar (prefix-arg)
  (interactive "p")
  (if (string= "hbs" (file-name-extension (buffer-file-name)))
      (upload-handlebarstemplates prefix-arg)
    (upload-template prefix-arg)))

(defun upload-template (prefix-arg)
  (interactive "p")

  (when (eq 4 prefix-arg)
    (let ((url (projectile-completing-read  "Choose environment: " gform-environments)))
      (setq gform-last-url url)))

  (let ((json (buffer-string)))
    (with-temp-buffer
      (insert (format "# -*- restclient -*-

POST %s
Content-Type: application/json; charset=utf-8
Csrf-Token: nocheck

" gform-last-url))
      (insert json)
      (restclient-mode)
      (set-buffer-multibyte nil)
      (restclient-http-send-current-stay-in-window))
    (message "Uploading json-mode buffer %s to %s" (buffer-name (current-buffer)) gform-last-url)))


(defconst gform-environments-new-form
  '("http://localhost/submissions/new-form/"
    "http://localhost:9295/submissions/new-form/"
    "https://www.development.tax.service.gov.uk/submissions/new-form/"
    "https://www.qa.tax.service.gov.uk/submissions/new-form/"
    "https://www.staging.tax.service.gov.uk/submissions/new-form/"
    "https://test-www.tax.service.gov.uk/submissions/new-form/"))

(defvar gform-last-new-form-url (car gform-environments-new-form))

(defun open-template-in-browser (prefix-arg)
  (interactive "p")
  (let ((find-template-id-regex "\"_id\"[[:space:]]*:[[:space:]]*\"\\([a-zA-Z0-9-]*\\)\""))
    (when (eq 4 prefix-arg)
      (let ((url (projectile-completing-read  "Choose environment: " gform-environments-new-form)))
        (setq gform-last-new-form-url url)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward find-template-id-regex)
      (browse-url (concat gform-last-new-form-url (match-string 1))))))

(defun gform-hmrc-start ()
  (interactive)
  (with-current-buffer (dired-noselect "/Users/pepa/develop-hmrc/gform-frontend")
    (sbt-hydra)
    (add-hook 'sbt-hydra:after-create-hook 'gform-hmrc-sbt-run)))

(defun gform-hmrc-sbt-run ()
  (with-current-buffer (dired-noselect "/Users/pepa/develop-hmrc/gform")
    (sbt-hydra)
    (remove-hook 'sbt-hydra:after-create-hook 'gform-hmrc-sbt-run)))

(defun gform-foldright-start ()
  (interactive)
  (with-current-buffer (dired-noselect "/Users/pepa/develop-foldright/gform-frontend")
    (sbt-hydra)
    (add-hook 'sbt-hydra:after-create-hook 'gform-foldright-sbt-run)))

(defun gform-foldright-sbt-run ()
  (gform-run-microservice)
  (with-current-buffer (dired-noselect "/Users/pepa/develop-foldright/gform")
    (sbt-hydra)
    (remove-hook 'sbt-hydra:after-create-hook 'gform-foldright-sbt-run)
    (add-hook 'sbt-hydra:after-create-hook 'gform-foldright-launch)))

(defun gform-foldright-launch ()
  (gform-run-microservice)
  (remove-hook 'sbt-hydra:after-create-hook 'gform-foldright-launch))

(defun gform-run-microservice ()
  (sbt-hydra:run "microservice"))

(defun playframework-project-app-dir ()
  "If current project is play framework project return its app directory, otherwise return nil"
  (when-let ((project-root (projectile-project-root))
             (app-dir (concat project-root "app")))
    (when (and (file-directory-p app-dir)
               (eq 'sbt (projectile-project-type app-dir)))
      app-dir)))

(defun my/symbol-or-selection-at-point ()
  (let ((selection (projectile-symbol-or-selection-at-point)))
    ;;(ivy-toggle-regexp-quote)
    ;;(regexp-quote (replace-regexp-in-string " " "  " selection))
    ;;(regexp-quote selection)
    (unless (string= "Recent" selection) ;; Exception for Recent word from magit status buffer
      selection)))

(defvar look-for-thing-at-point-in-app-only-p t)
(defvar literal-counsel-rg-p t)

(defun look-for-thing-at-point ()
  (interactive)
  ;; Sort result by path
  (let ((app-dir (playframework-project-app-dir)))
    (if (and look-for-thing-at-point-in-app-only-p app-dir)
        (if literal-counsel-rg-p
            (run-literal-rg app-dir)
          (run-vanilla-rg app-dir))
      (if literal-counsel-rg-p
          (run-literal-rg nil)
        (run-vanilla-rg nil)))))

(defun run-literal-rg (app-dir)
  (let ((counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --fixed-strings %s")
        (counsel-ag-command "rg -M 240 --with-filename --no-heading --line-number --fixed-strings %s"))
    (literal-counsel-rg (my/symbol-or-selection-at-point) app-dir (format "literal-rg%s: " (if app-dir " (app)" "")))))

(defun run-vanilla-rg (app-dir)
  (let ((counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number  --color never %s"))
    (counsel-rg (my/symbol-or-selection-at-point) app-dir nil (format "vanilla-rg%s: " (if app-dir " (app)" "")))))

(defun my-shell-quote-argument (argument)
  (format "'%s'" argument))

(defun show-messages ()
  (interactive)
  (delete-other-windows)
  (let* ((messages (get-buffer-create "*Messages*"))
         (left (selected-window))
         (right (split-window left nil t)))
    (set-window-buffer right messages)
    (with-current-buffer messages
      (set-window-point right (point-max)))))

(defun scala-docs ()
  (interactive)
  (let ((libs-versions
         '(("2.13.5" . "https://www.scala-lang.org/api/2.13.5/index.html")
           ("2.12.13" . "https://www.scala-lang.org/api/2.12.13/index.html")
           ("cats" . "https://typelevel.org/cats/api/cats/index.html")
           ("ce-2" . "https://typelevel.org/cats-effect/api/2.x/")
           ("ce-3" . "https://typelevel.org/cats-effect/api/3.x/")
           ("fs2-3.0.1" . "https://javadoc.io/doc/co.fs2/fs2-core_2.13/3.0.1/fs2/index.html"))))
    (completing-read "Open docs: " libs-versions)))

(defun http-compare-last-two-responses ()
  (interactive)
  (let* ((compare-candidates (seq-filter (lambda (buffer)
                                           (eq 0 (string-match "*HTTP GET" (buffer-name buffer))))
                                         (buffer-list)))
         (buffer-a (car compare-candidates))
         (buffer-b (cadr compare-candidates)))

    (if (and buffer-a buffer-b)
        (ediff-buffers buffer-a buffer-b)
      (message "No available buffers to compare. Setting to `restclient-same-buffer-response' to nil")
      (setq restclient-same-buffer-response nil))))

(defun http-compare-last-two-responses-undo ()
  (interactive)
  (message "Setting to `restclient-same-buffer-response' to t")
  (setq restclient-same-buffer-response t))

(defun gform-internal-auth-refresh ()
  (interactive)
  (let ((output (process-lines "internal-auth-token-refresher")))
    (message "%s" (car output))))

(defun rust-test-only ()
  "Test using `cargo test`"
  (interactive)

  ;;(rust--compile "%s test %s" rust-cargo-bin "tests")

  (rust--compile "%s test %s" rust-cargo-bin "")
  ;;(rust--compile "%s test %s" rust-cargo-bin "glyph::segments::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "model::algebra::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "glyph::polygon::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "model::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "glyph_rasterizer::tests::test_rasterization")
  ;;(rust--compile "%s test %s -- --nocapture" rust-cargo-bin "tests::test_char_code_to_glyph_id")
  ;;(rust--compile "%s test %s" rust-cargo-bin "tests::test_monaco_char_a")
  ;;(rust--compile "%s test %s" rust-cargo-bin "bezier::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "snap_to_grid::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "controls::tests")
  ;;(rust--compile "%s test %s" rust-cargo-bin "controls::tests::snap_to_grid_right_middle_5")
  )

(defun rust-expand ()
  "Expand macros using `cargo expand`"
  (interactive)
  (rust--compile "%s expand --color never --lib --tests -- %s" rust-cargo-bin "controls")
  )

(define-key rust-mode-map (kbd "C-c C-t") 'rust-test-only)
(define-key rust-mode-map (kbd "C-c C-e") 'rust-expand)
