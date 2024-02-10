;; (setq use-package-verbose t
;;       use-package-expand-minimally nil
;;       use-package-compute-statistics t
;;       debug-on-error t)
(setq debug-on-error nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;;
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-control-modifier 'control)

(setq-default indent-tabs-mode nil)
(setq
 initial-scratch-message nil
 column-number-mode t
 dabbrev-case-fold-search nil
 set-mark-command-repeat-pop t ;; immediately after you type C-u C-SPC, you can type C-SPC instead of C-u C-SPC to cycle through the mark ring.
 kill-do-not-save-duplicates t
 dired-dwim-target t
 dired-listing-switches "-alo"
 frame-resize-pixelwise t
 ring-bell-function 'ignore
 scroll-error-top-bottom t
 scroll-preserve-screen-position t
 inhibit-startup-screen t
 initial-buffer-choice "~/.dotfiles/.config/emacs-profiles/29.2/init.el"
 isearch-lazy-count t
 vc-follow-symlinks t
 js-indent-level 2
 css-indent-offset 2
 ediff-window-setup-function 'ediff-setup-windows-plain ;; Do not display Ediff control window in separate frame
 initial-major-mode 'emacs-lisp-mode
 org-hide-emphasis-markers t
 backup-directory-alist '(("." . "~/.saves")))

(defalias 'yes-or-no-p 'y-or-n-p)

;; delete trailing whitespace before file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(show-paren-mode)
(global-subword-mode)

(set-face-attribute 'default nil :height 145)

(require 'dired-x)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)

(savehist-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (highlight-symbol-colors '("moccasin" "coral" "cyan3" "lavender" "chartreuse2"
;;                            "DarkOrange" "LightBlue2" "goldenrod2" "yellow1" "orchid"
;;                            "thistle" "LightCyan" "OliveDrab2" "RosyBrown1" "turquoise1"
;;                            "orchid1"))
;; (use-package highlight-symbol
;;   :ensure t
;;   :custom
;;   (highlight-symbol-colors '("moccasin" "OliveDrab2" "orchid1" "turquoise1" "lavender" "goldenrod2" "coral" "yellow1" ))
;;   :bind (("s-h" . highlight-symbol)
;;          ("s-n" . highlight-symbol-next)
;;          ("s-p" . highlight-symbol-prev)
;;          ("s-r" . highlight-symbol-remove-all)))

;;
(defun my-occurences-count ()
  "Show occurences count when jumping by `symbol-overlay-jump-next' and `symbol-overlay-jump-prev' and symbol is not highlighted."
  (let ((symbol (symbol-overlay-get-symbol)))
    (unless (symbol-overlay-assoc symbol) ;; Occurences of highlighted symbol are displayed by symbol-overlay
       ;; Recreated from highlight-symbol.el
      (let* ((symbol-regexp (format "\\_<%s\\_>" symbol))
             (case-fold-search nil)
             (count (how-many symbol-regexp (point-min) (point-max)))
             (current (if (= count 1) 1 ;; Don't run how-many again when there is only one occurence
                        (1+ (how-many symbol-regexp (point-min) (1- (point))))))
             message-log-max)
        (message "%s: %s/%s" symbol current count)))))

(add-hook 'symbol-overlay-jump-hook #'my-occurences-count)

(use-package symbol-overlay
  :ensure t
  :bind (("s-h" . symbol-overlay-put)
         ("s-n" . symbol-overlay-jump-next)
         ("s-p" . symbol-overlay-jump-prev)
         ("s-r" . symbol-overlay-remove-all))
  :config
  (set-face-background 'symbol-overlay-face-1 "moccasin")
  (set-face-background 'symbol-overlay-face-2 "OliveDrab2")
  (set-face-background 'symbol-overlay-face-3 "lavender")
  (set-face-background 'symbol-overlay-face-5 "goldenrod2"))

(defun my-symbol-overlay-maybe-count (orig-fun &rest args)
  "`symbol-overlay-put' shows name of the face when symbol is highlighted. This
turns it off by setting optional `show-color' param of `symbol-overlay-maybe-count' to nil"
  (unless (null (cadr args))
    (setf (cadr args) nil))
  (apply orig-fun args))

(advice-add 'symbol-overlay-maybe-count :around #'my-symbol-overlay-maybe-count)

(use-package move-dup
  :ensure t
  :bind (("M-n" . move-dup-move-lines-down)
         ("M-p" . move-dup-move-lines-up)
         ("C-c d" . move-dup-duplicate-down)))

;; Can this be replaced by build_in C-u C-SPC ?
(use-package goto-chg
  :ensure t
  :bind ("C-." . goto-last-change))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package vertico-prescient
  :ensure t
  :after vertico
  :init
  (setq
   vertico-prescient-enable-filtering nil ;;`orderless' manages the filtering part
   prescient-history-length 1000)
  ;; to make sorting and filtering more intelligent
  (vertico-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package consult
  :ensure t
  ;; :init
  ;; (setq consult-preview-key "C-.") ;; Press C-. to see the preview
  )

(defvar consult--source-sbt
  `(
    :name    "Sbt Buffer"
    :narrow   ?s
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    ;;:action   ,(lambda (arg) (message "Opened %s" arg))
    :default  t
    :items
    ,(lambda () (consult--buffer-query :sort 'visibility
                                       :as #'buffer-name
                                       :include "\\*sbt\\*")))
  "Buffer candidate source for `consult-buffer'.")

(with-eval-after-load 'consult
  (add-to-list 'consult-buffer-sources 'consult--source-sbt 'append))

(defun buffer-other-window-right ()
  (interactive)
  (when (eq 1 (count-windows))
    (split-window-right))
  (consult-buffer-other-window))

;; (keymap-global-set [remap goto-line] 'consult-goto-line)
;; (keymap-global-set (kbd "C-x b") 'consult-buffer)
;; (keymap-global-set (kbd "C-x n") 'buffer-other-window-right)

(defun my/comment-dwim (args)
  "Comments or uncomments the current line or region."
  (interactive "*P")
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))

(use-package emacs
  :bind (("C-c f" . recentf-open)
         ("M-o" . other-window) ;; (add-hook 'sgml-mode-hook (lambda () (keymap-set html-mode-map "\M-o" nil)))
         ("s-s" . replace-string)
         ("s-W" . toggle-truncate-lines)
         ("C-M-j" . join-line)
         ("C-C e" . next-error)
         ("C-x n" . buffer-other-window-right)
         ("M-;" . my/comment-dwim)
         ([remap set-goal-column] . list-buffers) ;; C-x C-n
         ([remap list-buffers] . consult-project-buffer) ;; C-x C-b
         ([remap switch-to-buffer] . consult-buffer) ;; C-x b
         ([remap goto-line] . consult-goto-line)
         ([remap move-beginning-of-line] . my/smarter-move-beginning-of-line))
  :custom
  (revert-without-query '(".*.scala$" ".*.ts$" ".*.tsx$")) ;; Revert scala files without asking.
  (next-error-verbose nil)
  (dired-use-ls-dired nil) ;; ls on macos doesn't support --dired flag
  :init
  ;;(load-theme 'modus-operandi-tinted t)
  ;;(load-theme 'modus-vivendi-tinted t)
  (load-theme 'ef-melissa-dark t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package webjump
  :bind ("C-c j" . webjump)
  :custom
  (webjump-sites '(("Silicon /debug - local" . "http://localhost:8902/debug")
                   ("Silicon /debug - qa" . "https://silicon.qa.oasvc.itv.com/debug")
                   ("Silicon /debug - prd" . "https://silicon.prd.oasvc.itv.com/debug")
                   ("Sunnydale /status - qa" . "https://sunnydale.qa.oasvc.itv.com/status")
                   ("Sunnydale /status - prd" . "https://sunnydale.prd.oasvc.itv.com/status")
                   ("pulls silicon github" . "https://github.com/ITV/silicon/pulls")
                   ("pulls gform github" . "https://github.com/hmrc/gform/pulls")
                   ("pulls gform-frontend github" . "https://github.com/hmrc/gform-frontend/pulls")
                   ("pulls gform-builder-extension github" . "https://github.com/hmrc/gform-builder-extension/pulls")
                   ("pulls gform-builder-extension-tests github" . "https://github.com/hmrc/gform-builder-extension-tests/pulls")
                   ("pulls eeitt-admin-frontend github" . "https://github.com/hmrc/eeitt-admin-frontend/pulls"))))

(use-package transpose-frame
  :ensure t
  :bind (:map ctl-x-4-map
              ("t" . transpose-frame)))

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package inf-mongo
  :ensure t
  :custom
  (inf-mongo-command "docker exec -it gform-frontend-mongo-1 mongo"))
;;(setq inf-mongo-command "docker exec -it gform-frontend-mongo-1 mongo")

(use-package term
  :bind (:map term-mode-map
              ("C-c C-v" . comint-clear-buffer)))

(use-package winner
  :bind
  (("C-s-p" . winner-undo) ;; Control + Shift + Cmd + p
   ("C-s-n" . winner-redo)) ;; Control + Shift + Cmd + n
  :config
  (winner-mode 1))

(use-package zygospore
  :ensure t
  :bind
  (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package mhtml-mode
  :bind (:map html-mode-map
              ("M-o" . other-window)))

(use-package diff
  :bind (:map diff-mode-map
              ([remap diff-goto-source] . other-window)))

(use-package yaml-ts-mode)

(defun my-compilation-mode-hook ()
  (cond ((and
          (fboundp 'cargo-mode--project-directory)
          (cargo-mode--project-directory))
         (cargo-minor-mode))))

(use-package compile
  :bind (("M-O"   . show-compilation))
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :hook ((compilation-filter . compilation-ansi-color-process-output)
         (compilation-mode . my-compilation-mode-hook))
  :custom
  (compilation-always-kill t) ;; Convenient for rust GUI development
  (compilation-auto-jump-to-first-error 'first-known)
  ;;(compilation-auto-jump-to-first-error nil)
  (compilation-ask-about-save nil)
  (compilation-context-lines nil)
  (compilation-scroll-output 'first-error)
  (compilation-skip-threshold 2)
  ;;;(compilation-window-height 100)
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker))))

(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)
         ("s-m" . magit-status)
         ("s-b" . magit-blame))
  :config
  ;; Show m option in `project-switch-project'
  (require 'magit-extras))

(use-package project
  :init
  :bind ("s-f" . project-find-file))

(use-package feature-mode
  :init
  (require 'org-table)
  :ensure t
  :hook (feature-mode . cargo-minor-mode)
  :config
  ;; Since When and Then keywords are so similar I prefer for them to have a distinct colors
  ;; TODO font-lock-add-keywords should be enought if `set' is used as last param, check the documentation
  (font-lock-remove-keywords 'feature-mode '(("^ *\\(When\\) " . font-lock-keyword-face)))
  (font-lock-add-keywords 'feature-mode '(("^ *\\(When\\) " . 'font-lock-function-name-face))))

(use-package treesit
  :custom
  (major-mode-remap-alist '((js-json-mode . json-ts-mode)))
  (treesit-font-lock-level 4))

(use-package scala-ts-mode
  :demand t ;; For some reason this needs to be loaded immediately
  :bind (:map scala-ts-mode-map
              ("s-i" . search-import)
              ("C-c C-k" . save-buffer)
              ;;([remap sp-previous-sexp] . mode-line-other-buffer)
              ([remap sp-next-sexp] . println-insert-after))
  :load-path "/Users/pepa/develop-emacs/scala-ts-mode/")

;;;; taken from https://github.com/bbatsov/prelude/blob/05dc795f2befb192f6ab16ef66fbb632ca2e3189/core/prelude-core.el#L138
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1)))

  (if-let ((minibufferp (current-buffer))
           (face-at-point (get-text-property (point) 'face)))
      (when (and (listp face-at-point)
                 (string= "consult-async-split" (face-name (car face-at-point))))
        (forward-char))))

(use-package hydra
  :ensure t)

(use-package sbt-mode
  :load-path "/Users/pepa/develop-emacs/emacs-sbt-mode/"
  :hook (sbt-mode . compilation-minor-mode)
  :bind (("C-c v" . sbt-hydra)
         :map sbt:mode-map
         ("C-c C-f" . next-error-follow-minor-mode))
  :init
  (add-hook 'sbt-mode-hook (lambda ()
        		     (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)
                             )))

(use-package smartparens
  ;;:ensure t
  :load-path "/Users/pepa/develop-emacs/smartparens/"
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode t)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "C-s-{")
  (sp-pair "{" "}" :wrap "C-{")
  (sp-pair "<" ">" :wrap "C-<")
  (keymap-global-set "s-[" 'sp-backward-unwrap-sexp)
  (keymap-global-set "s-]" 'sp-unwrap-sexp)
  (keymap-global-set "s-{" 'sp-rewrap-sexp)
  (keymap-set smartparens-mode-map "M-<backspace>" nil)
  :bind ([remap sp-previous-sexp] . mode-line-other-buffer))

(use-package crux
  :ensure t
  :bind (("C-c D" . crux-duplicate-and-comment-current-line-or-region) ;; I need to start use this
         ("M-j" . crux-smart-open-line-above)
         ("C-k" . crux-smart-kill-line)))

(use-package just-mode
  :ensure t)

(defun ripgrep-for-thing-at-point ()
  (interactive)
  (cond ((or
          (derived-mode-p 'scala-ts-mode)
          (derived-mode-p 'rust-ts-mode)
          (derived-mode-p 'emacs-lisp-mode))
         (consult-ripgrep nil (if (use-region-p)
                                  (buffer-substring-no-properties (region-beginning) (region-end))
                                (substring-no-properties (or (thing-at-point 'symbol) "")))))
        (t (consult-ripgrep nil (when (use-region-p)
                                  (buffer-substring-no-properties (region-beginning) (region-end)))))))

(bind-key "s-F" 'ripgrep-for-thing-at-point)

(use-package ef-themes
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package iedit
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package info-colors
  :ensure t
  :init
  (add-hook 'Info-selection-hook #'info-colors-fontify-node))

(use-package sbt-switcher
  :load-path "/Users/pepa/develop-emacs/sbt-switcher/"
  :bind ("C-x 4 s" . sbt-switcher:sbt))

;; Use package-vc-install instead of load-path
(use-package scala-import
  :load-path "/Users/pepa/develop-emacs/scala-import/")

(defun run-just-check ()
  (interactive)
  (compile "just check" nil))

(defun run-just-dist ()
  (interactive)
  (compile "just dist" nil))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :bind (
         :map typescript-ts-mode-map ;; typescript-ts-mode-map only exists after the package is loaded
         ("C-c C-k" . run-just-check)
         ("C-c C-r" . run-just-dist)
         ("C-M-S-p" . beginning-of-defun)
         ("C-M-S-n" . end-of-defun)
         ([remap sp-next-sexp] . println-insert-after)
         :map tsx-ts-mode-map  ;; tsx-ts-mode-map only exists after the package is loaded
         ("C-c C-k" . run-just-check)
         ("C-c C-r" . run-just-dist)
         ("C-M-S-p" . beginning-of-defun)
         ("C-M-S-n" . end-of-defun)
         ([remap sp-next-sexp] . println-insert-after)))

;; Cherry pick from rust-mode just these:
(load-file "/Users/pepa/develop-emacs/rust-mode/rust-compile.el")
(load-file "/Users/pepa/develop-emacs/rust-mode/rust-rustfmt.el")

(defun my-format-rust-buffer-on-save ()
  (setq-local rust-format-on-save t)
  (add-hook 'before-save-hook #'rust-before-save-method nil 'local)
  (add-hook 'after-save-hook #'rust-after-save-method nil 'local))

;; I think I had a problem with compilation-search-path not to be set properly, but compilation buffer works fine now
(defun run-with-rust ()
  "Set compilation-search-path for rust project"
  (setq-local compilation-search-path
              (project-name (project-current))))

(use-package rust-ts-mode
  :mode (("\\.rs\\'" . rust-ts-mode))
  :bind (:map rust-ts-mode-map
              ("C-M-S-p" . beginning-of-defun)
              ("C-M-S-n" . end-of-defun)
              ([remap sp-next-sexp] . println-insert-after))
  :config
  (add-hook 'rust-ts-mode-hook #'my-format-rust-buffer-on-save)
  ;;(add-hook 'rust-ts-mode-hook #'run-with-rust)
  (add-hook 'rust-ts-mode-hook #'cargo-minor-mode))

(use-package cargo-mode
  :load-path "/Users/pepa/develop-emacs/cargo-mode/"
  :custom
  (cargo-mode-use-comint nil)
  (keymap-unset cargo-minor-mode-map "C-q e") ;; Forget original binding
  :bind (:map cargo-minor-mode-map
              ("C-c C-e" . cargo-mode-execute-task)
              ("C-c C-b" . cargo-mode-build)
              ("C-c C-k" . cargo-mode-check)
              ("C-c C-r" . cargo-mode-run)
              ("C-c C-c" . cargo-mode-last-command)
              ("C-c C-l" . cargo-mode-run-clippy)
              ("C-c C-a" . cargo-mode-test)
              ("C-c C-o" . cargo-mode-test-current-buffer)
              ("C-c C-t" . cargo-mode-test-current-test)))

;;(use-package ansi-color)

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package s
  :ensure t)

(use-package restclient
  :ensure t)

(load-file "~/.dotfiles/.config/emacs-profiles/29.2/defuns.el")
(load-file "~/.dotfiles/.config/emacs-profiles/29.2/json-uploader.el")
(load-file "~/.dotfiles/.config/emacs-profiles/29.2/hbs-uploader.el")
(load-file "~/.dotfiles/.config/emacs-profiles/29.2/converters.el")
(load-file "~/.dotfiles/.config/emacs-profiles/29.2/my-modeline.el")
(load-file "~/.dotfiles/.config/emacs-profiles/29.2/minibuffer-hydras.el")

(defun upload-template-or-handlebar (prefix-arg)
  (interactive "p")
  (if (string= "hbs" (file-name-extension (buffer-file-name)))
      (upload-handlebarstemplates prefix-arg)
    (upload-template prefix-arg)))

(defun my-format-json-buffer-on-save ()
  (add-hook 'after-save-hook #'json-pretty-print-buffer nil 'local))

(use-package json-ts-mode
  :bind (:map json-ts-mode-map
              ("C-c C-c" . upload-template-or-handlebar)
              ("C-c C-o" . open-template-in-browser))
  :config
  (add-hook 'json-ts-mode-hook #'my-format-json-buffer-on-save))

(use-package emacs-lisp-mode
  :bind (:map emacs-lisp-mode-map
              ([remap sp-next-sexp] . println-insert-after)))

(use-package println-debugger
  :load-path "/Users/pepa/develop-emacs/println-debugger/")

(defalias 'my-macro-wrap-in-single-quotes-with-pipe-at-the-end
  (kmacro "<tab> C-SPC C-M-f ' | C-d C-n C-a"))

(defalias 'my-macro-rust-define-from-str-branch
  (kmacro "C-SPC C-M-f M-w C-SPC C-a \" C-SPC C-M-f ' C-f SPC = > SPC S e l f : : C-y C-n C-a"))

;; (defun search-docs-rs ()
;;   (interactive)
;;   (let ((search-term (buffer-substring (region-beginning) (region-end))))
;;     (if (use-region-p)
;;         (browse-url (format "https://docs.rs/releases/search?query=%s" search-term))
;;       (message "No active region. Searching on https://docs.rs needs active region"))
;;     ))

(defun search-rust-docs ()
  (interactive)
  (let ((search-term (buffer-substring (region-beginning) (region-end))))
    (if (use-region-p)
        (browse-url (format "https://doc.rust-lang.org/beta/std/index.html?search=%s" search-term))
      (message "No active region. Searching on https://doc.rust-lang.org needs active region"))))

;;(global-set-key "\C-cj" 'search-docs-rs)

(global-set-key (kbd "C-c C-j") 'search-rust-docs)

(with-eval-after-load 'rust-ts-mode
  (define-abbrev rust-ts-mode-abbrev-table "val" "let"))
(with-eval-after-load 'scala-ts-mode
  (define-abbrev scala-ts-mode-abbrev-table "let" "val"))

(add-hook 'rust-ts-mode-hook 'abbrev-mode)
(add-hook 'scala-ts-mode-hook 'abbrev-mode)
(add-hook 'sql-mode-hook 'abbrev-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-search-path '("/Users/pepa/develop-hmrc/gform-builder-extension/" nil))
 '(custom-safe-themes
   '("01aef17f41edea53c665cb57320bd80393761f836be5ab0bd53292afc94bd14d" "91fed8cade0d85d1f5a5621e72ac6e720945be79493465a79f020e673f7e2d24" "88267200889975d801f6c667128301af0bc183f3450c4b86138bfb23e8a78fb1" "205bb9accecaf0ae5e8cb5f09773be4f1175aca71322ba4fd44b539cd48463b6" "4c326abbf8b85c85e114691d3892cbbfe889b2b064dadd284cf5eccca3eecbff" "c49f79bfea991ed384cc0dc33328c8d812c413bf27cff0bc24ad58af2cdbccb4" "73c55f5fd22b6fd44f1979b6374ca7cc0a1614ee8ca5d4f1366a0f67da255627" "a6a979c8b7ccb1d4536f4fa74a6e47674a3ce65feea3fecdf1d9dc448fac47e0" "65809263a533c5151d522570b419f1a653bfd8fb97e85166cf4278e38c39e00e" "5f92b9fc442528b6f106eaefa18bb5e7bfa0d737164e18f1214410fef2a6678d" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" "3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3" "58264887d7ab17702ef85bbd96e11bd7f613622ff9c63990be860b958c978f09" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" default))
 '(package-selected-packages
   '(symbol-overlay symbo-overlay just-mode transpose-frame feature-mode feature marginalia zygospore inf-mongo ef-themes markdown-mode restclient rest-client modus-themes))
 '(safe-local-variable-values
   '((magit-revision-insert-related-refs)
     (magit-diff-highlight-hunk-body)
     (magit-diff-highlight-indentation)))
 '(sql-connection-alist
   '(("Muninn Local"
      (sql-product 'postgres)
      (sql-user "muninn-app")
      (sql-database "muninn")
      (sql-server "localhost")
      (sql-port 5432))
     ("Muninn QA"
      (sql-product 'postgres)
      (sql-user "muninn-app")
      (sql-database "muninn")
      (sql-server "db-pg-muninn.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Muninn PRD"
      (sql-product 'postgres)
      (sql-user "muninn-app")
      (sql-database "muninn")
      (sql-server "db-pg-muninn.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Rindr Local"
      (sql-product 'postgres)
      (sql-user "rindr")
      (sql-database "test")
      (sql-server "localhost")
      (sql-port 5434))
     ("Rindr QA"
      (sql-product 'postgres)
      (sql-user "rindr-app")
      (sql-database "rindr")
      (sql-server "db-pg-rindr.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Rindr PRD"
      (sql-product 'postgres)
      (sql-user "rindr-app")
      (sql-database "rindr")
      (sql-server "db-pg-rindr.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Heimdall Local"
      (sql-product 'postgres)
      (sql-user "heimdall-app")
      (sql-database "heimdall")
      (sql-server "localhost")
      (sql-port 5432))
     ("Heimdall QA"
      (sql-product 'postgres)
      (sql-user "heimdall-app")
      (sql-database "heimdall")
      (sql-server "db-pg-heimdall.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Heimdall PRD"
      (sql-product 'postgres)
      (sql-user "heimdall-app")
      (sql-database "heimdall")
      (sql-server "db-pg-heimdall.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Silicon Local"
      (sql-product 'postgres)
      (sql-user "silicon-app")
      (sql-database "silicon")
      (sql-server "localhost")
      (sql-port 5432))
     ("Silicon QA"
      (sql-product 'postgres)
      (sql-user "silicon-app")
      (sql-database "silicon")
      (sql-server "db-pg-shared12.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Silicon PRD"
      (sql-product 'postgres)
      (sql-user "silicon-app")
      (sql-database "silicon")
      (sql-server "db-pg-shared12.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Jeffrey local"
      (sql-product 'postgres)
      (sql-user "jeffrey")
      (sql-database "jeffrey")
      (sql-server "localhost")
      (sql-port 5432))
     ("Lisa local"
      (sql-product 'postgres)
      (sql-user "lisa-app")
      (sql-database "lisa")
      (sql-server "localhost")
      (sql-port 5432))
     ("Lisa QA"
      (sql-product 'postgres)
      (sql-user "lisa-app")
      (sql-database "lisa")
      (sql-server "db-pg-lisa.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Lisa PRD"
      (sql-product 'postgres)
      (sql-user "lisa-app")
      (sql-database "lisa")
      (sql-server "db-pg-lisa.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Davinci local"
      (sql-product 'postgres)
      (sql-user "davinci")
      (sql-database "davinci")
      (sql-server "localhost")
      (sql-port 5432))
     ("Zeus local"
      (sql-product 'postgres)
      (sql-user "zeus-app")
      (sql-database "zeus")
      (sql-server "localhost")
      (sql-port 5432))
     ("Zeus QA"
      (sql-product 'postgres)
      (sql-user "zeus-app")
      (sql-database "zeus")
      (sql-server "db-pg-zeus.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Zeus PRD"
      (sql-product 'postgres)
      (sql-user "zeus-app")
      (sql-database "zeus")
      (sql-server "db-pg-zeus.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Branding Metadata local"
      (sql-product 'postgres)
      (sql-user "branding-metadata-app")
      (sql-database "branding-metadata")
      (sql-server "localhost")
      (sql-port 5432))
     ("Branding Metadata QA"
      (sql-product 'postgres)
      (sql-user "branding-metadata-app")
      (sql-database "branding-metadata")
      (sql-server "db-pg-brandingmetadata.qa.oasvc.itv.com")
      (sql-port 5432))
     ("Branding Metadata PRD"
      (sql-product 'postgres)
      (sql-user "branding-metadata-app")
      (sql-database "branding-metadata")
      (sql-server "db-pg-brandingmetadata.prd.oasvc.itv.com")
      (sql-port 5432))
     ("Vindler local"
      (sql-product 'postgres)
      (sql-user "vindler-app")
      (sql-database "vindler")
      (sql-server "localhost")
      (sql-port 5432)))))
(put 'narrow-to-region 'disabled nil)
