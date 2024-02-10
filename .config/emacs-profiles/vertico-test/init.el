(setq debug-on-error nil)

(set-register ?o "d")
(set-register ?i "crate::logging::log(&format!(\": {:?}\", ));")
(set-register ?p "println!(\"{}\",);")

(tool-bar-mode -1)

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
 initial-buffer-choice "~/.dotfiles/.config/emacs-profiles/vertico-test/init.el"
 isearch-lazy-count t
 vc-follow-symlinks t
 straight-vc-git-default-protocol 'ssh
 js-indent-level 2
 css-indent-offset 2
 ediff-window-setup-function 'ediff-setup-windows-plain ;; Do not display Ediff control window in separate frame
 initial-major-mode 'emacs-lisp-mode
 org-hide-emphasis-markers t
 backup-directory-alist '(("." . "~/.saves"))
 compilation-always-kill t ;; Convenient for rust GUI development
 )

(require 'org-tempo) ;; typing <s TAB creates a code block

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;;(toggle-frame-fullscreen)
(scroll-bar-mode -1)
(global-set-key (kbd "M-o") 'other-window)
(add-hook 'sgml-mode-hook (lambda () (define-key html-mode-map "\M-o" nil)))
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-W") 'toggle-truncate-lines)
(global-set-key (kbd "C-M-j") 'join-line)
(global-set-key (kbd "C-c e" ) 'next-error)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; delete trailing whitespace before file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(electric-pair-mode)

(show-paren-mode)
(global-subword-mode)

(set-face-attribute 'default nil :height 145)

(require 'dired-x)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)

(savehist-mode)

(straight-use-package 'highlight-symbol)
(global-set-key (kbd "s-h") 'highlight-symbol)
(global-set-key (kbd "s-n") 'highlight-symbol-next)
(global-set-key (kbd "s-p") 'highlight-symbol-prev)
(global-set-key (kbd "s-r") 'highlight-symbol-remove-all)

;; Testing setting below

;; (straight-use-package 'vertico)
;; (vertico-mode)
;; (define-key vertico-map "?" #'minibuffer-completion-help)

(straight-use-package 'vertico)
(vertico-mode +1)

(straight-use-package 'vertico-prescient)
(setq prescient-history-length 1000)
;; to make sorting and filtering more intelligent
(vertico-prescient-mode +1)
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)

;;(global-set-key (kbd "C-x C-z") 'selectrum-repeat)

;;(straight-use-package 'orderless)

;;(setq completion-styles '(orderless))

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "s-f") 'projectile-find-file)

(straight-use-package 'magit)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-m") 'magit-status)
(global-set-key (kbd "s-b") 'magit-blame)

(straight-use-package 'crux)
(define-key global-map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(define-key global-map (kbd "C-c f") 'crux-recentf-find-file)
(define-key global-map (kbd "M-j") 'crux-smart-open-line-above)

(straight-use-package 'consult)
;;(require 'consult)

(setq consult-preview-key "" ;; Press C-x to see the preview
      consult-project-root-function #'projectile-project-root)
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x n") 'buffer-other-window-right)
;; (consult-customize
;;  consult-ripgrep :group nil)
;; (consult-customize
;;  consult-ripgrep :group #'consult--grep-group)
;;(set-face-attribute 'selectrum-group-title nil :foreground "RosyBrown3")


(set-face-attribute 'show-paren-match nil :foreground "red" :background nil)


;;(set-face-attribute 'selectrum-group-separator nil :foreground "cornflower blue")

;; (set-face-attribute 'consult-file nil :height 155)
;; (set-face-attribute 'consult-buffer nil :height 155)
;; (set-face-attribute 'consult-preview-match nil :height 155)
;; (set-face-attribute 'consult-preview-line nil :height 55)
;; (set-face-attribute 'consult-grep-context nil :height 155)
;; (set-face-attribute 'consult-help nil :height 155)
;;(set-face-attribute 'shadow nil :height 255)

(straight-use-package
 '(restclient :type git :host github :repo "VlachJosef/restclient.el" :branch "master"))
(add-hook 'restclient-mode-hook (lambda () (smartparens-mode)))
(setq restclient-log-request nil)
(require 'restclient)

(straight-use-package 'hydra)
(straight-use-package 'scala-mode)
;;(straight-use-package
 ;;'(ensime-mode :type git :host github :repo "ensime/ensime-tng"))

;;(require 'ensime-mode)
;;(define-key ensime-mode-map (kbd "M-.") 'ensime-jump-to-definition)


(require 'scala-mode)

(define-key scala-mode-map (kbd "C-M-{") 'scala-wrap-line-in-curly-braces) ;; Control + Shift + Alt + {
(define-key scala-mode-map (kbd "C-c C-k") 'save-buffer) ;; Follow rust-mode

(defun scala-wrap-line-in-curly-braces()
  (interactive)
  (move-end-of-line 1)
  (push-mark)
  (move-beginning-of-line 1)
  (back-to-indentation)
  (execute-kbd-macro "{"))

(add-hook 'scala-mode-hook (lambda ()
			     (auto-revert-mode)
			     (smartparens-mode)))

(add-hook 'jsq-mode-hook (lambda ()
			  (smartparens-mode)))

(straight-use-package
 '(sbt-mode :type git :host github :repo "VlachJosef/emacs-sbt-mode" :branch "jv/reStart"))

(require 'sbt-mode-hydra)
(global-set-key (kbd "C-c v") 'sbt-hydra)

(straight-use-package
 '(scala-import :type git :host github :repo "VlachJosef/scala-import"))

(require 'scala-import)

(straight-use-package
 '(sbt-switcher :type git :host github :repo "VlachJosef/sbt-switcher"))

(require 'sbt-switcher)
(global-set-key (kbd "C-x 4 s") 'sbt-switcher:sbt)

;; TODO remove dependency on ivy
;; (straight-use-package
;;  '(sbt-test-runner :type git :host github :repo "VlachJosef/sbt-test-runner.el"))

;; (add-to-list 'load-path "~/develop-emacs/sbt-test-runner.el/")

;;(require 'sbt-test-munit)

(setq sbt:sbt-prompt-regexp "^>[ ]+")

(require 'cl-macs)
(defun clear-sbt-compilation-buffer ()
  (message "(clear-sbt-compilation-buffer message):" )

  (let ((current-sbt-root (sbt:find-root)))
    (cl-loop for process being the elements of (process-list)
             for current-process-buffer = (process-buffer process)
             if (and
		 (buffer-live-p current-process-buffer)
		 (bufferp current-process-buffer) ;; process must have associated buffer
		 (with-current-buffer current-process-buffer
                   (and
                    (sbt:mode-p)
                    (string= (sbt:find-root) current-sbt-root))))
             do (progn
                  (with-current-buffer current-process-buffer
                    (let ((inhibit-read-only t))
                      (ignore-errors (compilation-forget-errors))
                      (erase-buffer)))
                  ;; sbt-clear is blocking emacs 28 when running Play framework. This is reproduction code: (comint-send-string (get-buffer-process (current-buffer)) (kbd "C-l"))
  		  ;;(sbt:clear current-process-buffer)
                  ))))

(add-hook 'sbt-mode-hook (lambda ()
			   (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)
			   (add-hook 'before-save-hook 'clear-sbt-compilation-buffer)))

(add-hook 'minibuffer-setup-hook 'mini-hook)

(straight-use-package 'smartparens)
(require 'smartparens-config)
(sp-use-smartparens-bindings)
(sp-pair "(" ")" :wrap "C-(")
(sp-pair "[" "]" :wrap "C-s-{")
(sp-pair "{" "}" :wrap "C-{")
(global-set-key [remap kill-sexp] 'sp-kill-sexp)
(global-set-key (kbd "s-[") 'sp-backward-unwrap-sexp)
(global-set-key (kbd "s-]") 'sp-unwrap-sexp)
(global-set-key (kbd "s-{") 'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") nil)

(add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-mode)))

(straight-use-package 'goto-chg)
(global-set-key (kbd "C-.") 'goto-last-change)

;; (straight-use-package 'zerodark-theme)
;; (load-theme 'zerodark t)
;; (straight-use-package 'zenburn-theme)
;; (load-theme 'zenburn t)

(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox-dark-medium t)


;; (let ((class '((class color) (min-colors 89))))
;;   (custom-theme-set-faces
;;    'zerodark
;;    `(selectrum-current-candidate
;;      ((,class (:background "#48384c"
;;                            :weight bold
;;                            :foreground "#c678dd"))))
;;    `(selectrum-prescient-primary-highlight
;;    ((,class (:foreground "green"))))
;;    `(selectrum-prescient-secondary-highlight
;;    ((,class (:foreground "red"))))))

;; (enable-theme 'zerodark)

;;(straight-use-package 'which-key)
;;(which-key-mode)

(straight-use-package 's)
(require 's)
(straight-use-package 'request)
(straight-use-package 'graphql-mode)
(add-hook 'graphql-mode-hook (lambda () (smartparens-mode)))


(straight-use-package 'transpose-frame)
(define-key ctl-x-4-map "t" 'transpose-frame)

(straight-use-package
 '(json-mode :type git :host github :repo "VlachJosef/json-mode"))
(require 'json-mode)
(add-hook 'json-mode-hook (lambda () (smartparens-mode)))
(define-key json-mode-map (kbd "C-c C-c") 'upload-template-or-handlebar)
(define-key json-mode-map (kbd "C-c C-o") 'open-template-in-browser)

(straight-use-package 'markdown-mode)
(straight-use-package 'iedit)
(require 'iedit)
(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(define-key json-mode-map (kbd "C-c C-m")       'pipa-toggle-timestamp-and-iso-time)
(define-key restclient-mode-map (kbd "C-c C-m") 'pipa-toggle-timestamp-and-iso-time)

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
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line] 'my/smarter-move-beginning-of-line)

(defun look-for-thing-at-point-2 ()
  (interactive)
  (cond ((or
          (derived-mode-p 'scala-mode)
          (derived-mode-p 'rust-mode)
          (derived-mode-p 'emacs-lisp-mode))
         (consult-ripgrep nil (projectile-symbol-or-selection-at-point)))
        (t (consult-ripgrep nil (when (use-region-p)
                                  (buffer-substring-no-properties (region-beginning) (region-end)))))))

;;(global-set-key (kbd "s-F") 'consult-ripgrep)
(global-set-key (kbd "s-F") 'look-for-thing-at-point-2)
(define-key minibuffer-local-map (kbd "s-F") 'in-minibuffer-show-hydra)

(straight-use-package 'info-colors)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(straight-use-package
 '(println-debugger :type git :host github :repo "VlachJosef/println-debugger"))

;; (require 'println-debugger) ;; run (eval-buffer) in /Users/pepa/.config/emacs-profiles/vertico-test/straight/repos/println-debugger/println-debugger-common.el to fix this

;; (define-key emacs-lisp-mode-map [remap sp-previous-sexp] 'println-insert-before)
;; (define-key emacs-lisp-mode-map [remap sp-next-sexp] 'println-insert-after)
;; (define-key emacs-lisp-mode-map (kbd "C-M-p") 'println-insert-before)
;; (define-key emacs-lisp-mode-map (kbd "C-M-n") 'println-insert-after)

;; (define-key scala-mode-map [remap sp-previous-sexp] 'println-insert-before)
;; (define-key scala-mode-map [remap sp-next-sexp] 'println-insert-after)
;; (define-key scala-mode-map (kbd "C-M-p") 'println-insert-before)
;; (define-key scala-mode-map (kbd "C-M-n") 'println-insert-after)

(define-key scala-mode-map (kbd "C-c C-b") 'restclient:repeat-last-call)

(define-key sbt:mode-map (kbd "C-c C-b") 'restclient:repeat-last-call)

(straight-use-package 'ace-jump-zap)
(global-set-key (kbd "C-z") 'ace-jump-zap-up-to-char)

(straight-use-package
 '(git-timemachine :type git :host github :repo "emacsmirror/git-timemachine"))

(straight-use-package 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key ibuffer-mode-map (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(straight-use-package 'zygospore)

(winner-mode 1)
(global-set-key (kbd "C-s-p") 'winner-undo) ;; Control + Shift + Cmd + p
(global-set-key (kbd "C-s-b") 'winner-undo-2) ;; Control + Shift + Cmd + b
(global-set-key (kbd "C-s-n") 'winner-redo) ;; Control + Shift + Cmd + n

(straight-use-package 'burly)
(global-set-key (kbd "C-s-s") 'burly-bookmark-windows)   ;; Control + Shift + Cmd + s
(global-set-key (kbd "C-s-l") 'burly-open-last-bookmark) ;; Control + Shift + Cmd + l
(global-set-key (kbd "C-s-o") 'consult-bookmark)         ;; Control + Shift + Cmd + o

(straight-use-package 'move-dup)
(global-set-key (kbd "M-n") 'move-dup-move-lines-down)
(global-set-key (kbd "M-p") 'move-dup-move-lines-up)
(global-set-key (kbd "C-c d") 'move-dup-duplicate-down)

;;(straight-use-package 'docker)

(straight-use-package 'yaml-mode)

(straight-use-package 'dhall-mode)
(straight-use-package 'elm-mode)
(straight-use-package 'just-mode)
(straight-use-package 'glsl-mode)
(require 'glsl-mode)
(define-key glsl-mode-map (kbd "<backtab>") 'ff-find-other-file) ;; Allow Shift-Tab to switch between *.vert and *.frag shaders
(add-hook 'glsl-mode-hook
          (lambda ()
            (smartparens-mode)
            ))

(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (setq rust-format-on-save t)
            (setq-local compilation-ask-about-save nil) ;; Found in https://github.com/brotzeit/rustic
            (smartparens-mode)
            ))

(require 'rust-mode)
(define-key rust-mode-map (kbd "C-c C-k") 'rust-check)
(define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-l") 'rust-run-clippy)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)

(straight-use-package 'typescript-mode)
(add-hook 'typescript-mode-hook
          (lambda ()
            (setq-local compilation-ask-about-save nil) ;; Found in https://github.com/brotzeit/rustic
            (setq typescript-indent-level 2)
            (smartparens-mode)))

(require 'typescript-mode)
(define-key typescript-mode-map (kbd "C-c C-k") 'run-just-check)
(define-key typescript-mode-map (kbd "C-c C-r") 'run-just-dist)

(defun run-just-check ()
  (interactive)
  (message "compilation-search-path: %s" compilation-search-path)
  (compile "just check" nil))

(defun run-just-dist ()
  (interactive)
  (compile "just dist" nil))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(straight-use-package 'terraform-mode)

;; Testing ensime-ng
;; (straight-use-package 'company-mode)
;; (straight-use-package 'popup)

;; (straight-use-package 'marginalia)
;; (marginalia-mode)
;; (define-key minibuffer-local-map (kbd "s-8") 'marginalia-cycle)

;; (straight-use-package 'prettier-js)
;; (add-hook 'js-mode-hook
;;             (lambda ()
;;               (prettier-js-mode)))

(straight-use-package 'inf-mongo)
;;(setq inf-mongo-command "/Users/pepa/develop-sensible/mongodb-osx-x86_64-3.2.7/bin/mongo 127.0.0.1:27017")
(setq inf-mongo-command "docker exec -it gform-frontend-mongo-1 mongo")
(add-hook 'inf-mongo-mode-hook (lambda () (smartparens-mode)))

(defun buffer-other-window-right ()
  (interactive)
  (when (eq 1 (count-windows))
    (split-window-right))
  (consult-buffer-other-window))


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

(with-eval-after-load 'magit
  (set-face-attribute 'magit-diff-context-highlight nil :background "gray10")
  (set-face-attribute 'magit-branch-current nil :underline nil :box '(:line-width 1 :color "#83a598")))

(with-eval-after-load 'ediff
  (set-face-background 'ediff-current-diff-A "dark red")
  (set-face-background 'ediff-current-diff-B "green4")
  (set-face-background 'ediff-current-diff-C "purple4")
  (set-face-background 'ediff-fine-diff-A "red1")
  (set-face-foreground 'ediff-fine-diff-B "black")
  (set-face-background 'ediff-fine-diff-B "green2")
  (set-face-background 'ediff-fine-diff-C "dark violet"))

(set-face-foreground 'link "#fabd2f")

(with-eval-after-load 'org
  (set-face-foreground 'org-verbatim "#83a598"))

(with-eval-after-load 'ansi-color ;; Needs emacs 28
  (set-face-foreground 'ansi-color-blue "#83a598"))

;; (straight-use-package 'embark)
;; (define-key global-map (kbd "s-.") 'embark-act)

(load "~/.dotfiles/.emacs.d/personal/defuns.el")

(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (straight-use-package 'embark-consult)
    (require 'embark-consult)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pdf-view-midnight-colors '("#fdf4c1" . "#32302f"))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (magit-revision-insert-related-refs)
     (magit-diff-highlight-hunk-body)
     (magit-diff-highlight-indentation))))

;;'(magit-branch-current ((t (:box (:line-width 1 :color "#83a598")))))
;;'(magit-branch-current ((t (:box (:line-width 1 :color "#83a598") :underline nil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "dark gray"))))
 '(show-paren-match ((t (:foreground "red" :background nil :weight bold)))))


(global-set-key (kbd "C-M-s-p") 'itv-convert-identifier)

(straight-use-package 'wgsl-mode)

(defun search-docs-rs ()
  (interactive)
  (let ((search-term (buffer-substring (region-beginning) (region-end))))
    (if (use-region-p)
        (browse-url (format "https://docs.rs/releases/search?query=%s" search-term))
      (message "No active region. Searching on https://docs.rs needs active region"))
    ))

(defun search-rust-docs ()
  (interactive)
  (let ((search-term (buffer-substring (region-beginning) (region-end))))
    (if (use-region-p)
        (browse-url (format "https://doc.rust-lang.org/beta/std/index.html?search=%s" search-term))
      (message "No active region. Searching on https://doc.rust-lang.org needs active region"))))

(global-set-key "\C-cj" 'search-docs-rs)

(global-set-key (kbd "C-c C-j") 'search-rust-docs)

(add-hook 'conf-toml-mode-hook (lambda () (smartparens-mode)))
