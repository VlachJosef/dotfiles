(set-register ?i "g")
(set-register ?o "d")

(setq-default indent-tabs-mode nil)
(setq
 initial-scratch-message nil
 column-number-mode t
 dabbrev-case-fold-search nil
 kill-do-not-save-duplicates t
 dired-dwim-target t
 dired-listing-switches "-alo"
 frame-resize-pixelwise t
 ring-bell-function 'ignore
 scroll-error-top-bottom t
 scroll-preserve-screen-position t
 inhibit-startup-screen t
 initial-buffer-choice "~/.dotfiles/.config/emacs-profiles/bare/init.el"
 isearch-lazy-count t
 vc-follow-symlinks t
 straight-vc-git-default-protocol 'ssh
 js-indent-level 2
 ediff-window-setup-function 'ediff-setup-windows-plain ;; Do not display Ediff control window in separate frame
 initial-major-mode 'emacs-lisp-mode
 org-hide-emphasis-markers t
 backup-directory-alist '(("." . "~/.saves")))


(toggle-frame-fullscreen)
(scroll-bar-mode -1)
(global-set-key (kbd "M-o") 'other-window)
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

;; (straight-use-package 'zenburn-theme)
;; (load-theme 'zenburn t)

(set-face-attribute 'default nil :height 135)

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

(straight-use-package 'selectrum)
(selectrum-mode +1)

(straight-use-package 'selectrum-prescient)
(setq prescient-history-length 1000)
;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)

(global-set-key (kbd "C-x C-z") 'selectrum-repeat)

;;(straight-use-package 'orderless)

;(setq completion-styles '(orderless))

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "s-f") 'projectile-find-file)

(straight-use-package 'magit)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-m") 'magit-status)
(global-set-key (kbd "s-b") 'magit-blame)

(straight-use-package 'crux)
;;(define-key global-map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(define-key global-map (kbd "C-c f") 'crux-recentf-find-file)
(define-key global-map (kbd "M-j") 'crux-smart-open-line-above)

(straight-use-package 'consult)

(setq consult-preview-key "" ;; Press C-x to see the preview
      consult-project-root-function #'projectile-project-root)
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x n") 'buffer-other-window-right)

(straight-use-package 'restclient)
(add-hook 'restclient-mode-hook (lambda () (smartparens-mode)))

(straight-use-package 'hydra)
(straight-use-package 'scala-mode)

(require 'scala-mode)

(define-key scala-mode-map (kbd "C-M-{") 'scala-wrap-line-in-curly-braces) ;; Control + Shift + Alt + {

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

;; (straight-use-package
;;  '(sbt-test-runner :type git :host github :repo "VlachJosef/sbt-test-runner.el"))

(add-to-list 'load-path "~/develop-emacs/sbt-test-runner.el/")

(require 'sbt-test-munit)

(require 'cl-macs)
(defun clear-sbt-compilation-buffer ()
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
		  (sbt:clear current-process-buffer)))))

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

(straight-use-package 'zerodark-theme)
(load-theme 'zerodark t)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'zerodark
   `(selectrum-current-candidate
     ((,class (:background "#48384c"
                           :weight bold
                           :foreground "#c678dd"))))
   `(selectrum-prescient-primary-highlight
   ((,class (:foreground "green"))))
   `(selectrum-prescient-secondary-highlight
   ((,class (:foreground "red"))))))

(enable-theme 'zerodark)

(straight-use-package 'which-key)

(which-key-mode)

(straight-use-package 's)
(straight-use-package 'request)
(straight-use-package 'graphql-mode)
(add-hook 'graphql-mode-hook (lambda () (smartparens-mode)))


(straight-use-package 'transpose-frame)
(define-key ctl-x-4-map "t" 'transpose-frame)

(straight-use-package 'json-mode)
(require 'json-mode)
(add-hook 'json-mode-hook (lambda () (smartparens-mode)))
(define-key json-mode-map (kbd "C-c C-c") 'upload-template)
(define-key json-mode-map (kbd "C-c C-o") 'open-template-in-browser)

(straight-use-package 'markdown-mode)
(straight-use-package 'iedit)
(require 'iedit)
(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(load "~/.dotfiles/.emacs.d/personal/defuns.el")

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
  (consult-ripgrep nil (projectile-symbol-or-selection-at-point)))

(global-set-key (kbd "s-F") 'look-for-thing-at-point-2)
(define-key minibuffer-local-map (kbd "s-F") 'scala-minibuffer-search/body)

(straight-use-package 'info-colors)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(straight-use-package
 '(println-debugger :type git :host github :repo "VlachJosef/println-debugger"))

(require 'println-debugger)

(define-key emacs-lisp-mode-map [remap sp-previous-sexp] 'println-insert-before)
(define-key emacs-lisp-mode-map [remap sp-next-sexp] 'println-insert-after)
(define-key emacs-lisp-mode-map (kbd "C-M-p") 'println-insert-before)
(define-key emacs-lisp-mode-map (kbd "C-M-n") 'println-insert-after)

(define-key scala-mode-map [remap sp-previous-sexp] 'println-insert-before)
(define-key scala-mode-map [remap sp-next-sexp] 'println-insert-after)
(define-key scala-mode-map (kbd "C-M-p") 'println-insert-before)
(define-key scala-mode-map (kbd "C-M-n") 'println-insert-after)

(straight-use-package 'ace-jump-zap)
(global-set-key (kbd "C-z") 'ace-jump-zap-up-to-char)

(straight-use-package 'git-timemachine)

(straight-use-package 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key ibuffer-mode-map (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(straight-use-package 'zygospore)

(winner-mode 1)
(global-set-key (kbd "C-s-p") 'winner-undo) ;; Control + Shift + Cmd + p
(global-set-key (kbd "C-s-n") 'winner-redo) ;; Control + Shift + Cmd + n

(straight-use-package 'burly)
(global-set-key (kbd "C-s-s") 'burly-bookmark-windows)   ;; Control + Shift + Cmd + s
(global-set-key (kbd "C-s-l") 'burly-open-last-bookmark) ;; Control + Shift + Cmd + l
(global-set-key (kbd "C-s-o") 'consult-bookmark)         ;; Control + Shift + Cmd + o

(straight-use-package 'move-dup)
(global-set-key (kbd "M-n") 'move-dup-move-lines-down)
(global-set-key (kbd "M-p") 'move-dup-move-lines-up)
(global-set-key (kbd "C-c d") 'move-dup-duplicate-down)

(straight-use-package 'docker)

(straight-use-package 'yaml-mode)

;; (straight-use-package 'marginalia)
;; (marginalia-mode)
;; (define-key minibuffer-local-map (kbd "s-8") 'marginalia-cycle)

;; (straight-use-package 'prettier-js)
;; (add-hook 'js-mode-hook
;;             (lambda ()
;;               (prettier-js-mode)))

(straight-use-package 'inf-mongo)
(setq inf-mongo-command "/Users/pepa/develop-sensible/mongodb-osx-x86_64-3.2.7/bin/mongo 127.0.0.1:27017")
(add-hook 'inf-mongo-mode-hook (lambda () (smartparens-mode)))

(defun buffer-other-window-right ()
  (interactive)
  (when (eq 1 (count-windows))
    (split-window-right))
  (consult-buffer-other-window))


(defvar consult--source-sbt
  `(:name     "Sbt Buffer"
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

(straight-use-package 'embark)
(define-key global-map (kbd "s-.") 'embark-act)

(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (straight-use-package 'embark-consult)
    (require 'embark-consult)
    (add-to-list 'consult-buffer-sources 'consult--source-sbt 'append)))

(setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sql-connection-alist
   '(("Muninn Local"
      (sql-product 'postgres)
      (sql-user "muninn-app")
      (sql-database "test")
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
     ("Heimdall"
      (sql-product 'postgres)
      (sql-user "heimdall")
      (sql-database "heimdall")
      (sql-server "localhost")
      (sql-port 5436)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "gray55" :slant italic))))
 '(org-verbatim ((t (:inherit shadow :foreground "LightPink1")))))
