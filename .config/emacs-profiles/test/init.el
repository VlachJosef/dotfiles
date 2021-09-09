(setq
 initial-scratch-message nil
 column-number-mode t
 kill-do-not-save-duplicates t
 dired-dwim-target t
 dired-listing-switches "-alo"
 frame-resize-pixelwise t
 ring-bell-function 'ignore
 scroll-error-top-bottom t)

(toggle-frame-fullscreen)
(scroll-bar-mode -1)
(setq scroll-preserve-screen-position t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; (straight-use-package 'selectrum)
;; (selectrum-mode +1)

;; (setq orderless-skip-highlighting (lambda () selectrum-is-active)
;;       selectrum-highlight-candidates-function #'orderless-highlight-matches)

(straight-use-package 'vertico)
(vertico-mode +1)

(straight-use-package 'embark)
(define-key global-map (kbd "M-.") 'embark-act)

(set-face-attribute 'default nil :height 135)

(require 'dired-x)

(require 'recentf)
(recentf-mode 1)

(straight-use-package 'crux)
(define-key global-map (kbd "C-c f" ) 'crux-recentf-find-file)
(define-key global-map (kbd "C-c d" ) 'crux-duplicate-current-line-or-region)

;; (straight-use-package 'solarized-theme)
;; (load-theme 'solarized-light t)

(straight-use-package 'zenburn-theme)
(load-theme 'zenburn t)

(straight-use-package 'session)
(setq session-jump-undo-threshold 80) ; change positions must differ by 80 characters
(global-set-key (kbd "C-.") 'session-jump-to-last-change)

(straight-use-package 'git-timemachine)

(straight-use-package 'orderless)

(setq completion-styles '(substring)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(savehist-mode)

;; (straight-use-package 'marginalia)
;; (marginalia-mode)

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "s-f") 'projectile-find-file)

(straight-use-package 'consult)

(global-set-key [remap goto-line] 'consult-goto-line)

(setq consult-preview-key "" ;; Press C-x to see the preview
      consult-project-root-function #'projectile-project-root)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)

(straight-use-package 'highlight-symbol)
(global-set-key (kbd "s-h") 'highlight-symbol)
(global-set-key (kbd "s-n") 'highlight-symbol-next)
(global-set-key (kbd "s-p") 'highlight-symbol-prev)
(global-set-key (kbd "s-r") 'highlight-symbol-remove-all)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-W") 'toggle-truncate-lines)

(global-set-key (kbd "C-M-j") 'join-line)

(straight-use-package 'hydra)
(straight-use-package 'scala-mode)

(straight-use-package 'restclient)

(straight-use-package
 '(sbt-mode :type git :host github :repo "VlachJosef/emacs-sbt-mode" :branch "jv/reStart"))

(require 'sbt-mode-hydra)
(global-set-key (kbd "C-c v") 'sbt-hydra)

(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-mode +1)
(global-set-key [remap kill-sexp] 'sp-kill-sexp)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(orderless-match-face-0 ((t (:foreground "#72a4ff")))))
