(setq
 initial-scratch-message nil
 column-number-mode t
 kill-do-not-save-duplicates t
 dired-dwim-target t
 dired-listing-switches "-alo"
 frame-resize-pixelwise t
 ring-bell-function 'ignore
 scroll-error-top-bottom t
 scroll-preserve-screen-position t
 inhibit-startup-screen t
 initial-buffer-choice "~/.dotfiles/.config/emacs-profiles/bare/init.el")

(toggle-frame-fullscreen)
(scroll-bar-mode -1)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-W") 'toggle-truncate-lines)
(global-set-key (kbd "C-M-j") 'join-line)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(straight-use-package 'zenburn-theme)
(load-theme 'zenburn t)

(set-face-attribute 'default nil :height 135)

(require 'dired-x)

(require 'recentf)
(recentf-mode 1)

(savehist-mode)
