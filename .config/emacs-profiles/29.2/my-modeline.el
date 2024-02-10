(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                "  "
                mode-line-position
                my-project-root
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(defface my-modeline-project-name
  '((t :background "#6fd560" :foreground "black"))
  "Face to show project name in mode line")

(defvar-local my-project-root
    '(:eval (propertize (format " %s " (project-name (project-current))) 'face 'mode-line-emphasis)))

(put 'my-project-root 'risky-local-variable t)
