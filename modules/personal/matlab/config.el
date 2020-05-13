;;; .doom.d/matlab/config.el -*- lexical-binding: t; -*-

(use-package! matlab-mode
  :mode ("\\.m$" . matlab-mode)
  :defer t
  :config
  (add-hook 'matlab-mode-hook #'mnie/run-prog-mode-hooks))

(after! highlight-numbers
  (puthash 'matlab-mode 'do-not-use highlight-numbers-modelist))

;;;### autoload
;;;

(defun mnie/run-prog-mode-hooks ()
  "Matlab-mode does not inherit from prog-mode. So prog-mode hooks must be run manually"
    (run-hooks 'prog-mode-hook))
