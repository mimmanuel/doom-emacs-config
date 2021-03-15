;;; .doom.d/matlab/config.el -*- lexical-binding: t; -*-

(use-package! matlab-mode
  :mode ("\\.m$" . matlab-mode)
  :defer t
  :init
  (setq matlab-return-add-semicolon t
	matlab-quiesce-nosemi-regexp "\\s-*\\(function\\|parfor\\|for\\|spmd\\|while\\|try\\|catch\\|\ switch\\|otherwise\\|case\\|break\\|if\\|else\\|end\\|return\\|disp\\|\ $\\|%\\|methods\\|classdef\\|properties\\)")
  (set-company-backend! 'matlab-mode
    'company-capf 'company-file 'company-dabbrev-code 'company-keywords 'company-yasnippet)
  (add-hook 'matlab-mode-hook (defun mnie/run-prog-mode-hooks-h ()
			       "Run prog-mode hooks"
			       (run-hooks 'prog-mode-hook)))
  (add-hook 'matlab-mode-hook (defun +matlab-add-semicolon-on-exit-insert-state-h ()
				"Add semicolon after exiting insert state."
				(add-hook 'evil-insert-state-exit-hook #'+matlab-semicolon-at-eol nil t)))
  (add-hook 'matlab-mode-hook (defun +matlab-set-autofill-fun-h ()
                                "Set autofill function"
                                (set (make-local-variable 'auto-fill-function)
                                     #'matlab-auto-fill)
                                (set (make-local-variable 'outline-regexp) "\\s-*\\(%%\\|function\\)")))
  :config
;; Flycheck-mlint
(with-eval-after-load 'flycheck
  (flycheck-define-checker matlab-mlint
                 "A MATLAB syntax checker using mlint"
                 :command ("mlint" source)
                 :error-patterns
                 (
                  ;;(error line-start "L " line " (C " column "):" (message "Invalid") line-end)
		  (warning line-start "L " line " (C " column "-" end-column "):" (message) line-end)
		  (warning line-start "L " line " (C " column "):" (message) line-end))
                 :modes matlab-mode)
               (add-to-list 'flycheck-checkers 'matlab-mlint)))

(after! highlight-numbers
  (puthash 'matlab-mode 'do-not-use highlight-numbers-modelist))

(after! projectile
  (pushnew! projectile-project-root-files "matlabsetup.m"))

(map!
 :mode matlab-mode
 :localleader
 "f" #'matlab-beginning-of-defun
 "F" #'matlab-end-of-defun
 )

(with-eval-after-load 'matlab-mode
  (defun matlab-find-block-comments (limit)
    "Find code that is commented out with %{ until %}.
Argument LIMIT is the maximum distance to search."
    (if (and (< (point) limit)
	     (re-search-forward "^\\s-*%{\\s-*$" limit t))
	(let ((b1 (match-beginning 0))
	      (e1 (match-end 0))
	      (b2 nil) (e2 nil)
	      (b3 nil) (e3 nil))
	  (goto-char b1)
	  (if (and (not (bolp))
		   (progn
		     (forward-char -1)
		     (matlab-cursor-in-string-or-comment)))
	      (progn
		(goto-char e1) ;; skip over this one.
		nil)
	    ;; Else, find the end.  We will certainly be in
	    ;; a comment, so no need to check on the end.
	    (setq b2 (re-search-forward "^\\s-*%}\\s-*$" limit t))
	    (if (not b2)
		(progn
		  ;; No end ?  Let's tell font-lock to just go
		  ;; to point-at-eol can call it done.
		  (goto-char e1)
		  (set-match-data
		   (list b1 (point-max)
			 b1 (point-max)
			 b1 e1
			 (point-max) (point-max)
			 ))
		  (goto-char (point-max))
		  t)

	      ;; We have a match.  Return that region.
	      (setq b2 (match-beginning 0)
		    e2 (match-end 0))
	      (set-match-data
	       (list b1 e2  ; full match
		     b1 e2  ; the full comment
		     b1 e1  ; the block start
		     b2 e2  ; the block end
		     ))
	      (goto-char e2); move to end
	      t
	      ))))))


;;;###autoload
(defun +matlab-semicolon-at-eol ()
    "Add semicolon at end of current line"
    (interactive)
    (save-excursion
      (end-of-line)
      (matlab-semicolon-on-return)))
