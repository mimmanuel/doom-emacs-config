;;; personal/matlab/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +matlab-semicolon-at-eol ()
    "Add semicolon at end of current line"
    (interactive)
    (save-excursion
      (end-of-line)
      (matlab-semicolon-on-return)))
