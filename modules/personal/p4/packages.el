;; -*- no-byte-compile: t; -*-
;;; personal/p4/packages.el

(package! p4)

(when (featurep! :tools magit)
  (package! magit-p4))
