;;; personal/p4/config.el -*- lexical-binding: t; -*-

(use-package! p4)

(map!
 :after p4
 :leader
 :prefix ("p 4" . "Perforce")
 ;(:prefix ("4" . "Perforce")
 :desc "Add" "a" #'p4-add
 :desc "Edit" "e" #'p4-edit
 :desc "Submit" "s" #'p4-submit);)

(after! projectile
  (pushnew! projectile-project-root-files ".p4settings"))
