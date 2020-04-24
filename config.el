;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mathias Nielsen"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hasklig" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Verdana" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;(server-start)

(map!
 :leader
 :prefix ("S" . "server")
 :desc "Start server" "s" 'server-start
 :desc "Edit server" "e" 'server-edit
 :desc "Delete server" "x" 'server-force-stop
 )

(if (eq system-type 'darwin)
  ; something for OS X if true
  ; optional something if not
    (setq mac-command-modifier 'meta
          mac-option-modifier 'option)
)

(load-theme 'doom-palenight)

(use-package! evil-org
  :after org
  :config
  (evil-org-set-key-theme '(navigation insert textobjects return todo additional calendar)))

;;(load! "+org.el")

(after! org
  (setq org-log-done 'time
        org-log-into-drawer t
        org-startup-folded nil))

(map!
 :mode org
 :n "RET" #'org-open-at-point)

(after! org
  (setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!)")
        (sequence "WAITING(w!)" "|")
        (sequence "|" "CANCELLED(C!)")
        (sequence "ACTIVE(a)" "ON-HOLD(h@!)" "|" "COMPLETED(c!)")))

(setq org-treat-S-cursor-todo-selection-as-state-change nil))

(after! org
  (setq org-todo-keyword-faces
    '(("TODO" . (t (:inherit org-todo)))
      ;;("STARTED" . (t (:inherit org-todo :foreground "green")))
      (("COMPLETED" "DONE") . (t (:inherit org-done :strike-through t)))
      ("ON-HOLD" . "orange"))))

(setq org-priority-highest ?A
      org-priority-lowest ?D
      org-priority-default ?B)

(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
          "* TODO %?")
          ("p" "Project" entry (file+headline "~/org/gtd.org" "Projects")
          "* ACTIVE %? [%] :project:")
          ("i" "Tickler" entry (file+olp+datetree "~/org/tickler.org")
          "* %?"))))

(after! org
  (setq org-tag-alist '(("important" . ?i) ("urgent" . ?u)
                        (:startgroup . nil)
                        ("@localpc" . ?l) ("@devpc" . ?l) ("@rqm" . ?r)
                        (:endgroup .nil)
                        (:newline . nil)
                        ("project" . ?p))))

(after! org
  (let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight normal :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline))))
   `(org-level-7 ((t (,@headline))))
   `(org-level-6 ((t (,@headline))))
   `(org-level-5 ((t (,@headline))))
   `(org-level-4 ((t (,@headline :height 1.0))))
   `(org-level-3 ((t (,@headline :height 1.0))))
   `(org-level-2 ((t (,@headline :height 1.0))))
   `(org-level-1 ((t (,@headline :height 1.2 :weight bold))))
   `(org-document-title ((t (,@headline :height 1.5 :underline nil)))))))

(use-package! org-superstar)

(setq org-agenda-files (list "~/org/gtd.org"))

(setq org-stuck-projects '("+PROJECT" ("TODO" "NEXT") nil ""))

(setq org-agenda-window-setup 'current-window)
;;(add-hook 'evil-org-agenda-mode-hook #'org-super-agenda-mode)
;;(setq org-super-agenda-header-map (make-sparse-keymap))

(setq org-agenda-start-on-weekday nil
      org-agenda-span 10
      org-agenda-start-day "0d")

;; Speed up org-agenda
;;
(setq org-agenda-inhibit-startup t
      org-agenda-dim-blocked-tasks nil
      org-use-tag-inheritance nil
      org-agenda-use-tag-inheritance nil)

(use-package! org-super-agenda
  :after evil-org
  :config
  (add-hook 'evil-org-agenda-mode-hook #'org-super-agenda-mode))

(use-package! org-ql
  :after org
  :config
    (defvaralias 'org-lowest-priority 'org-priority-lowest)
)

(after! org
  (setq org-agenda-custom-commands
        '(("n" "Agenda"
           ((org-ql-block '(todo "STARTED"))
            (org-ql-block '(and (todo "TODO") (priority "A")))
            (org-ql-block '(and (todo "TODO") (priority <= "B")))
            ;;(todo "STARTED")
            ;;(todo "TODO" ((org-agenda-overriding-header "Todo list")))
            ))
          ("r" . "Review")
          ("rd" "Daily Review"
           ((alltodo "" ((org-agenda-overriding-header "Inbox")
                         (org-agenda-files (concat org-directory "inbox.org"))))))
          )))

(setq org-agenda-prefix-format '((agenda . " %-1i %?-12t% s")
                                (todo . " %-1i ")
                                (tags . " %-1i")
                                (search . " %-1i")))

(setq org-agenda-category-icon-alist
      `(("" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Review" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Reading" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Development" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
        ("Planning" ,(list (all-the-icons-octicon "calendar")) nil nil :ascent center)))

(use-package! deft
  :init
  (setq deft-directory "~/.deft/"))

(setq org-agenda-category-icon-alist
      `(("" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Review" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Reading" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Development" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
        ("Planning" ,(list (all-the-icons-octicon "calendar")) nil nil :ascent center)))
