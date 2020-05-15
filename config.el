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

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;;(server-start)

(map!
 :leader
 :prefix ("S" . "server")
 :desc "Start server" "s" 'server-start
 :desc "Edit server" "e" 'server-edit
 :desc "Delete server" "x" 'server-force-stop
 )

(remove-hook 'server-visit-hook #'make-frame)

(if (eq system-type 'darwin)
  ; something for OS X if true
  ; optional something if not
    (setq mac-command-modifier 'meta
          mac-option-modifier 'option)
)

(pushnew! default-frame-alist '(height . 40) '(width . 128))

(load-theme 'doom-palenight)

(map!
 :after evil
 :n "TAB" #'evil-toggle-fold)

(use-package! evil-org
  :after org
  :config
  (evil-org-set-key-theme '(navigation insert textobjects return todo additional calendar)))

  (map!
   :after org
   :map org-mode-map
   ;; Map RET to open-at-point
   :n "RET" #'org-open-at-point
   ;; Navigate Visible headings
   :n "J" #'org-next-visible-heading
   :n "K" #'org-previous-visible-heading
   :n "L" #'org-show-subtree
   :n "H" #'org-cycle)

(use-package! nxml
  :defer t
  :mode ("\\.xml$" . nxml-mode)
  :init
  (setq nxml-auto-insert-xml-declaration-flag nil)
  :config
  (set-file-template! 'nxml-mode)
  )

(after! org
  (pushnew! +org-babel-mode-alist '(xml . nxml)))

(add-to-list 'auto-mode-alist '("\\.err$" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out$" . text-mode))

;;(load! "+org.el")

(after! org
  (setq org-log-done 'time
        org-log-into-drawer t
        org-startup-folded nil))

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
  (setq org-global-properties '(("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")
                                "StoryPoints" . "1 2 3 5 8 13 20 40 100")
        org-columns-default-format "%40ITEM(Task) %3StoryPoints(SP){:} %17Effort(Estimated Effort){:} %CLOCKSUM"))

(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
           "* TODO [#C] %?")
          ("u" "Urgent Todo" entry (file+headline "~/org/gtd.org" "Misc")
           "* TODO [#A] %?"
           :jump-to-captured t)
          ("p" "Project" entry (file+headline "~/org/gtd.org" "Misc")
          "* ACTIVE %? [%] :project:")
          ("i" "Tickler" entry (file+olp+datetree "~/org/tickler.org")
          "* %?"))))

(after! org
  (setq org-tag-alist '(
                        (:startgroup . nil)
                        ("@localpc" . ?l) ("@devpc" . ?d) ("@rqm" . ?r)
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
   `(org-link ((t (:foreground ,base-font-color :underline t))))
   `(org-list-dt ((t (:foreground ,base-font-color :weight bold))))
   `(org-level-8 ((t (,@headline))))
   `(org-level-7 ((t (,@headline))))
   `(org-level-6 ((t (,@headline))))
   `(org-level-5 ((t (,@headline))))
   `(org-level-4 ((t (,@headline :height 1.0))))
   `(org-level-3 ((t (,@headline :height 1.0))))
   `(org-level-2 ((t (,@headline :height 1.0))))
   `(org-level-1 ((t (,@headline :height 1.2 :weight bold))))
   `(org-document-title ((t (,@headline :height 1.5 :underline nil :weight bold)))))))

(use-package! org-superstar)

(use-package! org-pomodoro
  :defer t
  :config
  (setq org-pomodoro-audio-player (executable-find "vlc.exe"))
  )

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
  (setq org-agenda-custom-commands nil))

(defmacro +mnie/add-org-agenda-custom-commands (command)
  `(after! org (add-to-list 'org-agenda-custom-commands ,command)))

(+mnie/add-org-agenda-custom-commands
               '("n" "Next Actions"
                  ((agenda "" ((org-agenda-overriding-header "Today")
                               (org-agenda-span 'day)))
                   (alltodo "" ((org-agenda-overriding-header "On-going Tasks")
                                                 (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("WAITING"))
                                                                                (org-agenda-skip-subtree-if 'todo '("ON-HOLD"))
                                                                                (org-agenda-skip-entry-if 'regexp ":project:")
                                                                                (org-agenda-skip-entry-if 'notregexp "CLOCK:")))))
                   (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority")
                                                 ))
                   (tags-todo "PRIORITY=\"B\"|PRIORITY=\"C\"" ((org-agenda-overriding-header "Medium Priority")
                                                               (org-agenda-sorting-strategy '(priority-down))
                                                               )))
                  ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("STARTED" "WAITING"))
                                                  (org-agenda-skip-subtree-if 'todo '("ON-HOLD"))
                                                  (org-agenda-skip-entry-if 'regexp ":project:")
                                                  (org-agenda-skip-entry-if 'regexp "CLOCK:"))))))

(after! org
  (add-to-list 'org-agenda-custom-commands '("r" . "Review") t))

(after! org
  (add-to-list 'org-agenda-custom-commands
               '("rd" "Daily Review"
                 ((todo "" ((org-agenda-overriding-header "Inbox")
                            (org-agenda-files '((expand-file-name "inbox.org" org-directory)))))
                  (tags-todo "EFFORT=\{\}" ((org-agenda-overriding-header "Process")))))))

(after! org
  (setq org-agenda-prefix-format '((agenda . " %-1i %?-12t% s")
                                (todo . " %-1i %?(org-entry-get nil \"StoryPoints\") ")
                                (tags . " %-1i %?(org-entry-get nil \"StoryPoints\") ")
                                (search . " %-1i ")))

  (setq org-agenda-category-icon-alist
      `(
        ("Review" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Reading" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Development" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
        ("Planning" ,(list (all-the-icons-octicon "calendar")) nil nil :ascent center)
        ("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
        ("Misc" ,(list (all-the-icons-octicon "checklist")) nil nil :ascent center)
        ("" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)))
)

(after! org
  (pushnew! org-link-abbrev-alist
            '("rqm" . "https://clm.dgs.com/qm/web/console/System%20Verification%20for%20projects%20and%20products#action=com.ibm.rqm.planning.home.actionDispatcher&subAction=viewTestCase&id=%s")
            '("jira" . "https://jira.kitenet.com/browse/%s"))
  )

(use-package! deft
  :init
  (setq deft-directory "~/.deft/"))

(setq org-agenda-category-icon-alist
      `(("" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Review" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Reading" ,(list (all-the-icons-material "library_books")) nil nil :ascent center)
        ("Development" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
        ("Planning" ,(list (all-the-icons-octicon "calendar")) nil nil :ascent center)))
