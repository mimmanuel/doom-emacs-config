;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Mathias Nielsen"
      user-mail-address "mnie@demant.com")

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
(setq doom-theme 'doom-nord)

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

(setq doom-localleader-key "SPC m"
      doom-localleader-alt-key "M-SPC m")

(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )

(map!
 :leader
 :prefix ("S" . "server")
 :desc "Start server" "s" 'server-start
 :desc "Edit server" "e" 'server-edit
 :desc "Delete server" "x" 'server-force-stop
 :desc "Switch to *server* buffer" "b" '(lambda () (switch-to-buffer server-buffer))
 )

(remove-hook 'server-visit-hook #'make-frame)

(if (eq system-type 'darwin)
  ; something for OS X if true
  ; optional something if not
    (setq mac-command-modifier 'meta
          mac-option-modifier 'option)
)

(pushnew! default-frame-alist '(height . 40) '(width . 128))

(map!
 :after evil
 :n "TAB" #'evil-toggle-fold)

(use-package! evil-org
  :defer t
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

(use-package! outlookedit
  :defer t
  :commands mno-edit-outlook-message mno-put-outlook-message
  :config (setq mno-get-outlook-body (concat "cscript //Job:getMessage " (expand-file-name "~//bin//outlook_emacs.wsf"))
                mno-put-outlook-body (concat "cscript //Job:putMessage " (expand-file-name "~//bin//outlook_emacs.wsf"))))

(map!
 :after outlookedit
 :leader
 :prefix ("oo" . "Outlook")
 :desc "Edit" "e" #'mno-edit-outlook-message
 :desc "Save" "s" #'mno-put-outlook-message)

(use-package! powershell
  :mode ("\.ps[12]*" . powershell-mode)
:hook (powershell-mode . lsp)
:config
(map! :map powershell-mode-map
      :localleader
"s" #'powershell))

(use-package! pyvenv
  :hook (python-mode . pyvenv-mode)
  :init
  (add-to-list 'exec-path "~/.pyvenv/shims/")
  ;; (with-eval-after-load 'flycheck
  ;;   (setq flycheck-python-flake8-executable "python"
  ;;         flycheck-python-mypy-executable "python"
  ;;         flycheck-python-pycompile-executable "python"
  ;;         flycheck-python-pylint-executable "python"))
  ;; (setenv "WORKON_HOME" "~/.pyvenv/versions/")
  (defun +pyvenv-create-or-activate ()
    "Create or activate virtualenv in project-root."
    (interactive)
    (let ((workon_home (getenv "WORKON_HOME"))
          (root (or (projectile-project-root)
                    (directory-file-name default-directory)))
          (venv-name "venv"))
      (if (file-exists-p (format "%s/%s/Scripts" root venv-name))
          (pyvenv-activate (format "%s/%s" root venv-name))
        (progn
          (message "Create virtual env")
          (setenv "WORKON_HOME" root)
          (pyvenv-create venv-name (executable-find "python"))
          (pyvenv-activate (format "%s/%s" root venv-name))
          (setenv "WORKON_HOME" workon_home))))))

(use-package! nxml-mode
  :defer t
  :mode ("\\.xml$" . nxml-mode)
  :init
  :config
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (set-file-template! 'nxml-mode)
  )

(after! org
  (pushnew! +org-babel-mode-alist '(xml . nxml)))

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

(after! org
 (setq org-priority-highest ?A
       org-priority-lowest ?D
       org-priority-default ?B
       org-priority-faces '((?A . (:inherit 'error))
			    (?B . (:inherit 'warning))
			    (?C . (:inherit 'font-lock-string-face))
			    (?D . (:inherit 'font-lock-comment-face
                                   :italic t)))))

(after! org
  (setq org-global-properties '(("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))
        org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"))

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
           "* %?")
          ("l" "link" plain (file "~/org/links.org")
           "[[%^{Link}][%^{Description}]]")
          )))

(after! org
  (setq org-tag-alist '(
                        (:startgroup . nil)
                        ("@localpc" . ?l) ("@devpc" . ?d) ("@kbn" . ?k)
                        (:endgroup .nil)
                        (:newline . nil)
                        ("project" . ?p) ("noexport" . ?n))))

(after! org
  (let* ((base-font-color     (face-foreground 'default nil 'default))
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
     `(org-document-title ((t (,@headline :height 1.5 :underline nil :weight bold))))
     ;; org-agenda-faces
     `(org-agenda-structure ((t (,@headline :height 1.2 :weight semi-bold :family "Segoe UI")))))))

(use-package! org-superstar
  :defer t)

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

(after! org
  (setq org-agenda-custom-commands nil))

(defmacro +mnie/add-org-agenda-custom-commands (&rest command)
  "Add new COMMAND to org-agenda-custom-commands sequentially"
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@command) nil)
       (after! org (add-to-list 'org-agenda-custom-commands ,var)))))

(+mnie/add-org-agenda-custom-commands
               `("n" "Next Actions"
                  ((agenda "" ((org-agenda-overriding-header "Today")
                               (org-agenda-start-day nil)
                               (org-agenda-span 'day)
                               (org-agenda-files (quote ,(mapcar (lambda (f) (concat org-directory f)) '("gtd.org" "tickler.org"))))))
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

(+mnie/add-org-agenda-custom-commands  '("c" . "Contexts")
                                       '("cl" "@localpc" tags-todo "@localpc")
                                       '("cd" "@devpc" tags-todo "@devpc")
                                       '("ck" "@kbn" tags-todo "@kbn")
                                       '("ce" "@emacs" tags-todo "@emacs"))

(after! org
  (add-to-list 'org-agenda-custom-commands '("r" . "Review") t))

(after! org
  (add-to-list 'org-agenda-custom-commands
               '("rd" "Daily Review"
                 ((todo "" ((org-agenda-overriding-header "Inbox")
                            (org-agenda-files (list (expand-file-name "inbox.org" org-directory)))))
                  (todo "" ((org-agenda-overriding-header "Process")
                            (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'regexp ":project:")
                                                           (and (org-agenda-skip-entry-if 'regexp ":@\\w+:"))))))
                  (tags-todo "refine" ((org-agenda-overriding-header "Refine")))))))

(after! org
  (setq org-agenda-prefix-format '((agenda . " %-1i %-12c %?-12t% s")
                                (todo . " %-1i %-12c ")
                                (tags . " %-1i  %-12c")
                                (search . " %-1i  %-12c ")))

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

(setq org-publish-project-alist '(
                                  ("deft-html"
                                   :base-directory "~/.deft/"
                                   :base-extension  "org"
                                   :publishing-directory "~/html_export/.deft"
                                   :publishing-function org-html-publish-to-html
                                   )
                                  ("deft-media"
                                   :base-directory "~/.deft/media/"
                                   :base-extension "png\\|svg"
                                   :publishing-directory "~/html_export/.deft/media"
                                   :publishing-function org-publish-attachment)
                                  ("deft-docx"
                                   :base-directory "~/.deft/"
                                   :base-extension "org"
                                   :publishing-directory "~/docx_export/.deft/"
                                   :publishing-function org-pandoc-export-to-docx
                                   )))

(use-package! org-roam
  :defer t
  :init
  (setq org-roam-db-update-method 'immediate))

(use-package! deft
  :defer t
  :init
  (setq deft-directory org-roam-directory
        deft-use-filter-string-for-filename nil
        deft-use-filename-as-title nil)
  (advice-add #'deft-complete :after '(lambda () (kill-buffer "*Deft*")))
  (map!
   :map deft-mode-map
   :i "C-j" #'evil-next-line
   :i "C-k" #'evil-previous-line))

(use-package! plantuml-mode
  :defer t
  :init
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path (expand-file-name "~/bin/plantuml.jar")))

(add-to-list 'auto-mode-alist '("\\.err$" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out$" . text-mode))

(use-package! selectrum
  :hook (after-init . selectrum-mode)
  :init
  (setq enable-recursive-minibuffers t)
  :general
  (:keymaps 'selectrum-minibuffer-map
            :states '(normal insert)
            "C-h" #'selectrum-previous-page
            "C-l" #'selectrum-next-page
            "C-j" #'selectrum-next-candidate
            "C-k" #'selectrum-previous-candidate
            "C-p" #'selectrum-previous-candidate
            "C-n" #'selectrum-next-candidate
            "C-s" #'selectrum-select-current-candidate)
  (:states '(normal visual)
           :keymaps 'global
           "zs" #'selectrum-repeat))

(use-package! embark
  :defer t
  :init
  ;; (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
  ;;     which-key-replacement-alist)
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-o" #'embark-act))

(use-package! marginalia
  :hook (selectrum-mode . marginalia-mode)
  :init)

(use-package! consult
  ;; :straight (:host github :repo "minad/consult" :branch "main") ;
  ;; :hook (selectrum-mode . consult-annotate-mode)
  :general
  ("C-s" #'consult-line)
  (:states 'normal
           "go" #'consult-outline))

;; (use-package! consult-flycheck)

(use-package! consult-selectrum)

(use-package! prescient
  :defer t)

(use-package! selectrum-prescient
  :hook (selectrum-mode . selectrum-prescient-mode))
