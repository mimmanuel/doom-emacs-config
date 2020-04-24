;;; c:/Users/mnie/AppData/Local/DoomEmacs/.doom.d/+org.el -*- lexical-binding: t; -*-

(after! org
  (setq org-log-done 'time
        org-log-into-drawer t
        org-startup-folded nil)

  ;; Set org-todo-keywords
  (setq org-todo-keywords
       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
         (sequence "WAITING(w!)" "|")
         (sequence "|" "CANCELLED(C!)")
         (sequence "ACTIVE(a)" "ON-HOLD(h@!)" "|" "COMPLETED(c!)")))

  (setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("NEXT" . "cyan")
        (("COMPLETED" "DONE") . org-done)
        ("ON-HOLD" . "orange")))

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-state-tags-triggers
        '(("ACTIVE" ("project" . t))
          ("ON-HOLD" ("project" . t))
          ("COMPLETED" ("project" . t))
          (done ("urgent") ("important"))))

  ;; Setup org-capture-templates
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %?")
        ("p" "Project" entry (file+headline "~/org/gtd.org" "Projects")
         "* ACTIVE %? [%] :project:")
        ("i" "Tickler" entry (file+olp+datetree "~/org/tickler.org")
         "* %?")))

  (setq org-M-RET-may-split-line nil)

  (setq org-tag-alist '(("important" . ?i) ("urgent" . ?u)
                        (:newline . nil)
                        ("project" . ?p)))
  ;; Setup org-agenda-files
  (setq org-agenda-files (list "~/org/gtd.org"))

  (setq org-stuck-projects '("+PROJECT" ("TODO" "NEXT") nil ""))

  (setq org-agenda-window-setup 'current-window)
  ;(add-hook 'evil-org-agenda-mode-hook #'org-super-agenda-mode)
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

  (setq org-agenda-custom-commands
      '(("n" "Agenda"
         ((agenda "" ((org-agenda-overriding-header "Today")
                      (org-agenda-span 'day)
                   ))
          (alltodo "" ((org-agenda-overriding-header "")

                       (org-super-agenda-groups '(
                                                  (:discard (:todo "WAITING" :tag "project"))
                                                  (:name "DO"
                                                         :and (
                                                               :tag "important"
                                                               :tag "urgent")
                                                         )
                                                  (:name "DECIDE"
                                                         :and
                                                         (
                                                          :tag "important"
                                                          )
                                                         )
                                                  ))
                       )
                   )
          )
         )
        ("r" . "Review")
        ("rd" "Daily Review"
         ((alltodo "" ((org-agenda-overriding-header "INBOX")
                       (org-agenda-files (list "~/org/inbox.org"))))
          (stuck nil)
          (agenda "" ((org-agenda-overriding-header "Upcoming") (org-agenda-span 3)))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups '(
                                                  (:discard (:todo "WAITING" :tag "project"))
                                                  (:name "DO"
                                                         :and (
                                                               :tag "important"
                                                               :tag "urgent")
                                                         )
                                                  (:name "DECIDE"
                                                         :and
                                                         (
                                                          :tag "important"
                                                          )
                                                         )
                                                  (:name "DELEGATE"
                                                         :tag "urgent")
                                                  (:name "DELETE"
                                                         :not (:tag "important" :tag "urgent" :tag "project"))
                                                  ))))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
          ))
        ("rw" "Weekly Review"
         (
          (agenda "" ((org-agenda-overriding-header "Next Week") (org-agenda-span 7) (org-agenda-start-on-weekday nil)))
          (tags "+project" ((org-agenda-overriding-header "Project List")
                            (org-super-agenda-groups '((:auto-todo t)))))
          )
         )
        ("p" "Projects"
         ((tags "+project" ((org-agenda-overriding-header "Project List")
                           (org-super-agenda-groups '((:auto-todo t)))))))
        )
      )

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (defun my--org-agenda-process ()
    (interactive)
    (org-agenda-set-tags)
    (org-agenda-refile nil nil nil))

  (defun my-org-confirm-babel-evaluate (lang body)
    "Don't confirm executing plantuml"
    (not (string= lang "plantuml"))
    )

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (add-hook 'org-mode 'org-display-inline-images)
  )

(after! plantuml
  (setq plantuml-jar-path "c:/Program Files/PlantUML/plantuml.jar")
  )
