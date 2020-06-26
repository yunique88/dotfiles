# Setup

## EMACS

### Initial Setup
1. download emacs
1. cleanup: `rm -rf ~/.emacs ~/.emacs.d`
1. install spacemacs: `git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d`
1. select vim style, heavy with full features
1. go to dotfile: `SPC f e d`
1. look through layers help page: `SPC h l`
1. then enable layers: (ex. org)
1. update to development branch to get latest release: `cd ~/.emacs.d && git checkout develop && git pull` then reopen emacs
1. append my custom scripts for emacs
```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           MY CUSTOMIZATIONS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           basic functions                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; desktop automatically re-open last closed file
(desktop-save-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           org-mode options                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org todo keywords
;; ! = timestamp, @ = note with timestamp
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d!)" "CANCELED(c@)" "DEFERRED(f@)")))

;; one separator empty line between org mode should fold as expected
(setq org-cycle-separator-lines 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         org-mode agenda options                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; agenda set which files to look for
(setq org-agenda-files (list "~/Dropbox/org/"))

;; open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; don't show done tasks
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;; don't show tasks that are scheduled or have deadlines in the
;; normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

;; don't show agenda block separators
(setq org-agenda-block-separator nil)

;; don't show tasks that are scheduled or have deadlines in the
;; normal todo list
;; (setq org-agenda-todo-ignore-deadlines (quote all))
;; (setq org-agenda-todo-ignore-scheduled (quote all))

;; sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

;; use am/pm instead 24h format
(setq org-agenda-timegrid-use-ampm t)

;; my custom view of agenda and todos
(setq org-agenda-custom-commands
      '(("h" "My Hourly Schedule for Today and Tomorrow"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-agenda-overriding-header (create-first-header "TODAY"))))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day "+1d")
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-agenda-overriding-header (create-header "TOMORROW"))))))
        ("d" "My Daily Schedule for Next 2 Weeks"
          ((agenda "" ((org-agenda-span 14)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-overriding-header (create-first-header "NEXT 2 WEEKS"))))
          (alltodo "" ((org-agenda-overriding-header (create-header "OTHER TODO's"))))
          (alltodo "" ((org-agenda-todo-ignore-deadlines nil)
                       (org-agenda-todo-ignore-scheduled nil)
                       (org-agenda-overriding-header (create-header "ALL TODO's"))))))))

;; define custom time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
        "......" "----------------")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    private functions & variables                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line
(setq halfbar '===============================================)
(setq wholebar '==================================================================================================================)
(setq title-length 20)

;; headers
(defun create-header (title)
   (format "\n\n\n%s\n%s%s%s\n%s\n"
                        (symbol-value 'wholebar)
                        (symbol-value 'halfbar)
                        (center-string title (symbol-value 'title-length))
                        (symbol-value 'halfbar)
                        (symbol-value 'wholebar)))

(defun create-first-header (title)
   (format "%s\n%s%s%s\n%s\n"
                        (symbol-value 'wholebar)
                        (symbol-value 'halfbar)
                        (center-string title (symbol-value 'title-length))
                        (symbol-value 'halfbar)
                        (symbol-value 'wholebar)))
;; center string format
;; use-case example:
;; (center-string "KJF" 10) ==> "   KJF    "
(defun center-string (string size)
  (let* ((padding (/ (- size (length string)) 2))
         (lpad (+ (length string) padding))
         (lformat (format "%%%ds" lpad))
         (rformat (format "%%%ds" (- size))))
    (format rformat (format lformat string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 other                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TREEMACS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq treemacs-show-hidden-files nil)

(setq-default dotspacemacs-configuration-layers '(
  (treemacs :variables treemacs-use-follow-mode 'tag)))

(setq-default dotspacemacs-configuration-layers '(
  (treemacs :variables treemacs-use-filewatch-mode t)))

(setq-default dotspacemacs-configuration-layers '(
  (treemacs :variables treemacs-use-scope-type 'Perspectives)))
```
at the end of `~/.spacemacs` file

### Useful Shortcuts
- major mode commands: `,` instead of `SPC m`
- insert snippet: `SPC i s`

### Snippets
highlight piece of code that will be used as snippet, then run `SPC SPC helm-yas-create-snippet-on-region`.


--------------------------------------------------------------------
