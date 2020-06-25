# Setup
## EMACS
#### Install Melpa
```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic functions                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))(not (gnutls-available-p))))(proto (if no-ssl "http" "https")))
(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; evil mode (vim commands)
(require 'evil)
(evil-mode 1)

;; desktop automatically re-open last closed file
(desktop-save-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode options                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org todo keywords
;; ! = timestamp, @ = note with timestamp
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d!)" "CANCELED(c@)" "DEFERRED(f@)")))

;; org tags align to right of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

;; bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; one separator empty line between org mode should fold as expected
(setq org-cycle-separator-lines 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode agenda options                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; agenda key bind
(define-key global-map "\C-ca" 'org-agenda)

;; agenda set which files to look for
(setq org-agenda-files (list "~/Dropbox/org/"))

;; open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;; show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))

;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; don't show done tasks
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;; don't show agenda block separators
(setq org-agenda-block-separator nil)

;; don't give warning color to tasks with impending deadlines
;; if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))

;; don't show tasks that are scheduled or have deadlines in the
;; normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

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
      '(("c" "My Custom Agenda View"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-sorting-strategy
                        (quote ((agenda time-up priority-down tag-up) )))
                      (org-agenda-overriding-header (create-header "TODAY"))))
          (agenda "" ((org-agenda-span 14)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-overriding-header (create-header "NEXT 2 WEEKS"))))
          (alltodo "" ((org-agenda-overriding-header (create-header "OTHER TODO's"))))
          (alltodo "" ((org-agenda-todo-ignore-deadlines nil)
                       (org-agenda-todo-ignore-scheduled nil)
                       (org-agenda-overriding-header (create-header "ALL TODO's"))))))))

;; refresh every 1 min agenda view
(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
(run-at-time nil 60 'kiwon/org-agenda-redo-in-other-window)

;; define custom time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
        "......" "----------------")))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private functions & variables                                          ;;
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

;; center string format
;; use-case example:
;; (center-string "KJF" 10) ==> "   KJF    "
(defun center-string (string size)
  (let* ((padding (/ (- size (length string)) 2))
         (lpad (+ (length string) padding))
         (lformat (format "%%%ds" lpad))
         (rformat (format "%%%ds" (- size))))
    (format rformat (format lformat string))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org-bullets org-timeline dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

```
to **~/.emacs** file
`M-x RET package-install RET dracula-theme`
- dracula-theme
- evil 
- org-bullets


#### Reference
##### File
- `C-x C-f todo.org` : open todo.org file
- `C-x C-s` : save file
- `C-x C-c` : close file
- `C-x 0` : close active (focused) window
- `C-x 2` : split window vertically
- `C-x 3` : split window horizontally

##### Org Navigation
- `C-c C-t [status shortcut]` : change todo status
- `shift-right/left` : change todo status (not recommended)
- `M-shift-RET` : new row with TODO heading
- `M-RET` : new row without TODO heading
- `TAB` : fold/unfold current level headings
- `shift-TAB` : fold/unfold all level headings

##### content
- `[[link url][description]]` : link
- `C-c C-o` or click : open hyperlink

##### Schedule/ Deadlines - [link](https://orgmode.org/manual/Inserting-deadline_002fschedule.html)
- `C-c C-d` : insert deadline stamp
- `C-c C-s` : insert schedule stamp
- `C-c / d` : show all deadlines
- `C-1 C-c / d` : show all deadlines due tomorrow
- `C-c / b` : show deadline/schedule item before given date
- `C-c / a` : show deadline/schedule item after given date
- `shift-right/left` : go forward/backward a day

##### Agenda - [link](https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html)
- `C-c a a` : show agenda
- `l` : show log (displays finished tasks and completion times)
- `RET` : go to original location of the item in org file
- `x` : show agenda

#### [Cheatsheet](https://emacsclub.github.io/html/org_tutorial.html)






-----------------------------------------------------------------

-----------------------------------------------------------------

-----------------------------------------------------------------














## UBUNTU

#### Dotfiles
- `git clone https://github.com/azulee/dotfiles.git`
- `cd ~/dotfiles && ./deploy.sh`
<br/><br/>

#### Key Delay
- Open ** Keyboard**
- Under Repeat Keys, lower **Delay**
<br/><br/>

#### [Window Management](https://askubuntu.com/questions/22207/quickly-place-a-window-to-another-screen-using-only-the-keyboard)
- Run `sudo apt-get update && sudo apt-get install compizconfig-settings-manager compiz-plugins`
- Open **CCSM**
- Click **Window Management** and **Put**
- Check **Enable Put** box
- Modify **Put To Next Output** to set a desired shortcut
- [Optional] I unchecked **Animations** and **Fading Windows** from Effects.
- Log out and back in again.
<br/><br/>

#### [CAPS as control key](https://askubuntu.com/questions/969053/map-caps-lock-to-control-on-ubuntu-17-10)
- Run `sudo apt install gnome-tweak-tool`
- Open **Tweaks Tool**
- Click **Typing** and **Ctrl key position** and **Caps Lock as Ctrl**
- Log out and back in again.
<br/><br/>

#### Text Size Increase
- Open **Tweaks Tool**
- Click **Fonts** and update **Scaling Factor**
<br/><br/>

#### [Korean Keyboard](http://hochulshin.com/ubuntu-1604-hangul/)
##### 한글 설치
- `sudo apt-get install fcitx-hangu`l로 한글을 설치한다.
- System Settings > Language Support를 실행해서 아직 완전히 설치되지 않다고 표시되는데 잠시 기다려서 모두 설치한다.
- Keyboard input method system:을 ibus가 아닌 fcitx로 변경한다.
- 재부팅한다.
##### 한영 전환 설정
######Shortcut 설정
- AllSettings > Keyboard > Shortcuts Tab > Typing을 선택한다.
 - Switch to Next source, Switch to Previous sourc, Compose Key, Alternative Characters Key를 모두 Disabled로 선택한다. Disabled로 선택하기 위해서는 backspace를 누르면 된다.
- Compose Key의 Disabled를 길게 눌러 Right Alt를 선택한다.
- Switch to next source는 한영키를 눌러 Multikey를 선택한다. 반드시 Compose Key 설정이 먼저되어야 Multikey를 선택할 수 있다.
- AllSetting 윈도우를 닫고 상단 메뉴바 오른쪽의 입력기 선택하는 것을 본다. 키보드 표시가 된 것이 fcitx이다. fcitx아이콘을 눌러서 Configure Current Input Method를 선택한다.
- Keyboard-English(US)가 있다면 +를 눌러 Hangul을 추가한다. (Uncheck “Only Show Current Language”). Korean이 아닌 Hangul이여야 한다.
- Global Config tab에서 Trigger Input Method는 한/영키를 눌러 Multikey로 설정(왼쪽 오른쪽 모두)하고 Extrakey for trigger input method는 Disabled로 설정한다. (Mac에서는 command key이므로 대신 shift+space를 선택한다.)
- Global Config tab에서 Program > Share State Among Window > All을 선택한다.
<br/><br/>

## Bluetooth Headphones
- `sudo vim /etc/bluetooth/main.conf`
- replace `#ControllerMode = dual` with `ControllerMode = bredr`
- `sudo service bluetooth restart`