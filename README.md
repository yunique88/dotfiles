# Setup
## EMACS
#### Install Melpa
```
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))(not (gnutls-available-p))))(proto (if no-ssl "http" "https")))
(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELED(c@)")))
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode agenda options                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; agenda key bind
(define-key global-map "\C-ca" 'org-agenda)

;; agenda set which files to look for
(setq org-agenda-files (list "~/Dropbox/org/work.org"))

;; open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;; show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))

;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; don't give awarning colour to tasks with impending deadlines
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
```
to **~/.emacs** file

- run command `git clone https://github.com/emacs-evil/evil ~/.emacs.d/evil`
- in emacs, `M-x RET package-install RET dracula-theme`

#### Reference
##### File
- `C-x C-f todo.org` : open todo.org file
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