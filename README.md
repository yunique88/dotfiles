# Setup
## EMACS
#### Install Melpa
`
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
`
to **~/.emacs** file

- run command `git clone https://github.com/emacs-evil/evil ~/.emacs.d/evil`
- in emacs, `M-x RET package-install RET dracula-theme`

#### Reference
##### Open a File
`C-x C-f Dropbox/todo.org`t
##### Change todo status directly
`C-c C-t [status shortcut]`
##### Change todo status (not recommended)
`shift-right/left`
##### New row with TODO heading
`M-shift-RET`
##### hyperlink
`[[link url][description]]`
##### open hyperlink
`C-c C-o` or click
##### fold/unfold current level headings
`TAB`
##### fold/unfold all level headings
`shift-TAB`
##### close active (focused) window
`C-x 0`
##### split window vertically
`C-x 2`
##### split window horizontally
`C-x 3`


#### Example
[good examples](https://emacsclub.github.io/html/org_tutorial.html)




















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