#!/bin/bash

## prepare system and install yay
sudo pacman -S --needed --noconfirm git go base-devel

mkdir -p ~/git
cd ~/git
if [[ ! -d yay ]]
then
  git clone https://aur.archlinux.org/yay.git
fi
cd yay
git pull --no-rebase
makepkg -si --noconfirm --needed


## install basic packages
yay -S --needed --noconfirm pacman-contrib networkmanager networkmanager-openvpn network-manager-applet networkmanager-strongswan dhclient dnsmasq ccid pcsclite lsb-release ethtool x86_energy_perf_policy bash-completion zsh-completions hddtemp alsa-utils pulseaudio-alsa wireless_tools lua-filesystem cmake clang wmname wget unzip iproute2 net-tools tcpdump python-requests bind-tools mtr scrot geeqie openvpn remmina chrony nfs-utils pgadmin4 notmuch plantuml flake8 autopep8 openbsd-netcat pulseaudio-alsa fwupd dmidecode rsync strace lsof sysstat jq screen fzf smartmontools dosfstools ntfs-3g exfat-utils firefox chromium cantarell-fonts gsfonts noto-fonts terminus-font tex-gyre-fonts ttf-caladea ttf-carlito ttf-dejavu ttf-droid ttf-inconsolata ttf-opensans ttf-liberation ttf-ubuntu-font-family ttf-courier-prime ttf-heuristica ttf-merriweather ttf-merriweather-sans ttf-oswald ttf-quintessential ttf-signika ttf-symbola nerd-fonts-complete otf-font-awesome ttf-font-awesome xss-lock acpid hal bluez-utils python-pyqt5 perl awesome vicious gopass mpv mpd ncmpcpp spotify libreoffice-fresh emacs rxvt-unicode-patched urxvt-perls compton copyq unclutter ccze xorg-apps mesa xf86-video-intel xf86-video-nouveau swayidle-git sway-git swaybg-git waybar-git swaylock-effects-git wf-recorder-git swayidle-git swaynagmode grimshot-git light-git mako-git bemenu bemenu-dmenu networkmanager-dmenu-git rofi-git wofi playerctl pamixer etc-update docx2txt atril cups cups-pdf cups-filters foomatic-db foomatic-db-ppds foomatic-db-nonfree foomatic-db-nonfree-ppds foomatic-db-engine foomatic-db-gutenprint-ppds texlive-bin emptty-git aspell-de aspell-es aspell-ru hunspell-de hunspell-en_GB hunspell-en_US hunspell-es_es tp_smapi intel-undervolt python-language-server python-pyls-isort-git ccls yaml-language-server-bin llvm gopls java-language-server zathura-pdf-mupdf kubectl-bin ansible ansible-lint terraform helm docker istio-bin udisks2 ditaa plantuml-ascii-math laptop-mode-tools-git

## skipped afew-git xnviewmp

sudo systemctl enable chronyd
sudo systemctl enable pcscd
sudo systemctl enable org.cups.cupsd.service
sudo systemctl enable docker
sudo systemctl enable emptty
sudo systemctl enable NetworkManager
sudo systemctl enable NetworkManager-dispatcher

cp -R .config/* ~/.config/
cp -R .gnupg ~/
cp -R .emacs.d ~/
cp -R bin ~/
sudo cp etc/environment /etc/
sudo cp etc/systemd/user/* /etc/systemd/user/
