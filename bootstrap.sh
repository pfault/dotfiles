#!/bin/bash

## prepare boot stick:
# dd bs=4M if=archlinux.iso of=/dev/sdx status=progress && sync

cryptsetup -c aes-xts-plain64 -s 512 -h sha512 --iter-time 5000 --use-random --verify-passphrase luksFormat /dev/sdxY
cryptsetup luksOpen /dev/sdxY cryptroot
pacstrap /mnt base base-devel linux linux-firmware vim git zsh zsh-completions grub efibootmgr dosfstools os-prober cryptsetup lvm2 mtools sudo intel-ucode mkinitcpio dhcpcd wpa_supplicant networkmanager

genfstab -U /mnt >> /mnt/etc/fstab

arch-chroot /mnt

cp etc/locale.gen /etc/locale.gen
locale-gen

ln -s /usr/share/zoneinfo/Europe/Berlin /etc/localtime

vim /etc/hostname
vim /etc/hosts

cp etc/locale.conf /etc/locale.conf

cp etc/mkinitcpio.conf /etc/mkinitcpio.conf

mkinitcpio -p linux

grub-install --target=x86_64-efi --efi-directory /boot/efi --bootloader-id=GRUB
echo 'GRUB_CMDLINE_LINUX_DEFAULT="rd.luks.name=e4077773-b4da-400d-9bfc-a15e562ba5e0=cryptroot resume=/dev/mapper/System-swap nowatchdog pcie_aspm=force i915.enable_rc6=1 i915.enable_fbc=1 i915.lvds_downclock=1 i915.semaphores=1 mem_sleep_default=deep nmi_watchdog=0 lsm=landlock,lockdown,yama,apparmor,bpf"' >> /etc/default/grub
blkid | grep crypto_LUKS | awk '{print $2}' >> /etc/default/grub
vim /etc/default/grub

grub-mkconfig -o /boot/grub/grub.cfg

echo "Set root password:"
passwd

groupadd sudo
groupadd pfault
useradd -g pfault -G users,games,scanner,power,audio,disk,storage,video,sudo -m -s /bin/zsh pfault
echo "Set pfault password:"
passwd pfault
