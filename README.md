## GNU Emacs

```
wget -O ~/.emacs https://raw.githubusercontent.com/txgvnn/dots/master/.emacs
```

Just start and enjoy Emacs

*Optional*
 * <kbd>M-x</kbd> <kbd>develop-dot</kbd> Diff new .emacs version
 * <kbd>M-x</kbd> <kbd>develop-go</kbd> Golang development
 * <kbd>M-x</kbd> <kbd>develop-python</kbd> Python development
 * <kbd>M-x</kbd> <kbd>develop-php</kbd> PHP development
 * <kbd>M-x</kbd> <kbd>develop-terraform</kbd> Terraform development
 * <kbd>M-x</kbd> <kbd>develop-ansible</kbd> Ansible development
 * <kbd>M-x</kbd> <kbd>develop-docker</kbd> Docker development
 * <kbd>M-x</kbd> <kbd>develop-vagrant</kbd> Vagrant development
 * <kbd>M-x</kbd> <kbd>develop-kubernetes</kbd> Kubernertes development
 * <kbd>M-x</kbd> <kbd>develop-...</kbd> ... development

### Email with Gnus & smtpmail-multi

```
wget -O ~/.gnus https://raw.githubusercontent.com/txgvnn/dots/master/.gnus
wget -O ~/.authinfo https://raw.githubusercontent.com/txgvnn/dots/master/.authinfo
```
You have to customize the `.gnus` and `.authinfo` (default 2 accounts) with your credentials.

Setup `.authinfo.gpg`(by gnupg) instead of `.authinfo` if you want to protect your sensitive data.

## Screen

```
wget -O ~/.screenrc https://raw.githubusercontent.com/txgvnn/dots/master/.screenrc
```

## Bash

```
wget -O- https://raw.githubusercontent.com/txgvnn/dots/master/.bashrc >> ~/.bashrc
```
