# TxGVNN/dots
# $cat this-file >> ~/.bashrc
##PS1
color='32'
if [ "$(id -u)" -eq 0 ]; then
    color='31'
fi
if ! declare -f "__git_ps1" >/dev/null; then
    function __git_ps1(){ echo "";}
fi
export GIT_PS1_SHOWDIRTYSTATE=true
PS1="\n\[\e[0;${color}m\]\342\224\214\[\e[1;30m\](\[\e[0;${color}m\]\u\[\e[0;35m\]@\h\[\e[1;30m\])\$(if [[ \$? == 0 ]]; then echo \"\[\e[6;32m\]\342\224\200\"; else echo \"\[\e[6;31m\]\342\224\200\"; fi)\[\e[0m\]\[\e[1;30m\](\[\e[0;34m\]\w\[\e[1;30m\])\342\224\200(\[\e[0;33m\]\t\[\e[1;30m\]\[\e[1;30m\])\$(__git_ps1)\n\[\e[0;${color}m\]\342\224\224>\[\e[0m\]"

function cdenv(){
    if [ -z "$1" ]; then
        cd || exit 1
    else
        cd "$1"
    fi

    # .bin my-environment
    if [ -e .bin ]; then
        if [[ $PATH != *"$(pwd)/.bin"* ]]; then
            PS1="\342\224\200\e[1;30m\](\e[0;35m\].bin\e[1;30m\])\e[0m\]"$PS1
            export PATH=$(pwd)/.bin:$PATH
        fi
        if [ -e .bin/env ]; then
            . .bin/env
        fi
    fi

    # virtualenv
    if [ -e bin ]; then
        if [[ $PATH != *"$(pwd)/bin"* ]]; then
            PS1="\342\224\200\e[1;30m\](\e[0;35m\]bin\e[1;30m\])\e[0m\]"$PS1
            #export PATH=$(pwd)/bin:$PATH
        fi
        if [ -e bin/activate ]; then
            . bin/activate
        fi
    fi
    # docker compose
    if [ -e docker-compose.yml ]; then
        if [[ $PS1 != *"d-compose"* ]]; then
            PS1="\342\224\200\e[1;30m\](\e[0;35m\]d-compose\e[1;30m\])\e[0m\]"$PS1
        fi
    else
        PS1=$(echo $PS1 | sed 's/\\342\\224\\200\\e\[1;30m\\\](\\e\[0;35m\\\]d-compose\\e\[1;30m\\\])//g')
    fi
    # vagrant
    if [ -e Vagrantfile ]; then
        if [[ $PS1 != *"vagrant"* ]]; then
            PS1="\342\224\200\e[1;30m\](\e[0;35m\]vagrant\e[1;30m\])\e[0m\]"$PS1
        fi
    else
        PS1=$(echo $PS1 | sed 's/\\342\\224\\200\\e\[1;30m\\\](\\e\[0;35m\\\]vagrant\\e\[1;30m\\\])//g')
    fi
}
# Alias
alias cd="cdenv"
alias em="emacs -nw"
# Export
export HISTTIMEFORMAT="%F %T "
## Check pseudoterminal or not?
export TERM=xterm-256color
if [[ $(tty) != */dev/pts/* ]]; then
    export TERM=linux
fi
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export VAGRANT_DEFAULT_PROVIDER=libvirt
