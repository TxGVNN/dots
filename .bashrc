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
PS1="\$(__service_ps)\n\[\e[0;${color}m\]+\[\e[1;30m\](\[\e[0;${color}m\]\u\[\e[0;36m\]@\h\[\e[1;30m\])\$(if [[ \$? == 0 ]]; then echo \"\[\e[1;32m\]-\"; else echo \"\[\e[1;31m\]-\"; fi)\[\e[0m\]\[\e[1;30m\](\[\e[0;34m\]\w\[\e[1;30m\])-(\[\e[0;33m\]\t\[\e[1;30m\]\[\e[1;30m\])\$(__git_ps1)\n\[\e[0;${color}m\]+>\[\e[0m\]"

function __service_ps(){
    local ret=$?
    # torsock on
    if env | grep torsocks -q ; then
        printf "-\e[1;30m(\e[1;30mtor\e[1;30m)\e[0m"
    fi
    return $ret
}
function ps1(){
    if [[ $PS1 != *"$1"* ]]; then
        PS1="-\[\e[1;30m\](\[\e[0;35m\]"$1"\[\e[1;30m\])\[\e[0m\]"$PS1
    fi
}

declare -f "cdenv" > /dev/null || function cdenv(){
    if [ -z "$1" ]; then
        cd || exit 1
    else
        cd "$1"
    fi

    # .bin
    if [ -e .bin ]; then
        if [[ $PATH != *"$(pwd)/.bin"* ]]; then
            ps1 ".bin"
            export PATH=$(pwd)/.bin:$PATH
        fi
        if [ -e .bin/env ]; then
            . .bin/env
        fi
    fi

    # Makefile
    if [ -e Makefile ]; then
        ps1 "make"
    else
        PS1=$(echo $PS1 | sed 's/-\\\[\\e\[1;30m\\\](\\\[\\e\[0;35m\\\]make\\\[\\e\[1;30m\\\])//g')
    fi

    # virtualenv
    if [ -e bin ]; then
        if [[ $PATH != *"$(pwd)/bin"* ]]; then
            ps1 bin
            export PATH=$(pwd)/bin:$PATH
        fi
        if [ -e bin/activate ]; then
            . bin/activate
        fi
    fi

    # vagrant
    if [ -e Vagrantfile ]; then
        if [[ $PS1 != *"vagrant"* ]]; then
            ps1 vagrant
        fi
    else
        PS1=$(echo $PS1 | sed 's/-\\\[\\e\[1;30m\\\](\\\[\\e\[0;35m\\\]vagrant\\\[\\e\[1;30m\\\])//g')
    fi
}

function cdtmp(){
    cd "$(mktemp -d -t ${USER}_$(date +%F_%H-%I)_XXX)" || exit 1
}

function lstmp(){
    ls /tmp/"$USER"*
}

function mkcd(){
    if [ $# -ne 1 ]; then
        echo "Usage: mkcd DIR"
    fi
    mkdir "$1" && cd "$1"
}

# SSH and screen
function sshscreen(){
    ssh "$@" -v -t 'if screen -ls | grep gtx -q ; then screen -x gtx ;else screen -S gtx ;fi'
}

# SSH and screen
function sshtmux(){
    ssh "$@" -v -t 'if tmux ls | grep gtx -q ; then tmux at -t gtx ;else tmux new -s gtx ;fi'
}

# Alias
alias cd="cdenv"
alias em="emacs -nw"
# Export
# export HISTTIMEFORMAT="%F %T "
## Check pseudoterminal or not?
export TERM=xterm-256color
if [[ $(tty) != */dev/pts/* ]]; then
    export TERM=linux
fi
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
