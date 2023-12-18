# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

# Color mapping
grey='\[\033[1;30m\]'
red='\[\033[0;31m\]'
RED='\[\033[1;31m\]'
green='\[\033[0;32m\]'
GREEN='\[\033[1;32m\]'
yellow='\[\033[0;33m\]'
YELLOW='\[\033[1;33m\]'
purple='\[\033[0;35m\]'
PURPLE='\[\033[1;35m\]'
white='\[\033[0;37m\]'
WHITE='\[\033[1;37m\]'
blue='\[\033[0;34m\]'
BLUE='\[\033[1;34m\]'
cyan='\[\033[0;36m\]'
CYAN='\[\033[1;36m\]'
NC='\[\033[0m\]'

export PS1="$yellow[$CYAN\u$yellow@$red\H$yellow $GREEN\W$grey$yellow]$NC\$ "

export GPG_TTY=$(tty)

if [ -f /opt/rh/gcc-toolset-12/enable ]; then
    source /opt/rh/gcc-toolset-12/enable
fi

alias e='emacsclient -nw -a "" -c'
export EDITOR='emacsclient -nw -a "" -c'

function man() {
	emacsclient -nw -a "" -c --eval "(progn (man \"$1\") (delete-window))"
}

function emacsdown() {
    emacsclient -nw -a "" -c --eval "(server-stop)"
}

alias bazel=bazelisk

export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

export PATH="${HOME}/mybin/bin":"${HOME}/mybin"${PATH:+":${PATH}"}

# turn the below off.
# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
# End of saving histories from different terminals

# node.js
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# golang
export GOPATH="$HOME/go"; export GOROOT="$HOME/.go"; export PATH="$GOPATH/bin:$PATH"; # g-install: do NOT edit, see https://github.com/stefanmaric/g
export GOBIN=${HOME}/gobin
export PATH=${HOME}/gobin${PATH:+":${PATH}"}

# rust
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# MKL
# if [ -f /opt/intel/oneapi/setvars.sh ] && [ -z "${SETVARS_COMPLETED}" ]; then
#     export MKLROOT=/opt/intel/oneapi/mkl/latest
#     source /opt/intel/oneapi/setvars.sh intel64
# fi

# MOSEK
if [ -d ${HOME}/mosek ]; then
    export MSKHOME=${HOME}
    export CPATH=${MSKHOME}/mosek/10.1/tools/platform/linux64x86/h${CPATH:+":$CPATH"}
    export PATH=${MSKHOME}/mosek/10.1/tools/platform/linux64x86/bin${PATH:+":$PATH"}
    export LD_LIBRARY_PATH=${MSKHOME}/mosek/10.1/tools/platform/linux64x86/bin:${LD_LIBRARY_PATH}
    export LIBRARY_PATH=${MSKHOME}/mosek/10.1/tools/platform/linux64x86/bin:${LIBRARY_PATH}
fi

# texlive
if [ -d ${HOME}/texlive/2022 ]; then
    export PATH=${HOME}/texlive/2022/bin/x86_64-linux${PATH:+":$PATH"}
    export MANPATH=${HOME}/texlive/2022/texmf-dist/doc/man:${MANPATH}
    export INFOPATH=${HOME}/texlive/2022/texmf-dist/doc/info:${INFOPATH}
fi

function deduppath() {
    if [ -n "$PATH" ]; then
        old_PATH=$PATH:; PATH=
        while [ -n "$old_PATH" ]; do
            x=${old_PATH%%:*}       # the first remaining entry
            case $PATH: in
                *:"$x":*) ;;          # already there
                *) PATH=$PATH:$x;;    # not there yet
            esac
            old_PATH=${old_PATH#*:}
        done
        PATH=${PATH#:}
        unset old_PATH x
        export PATH
    fi
}

export PATH="${HOME}/.local/bin:${HOME}/bin:$PATH"

deduppath
