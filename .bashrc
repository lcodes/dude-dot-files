# -*- mode: Shell-script; tab-width: 4; fill-column: 80; -*-

# Written by Jeremie Pelletier <jeremiep@gmail.com>
#
# License: Public Domain


# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# Platform detection
# ==================

platform=`uname`
case $platform in
    Darwin)  isOSX=yes;;
    Linux)   isLinux=yes;;
    FreeBSD) isBSD=yes;;
    Cygwin)  isWindows=yes;;
esac


# Terminal capabilities
# =====================

case "$TERM" in
    xterm-*color) termColors=yes;;
esac


# Environment Variables
# =====================

if [[ $termColors == yes ]]; then
    export PS1='\[\e[1;32m\]\u@\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0m\]\$ '
else
    export PS1='\u@\h:\w\$ '
fi

export JAM=jeremiep@jam.local
export JAL=jeremie@jal.local
export JAW=jeremie@jaw.local
export DUDE=jeremie@dudebox

if [ -d /opt/local/bin ]; then
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
fi

if [ -d ~/.cabal/bin ]; then
    export PATH=~/.cabal/bin:$PATH
fi


# Aliases
# =======

if [[ $termColors == yes ]]; then
    if [[ $isOSX == yes || $isBSD == yes ]]; then
        alias ls='ls -G'
        alias grep='grep -G'
        alias fgrep='fgrep -G'
        alias egrep='egrep -G'
    else
        alias ls='ls --color=auto'
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    fi
fi

alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'

alias jhelp='./confiugre --help'
alias jconf='./configure'
alias jmake='make -j 4' # TODO: from cpu-count
alias jinstall='sudo make install -j 2'

alias sshm='ssh $JAM'
alias sshl='ssh $JAL'
alias sshw='ssh $JAW'
alias sshdude='ssh $DUDE'


# Bash settings
# =============

shopt -s histappend
shopt -s checkwinsize

HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000
HISTFILESIZE=2000

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# lesspipe
# ========

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# D Programming Language
# ======================

if [[ -d "~/dmd2" ]]; then
    case $platform in
        Darwin) dirname=osx;;
        Linux) dirname=linux;;
        FreeBSD) dirname=bsd;;
        Windows) dirname=win32;;
    esac
    export PATH=~/dmd2/$dirname/bin:$PATH
fi

