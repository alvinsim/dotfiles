# $FreeBSD: releng/11.0/share/skel/dot.profile 278616 2015-02-12 05:35:00Z cperciva $
#
# .profile - Bourne Shell startup script for login shells
#
# see also sh(1), environ(7).
#

# These are normally set through /etc/login.conf.  You may override them here
# if wanted.
# PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$HOME/bin; export PATH
# BLOCKSIZE=K;	export BLOCKSIZE

# Setting TERM is normally done through /etc/ttys.  Do only override
# if you're sure that you'll never log in via telnet or xterm or a
# serial line.
# TERM=xterm; 	export TERM

EDITOR=vi;   	export EDITOR
PAGER=more;  	export PAGER

# set ENV to a file invoked each time sh is started for interactive use.
ENV=$HOME/.shrc; export ENV

if [ -x /usr/bin/fortune ] ; then /usr/bin/fortune freebsd-tips ; fi

# export TERM to dtterm to eupport emacs
export TERM=xterm-256color

# change value of PS1 to show git status of a git repo
source /usr/local/share/git-core/contrib/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_HIDE_IF_PWD_IGNORED=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export PS1='\u@\h \w$(__git_ps1 " (%s)") \$ '

# set LANG to use UTF-8 for powerline-fonts to work in tmux
export LANG=en_US.UTF-8
#export LC_CTYPE=en_US.UTF8

# set JAVA_HOME
export JAVA_HOME=/usr/local/openjdk12
export PATH=$PATH:$JAVA_HOME
