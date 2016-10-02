# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
# umask 027

# set GEM_HOME according to detected location
if hash ruby ; then
    export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
fi

# set ZPLUG_HOME
export ZPLUG_HOME="$HOME/.zplug"

# set PATH so it includes user's rubygems bin if it exists
if [ "$GEM_HOME" != "" ] ; then
    PATH="$GEM_HOME/bin:$PATH"
fi

# set PATH so it includes user's cabal bin if it exists
if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

# set PATH so it includes user local bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# set PATH so it includes user's zplug bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$ZPLUG_HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# add system binary locations for non-root users
if [ "$UID" != "0" ]; then
    PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin"
fi

# set default man path
export MANPATH=$(manpath)

# set MANPATH so it includes user's private man's if it exists
if [ -d "$HOME/.local/share/man" ]; then
    MANPATH="$HOME/.local/share/man:$MANPATH"
fi

# set MANPATH so it includes user's zplug man's if it exists
if [ -d "$HOME/.zplug/doc/man" ]; then
    MANPATH="$HOME/.zplug/doc/man:$MANPATH"
fi

# set MANPATH so it includes user's rubygems man's if it exists
if [ -d "$GEM_HOME/gems" ] ; then
    if [ -n "$(find $GEM_HOME/gems -maxdepth 2 -path $GEM_HOME/gems'/*/man' -print -quit)" ]; then
        for dir in $GEM_HOME/gems/*/man ; do
            MANPATH="$MANPATH:$dir"
        done
    fi
fi

# Start gpg-agent if not yet running
if hash gpg-agent; then
    if gpg2 --version | fgrep -q 'gpg (GnuPG) 2.0'; then
        export GPG_ENV_FILE="$HOME/.gpg-agent-info"
        . "$GPG_ENV_FILE"
        export GPG_AGENT_INFO
        if ! gpg-agent; then
            gpg-agent --daemon --enable-ssh-support --write-env-file "$GPG_ENV_FILE"
        fi
    else
        gpgconf --launch gpg-agent
        export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
    fi
fi

# set default editor and pager
export EDITOR=vi
export PAGER=less

# set default ledger file
export LEDGER_FILE="$HOME/vcs/personal/accounting/balance.journal"

# set HOST variable
export HOST=$(hostname)

# set default ant options
export ANT_ARGS="-logger org.apache.tools.ant.listener.AnsiColorLogger -emacs"

# set name and email address
export NAME="Kevin Velghe"
export EMAIL="kevin@paretje.be"

# set key to use to sign packages
export DEB_SIGN_KEYID="64AD7E10"

# if this is tty1, start X server
if [ "$TTY" = "/dev/tty1" ]; then
    exec startx
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
