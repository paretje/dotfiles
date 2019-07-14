# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
# umask 027

# set UID variable
if [ -z "$UID" ]; then
    export UID=$(id -u)
fi

# set GEM_HOME according to detected location
if hash ruby > /dev/null 2>&1 ; then
    export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
fi

# set ZPLUG_HOME
export ZPLUG_HOME="$HOME/.zplug"

# set GOPATH
export GOPATH="$HOME/.go"

# TODO: just remove these checks, and build the PATH
# set PATH so it includes user's rubygems bin if it exists
if [ "$GEM_HOME" != "" ] ; then
    PATH="$GEM_HOME/bin:$PATH"
fi

# set PATH so it includes user's cabal bin if it exists
if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

# set PATH so it includes user's go bin if it exists
if [ -d "$GOPATH/bin" ] ; then
    PATH="$GOPATH/bin:$PATH"
fi

# set PATH so it includes user's cargo bin if it exists
if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

export NPM_PACKAGES="${HOME}/.npm-packages"
if [ -d "$NPM_PACKAGES/bin" ] ; then
    PATH="$NPM_PACKAGES/bin:$PATH"
fi

# set PATH so it includes user local bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# set PATH so it includes user's zplug bin if it exists
if [ -d "$ZPLUG_HOME/bin" ] ; then
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
if hash gpg-agent > /dev/null 2>&1; then
    if gpg2 --version | fgrep -q 'gpg (GnuPG) 2.0'; then
        export GPG_ENV_FILE="$HOME/.gpg-agent-info"
        . "$GPG_ENV_FILE"
        export GPG_AGENT_INFO
        if ! gpg-agent; then
            gpg-agent --daemon --enable-ssh-support --write-env-file "$GPG_ENV_FILE"
        fi
    else
        gpgconf --launch gpg-agent
        if [ -d "/var/run/user/$UID" ]; then
            export SSH_AUTH_SOCK="/var/run/user/$UID/gnupg/S.gpg-agent.ssh"
        else
            export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
        fi
    fi
fi

# set default editor, pager, ...
export EDITOR=vi
export PAGER=less
export BROWSER=qutebrowser

# set default ledger file
export LEDGER_FILE="$HOME/vcs/personal/accounting/balance.journal"

# set HOST variable
export HOST=$(hostname)

# set default ant options
export ANT_ARGS="-logger org.apache.tools.ant.listener.AnsiColorLogger -emacs"

# set name and email address
export NAME="Kevin Velghe"
if [ "$HOST" = "parsley" -o "$HOST" = "fennel" -o "$HOST" = "chervil" ]; then
    export EMAIL="kevin.velghe@senso2.me"
else
    export EMAIL="kevin@paretje.be"
fi

# set key to use to sign packages
if [ "$HOST" = "parsley" -o "$HOST" = "chervil" ]; then
    export DEB_SIGN_KEYID="E672C080A81F2D8CCDF0198C438CD95296C1A48A"
else
    export DEB_SIGN_KEYID="A00FD8ECD1BC0694C8ED1C835473109364AD7E10"
fi

# set default org refile file
if [ "$HOST" = "parsley" -o "$HOST" = "chervil" ]; then
    export ORG_REFILE="$HOME/vcs/senso2me/notes/s2m-refile.org"
else
    export ORG_REFILE="$HOME/vcs/personal/notes/refile.org"
fi

# mail directory
export MAILDIR="$HOME/.mail"

# less options
# TODO: what about other options of alias?
export LESS="-FRXi"

# default prefix in Authorize header using httpie + httpie-jwt-aut
export JWT_AUTH_PREFIX='JWT'

# define mail accounts used on different devices
if [ "$HOST" = "parsley" -o "$HOST" = "chervil" ]; then
    export MAIL_ACCOUNTS="senso2me"
elif [ "$HOST" = "kevin-laptop" ]; then
    export MAIL_ACCOUNTS="prive notes"
fi

# disable system install from pip
export PIP_USER="yes"

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
