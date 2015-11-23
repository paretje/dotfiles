# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
# umask 027

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's cabal bin if it exists
if [ -d "$HOME/.cabal/bin" ] ; then
	PATH="$PATH:$HOME/.cabal/bin"
fi

# set PATH so it includes user's rubygems bin if it exists
if [ -d "$HOME/.gem/ruby/1.9.1/bin" ] ; then
	PATH="$PATH:$HOME/.gem/ruby/1.9.1/bin"
fi
if [ -d "$HOME/.gem/ruby/2.0.0/bin" ] ; then
	PATH="$PATH:$HOME/.gem/ruby/2.0.0/bin"
fi
if [ -d "$HOME/.gem/ruby/2.1.0/bin" ] ; then
	PATH="$PATH:$HOME/.gem/ruby/2.1.0/bin"
fi

# Start gpg-agent if not yet running
export GPG_ENV_FILE="$HOME/.gpg-agent-info"
. "$GPG_ENV_FILE"
export GPG_AGENT_INFO
if ! gpg-agent; then
	gpg-agent --daemon --enable-ssh-support --write-env-file "$GPG_ENV_FILE"
fi

# Set the appropriate VDPAU driver to use
if [ "$(hostname)" = "kevin-laptop" ]; then
	export VDPAU_DRIVER="va_gl"
fi
if [ "$(hostname)" = "kevin-desktop" ]; then
	export VDPAU_DRIVER="nouveau"
fi

# Use GTK style in Qt5 applications
export QT_STYLE_OVERRIDE=gtk

# Fool Qt we're using gnome in order to get themes icons
# Remember that this requires setting the gconf settings, otherwise you'll get
# default gnome styling: no icons ... 
export DESKTOP_SESSION=gnome

# set PYTHONPATH
if [ -d "$HOME/.vim/bundle/ropevim" ] ; then
	export PYTHONPATH="$PYTHONPATH:$HOME/.vim/bundle/ropevim"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
	# include .bashrc if it exists
	if [ -f "$HOME/.bashrc" ]; then
		. "$HOME/.bashrc"
	fi
fi

# set default editor and pager
export EDITOR=vi
export PAGER=less

# set default man path
export MANPATH=$(manpath)

# set MANPATH so it includes user's private man's if it exists
if [ -d "$HOME/.local/man" ]; then
	MANPATH="$HOME/.local/man:$MANPATH"
fi

# set MANPATH so it includes user's rubygems man's if it exists
if [ -d "$HOME/.gem/ruby/2.1.0/gems" ] ; then
	if [ -n "$(find $HOME/.gem/ruby/2.1.0/gems -maxdepth 2 -path $HOME/.gem/ruby/2.1.0/gems'/*/man' -print -quit)" ]; then
		for dir in $HOME/.gem/ruby/2.1.0/gems/*/man ; do
			MANPATH="$MANPATH:$dir"
		done
	fi
fi

# set default ledger file
export LEDGER_FILE="$HOME/vcs/personal/accounting/balance.journal"

# add system binary locations
PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin"
