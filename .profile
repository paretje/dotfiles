# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 027

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

# Start gpg-agent if not yet running
if [ ! -f "$HOME/.gpg-agent-info" ]; then
	gpg-agent --daemon --enable-ssh-support --write-env-file "$HOME/.gpg-agent-info"
fi

# Set the appropriate VDPAU driver to use
if [ "$(hostname)" = "kevin-laptop" ]; then
	export VDPAU_DRIVER="va_gl"
fi
if [ "$(hostname)" = "kevin-desktop" ]; then
	export VDPAU_DRIVER="nouveau"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi
