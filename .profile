# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Add locally installed software using software-specific package management
PATH="$PATH:$HOME/.cabal/bin:$HOME/.gem/ruby/1.9.1/bin"

# Start gpg-agent if not yet running
if [ ! -f "$HOME/.gpg-agent-info" ]; then
	gpg-agent --daemon --enable-ssh-support --write-env-file "$HOME/.gpg-agent-info"
fi

export WWW_HOME="https://duckduckgo.com/?kl=be-nl&kp=-1"
