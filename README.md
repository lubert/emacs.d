## Requirements

* Emacs 24.5 or greater

## Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

    git clone https://github.com/lubert/emacs.d.git ~/.emacs.d

## Emacs server

To run emacs as a client/server to reduce startup time, add the
following to your `.bashrc` or `.bash_profile`:

    alias emacs='emacsclient -nw -c -a ""'
    export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient
