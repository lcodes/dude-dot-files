# Install emacs
cd ~
apt-get install -y emacs
[ ! -d .dude-dot-files ] && \
    git clone --recursive git://github.com/ddude/dude-dot-files.git .dude-dot-files
[ ! -f .emacs ] && \
    ln -s .dude-dot-files/.emacs .emacs
[ ! -d .site-lisp ] && \
    ln -s .dude-dot-files/.site-lisp .site-lisp
[ ! -f .bashrc ] && \
    ln -s .dude-dot-files/.bashrc .bashrc
[ ! -f .bash_profile ] && \
    ln -s .dude-dot-files/.bash_profile .bash_profile