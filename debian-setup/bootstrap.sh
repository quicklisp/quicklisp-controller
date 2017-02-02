#!/bin/bash -e

SBCL="sbcl --noinform --no-userinit --no-sysinit --non-interactive"
QUICKLISP="$SBCL --load $HOME/quicklisp/setup.lisp"

# Non-debian packages bundled up from hornbeam
cd "$HOME"
wget https://www.quicklisp.org/quicklisp-controller/packages.tar
tar xvf packages.tar
su root -c 'dpkg -i packages/*.deb'


# Quicklisp setup
wget https://beta.quicklisp.org/quicklisp.lisp
$SBCL --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)'

# Quicklisp-controller setup
su root -c 'apt-get -y install git'
cd quicklisp/local-projects/
git clone https://github.com/quicklisp/quicklisp-controller.git
cd quicklisp-controller/debian-setup
su root -c 'apt-get -y install `cat debian-8-packages.txt`'


# Other required git repos
cd "$HOME"/quicklisp/local-projects/

git clone https://github.com/quicklisp/project-info.git
git clone https://github.com/xach/commando.git
git clone https://github.com/xach/githappy.git

cd "$HOME"

git clone https://github.com/quicklisp/quicklisp-projects.git


# Initial setup
$QUICKLISP \
     --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
     --eval '(ql:quickload "quicklisp-controller")' \
     --eval '(quicklisp-controller:setup-directories "~/quicklisp-projects/")'


echo "(require 'sb-posix)" >> .sbclrc
echo "(sb-posix:setenv \"PATH\" \"/bin:/usr/bin:/usr/local/bin:$HOME/bin\" 1)" >> .sbclrc

# Emacs setup
$QUICKLISP --eval '(ql:quickload "quicklisp-slime-helper")'

echo '(load (expand-file-name "~/quicklisp/slime-helper.el"))' >> "$HOME"/.emacs
echo '(setq inferior-lisp-program "sbcl")' >> "$HOME/".emacs

echo "Ready for slime"



