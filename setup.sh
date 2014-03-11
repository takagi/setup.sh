###
### Setup script for my own work on Ubuntu 32bit
###
### Components this script sets up are:
### - ssh
### - Emacs
### - SBCL, a Common Lisp implementation
### - Quicklisp, a library manager for Common Lisp
### - Slime, a Lisp interaction mode for Emacs
### - Git
### - cl-pattern, a pattern-matching library for Common Lisp
###

# Setup ssh
apt-get install -y ssh

# Setup Emacs
#apt-get install -y gcc
#apt-get install -y make
apt-get install -y emacs

# Setup Lisp directory
cd
mkdir -p Lisp

# Setup SBCL binary
if [ ! `which sbcl` ] ; then
  cd
  wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.58-x86-linux-binary.tar.bz2
  bzip2 -cd sbcl-1.0.58-x86-linux-binary.tar.bz2 | tar xvf -
  rm sbcl-1.0.58-x86-linux-binary.tar.bz2
  cd sbcl-1.0.58-x86-linux
  bash install.sh
  cd ..
  rm -rf sbcl-1.0.58-x86-linux
fi

# Setup SBCL source
cd
if [ ! -e Lisp/sbcl-1.0.58-source ] ; then
  wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.58-source.tar.bz2
  bzip2 -cd sbcl-1.0.58-source.tar.bz2 | tar xvf -
  mv sbcl-1.0.58 ~/Lisp/sbcl-1.0.58-source
  rm sbcl-1.0.58-source.tar.bz2
fi

# Setup Quicklisp
cd
if [ ! -e Lisp/quicklisp ] ; then
  wget http://beta.quicklisp.org/quicklisp.lisp
  sbcl --load quicklisp.lisp
  rm quicklisp.lisp
fi

# Configure .sbclrc
cd
if [ ! -e .sbclrc ] ; then
  echo ";;; Quicklisp shortcut
(defun :ql (&rest args)
  (apply #'ql:quickload args))

;;; SBCL source location
(sb-ext:set-sbcl-source-location \"~/Lisp/sbcl-1.0.58-source\")
" >> .sbclrc
fi

# Setup Slime
cd
if [ ! -e .emacs.d/ ] ; then
  mkdir ~/.emacs.d
  wget http://common-lisp.net/project/slime/snapshots/slime-current.tgz
  tar xvzf slime-current.tgz
  rm slime-current.tgz
  mv slime-* ~/.emacs.d/slime
fi

# Configure .emacs
cd
if [ ! -e .emacs ] ; then
  echo ";; don't make backup files, such as foo~
(setq-default make-backup-files nil)

;; use spaces to indent lines
(setq-default indent-tabs-mode nil)

;; set SBCL as inferior lisp program and setup SLIME
(setq inferior-lisp-program \"sbcl\")
(add-to-list 'load-path (expand-file-name \"~/.emacs.d/slime\"))
(require 'slime)
(slime-setup '(slime-repl))
" >> .emacs
fi

# Setup Git
apt-get install -y git

# Setup cl-pattern
cd
if [ ! -e Lisp/quicklisp/local-projects/cl-pattern ] ; then
  cd ~/Lisp/quicklisp/local-projects
  git clone git://github.com/arielnetworks/cl-pattern.git
  cd
fi
