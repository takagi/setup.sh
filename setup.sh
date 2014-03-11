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
sbcl=0
if [ $sbcl -eq 1 ] ; then
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
sbcl_source=0
if [ $sbcl_source -eq 1 ] ; then
  cd
  wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.58-source.tar.bz2
  bzip2 -cd sbcl-1.0.58-source.tar.bz2 | tar xvf -
  mv sbcl-1.0.58 ~/Lisp/sbcl-1.0.58-source
  rm sbcl-1.0.58-source.tar.bz2
fi

# Setup Quicklisp
quicklisp=0
if [ $quicklisp -eq 1 ] ; then
  cd
  wget http://beta.quicklisp.org/quicklisp.lisp
  sbcl --load quicklisp.lisp
  rm quicklisp.lisp
fi

# Configure .sbclrc
sbclrc=0
if [ $sbclrc -eq 1 ] ; then
  echo ";;; Quicklisp shortcut
(defun :ql (&rest args)
  (apply #'ql:quickload args))

;;; SBCL source location
(sb-ext:set-sbcl-source-location \"~/Lisp/sbcl-1.0.58-source\")
" >> .sbclrc
fi

# Setup Slime
slime=0
if [ $slime -eq 1 ] ; then
  cd
  mkdir ~/.emacs.d
  wget http://common-lisp.net/project/slime/snapshots/slime-current.tgz
  tar xvzf slime-current.tgz
  rm slime-current.tgz
  mv slime-* ~/.emacs.d/slime
fi

# Configure .emacs
dotemacs=0
if [ $dotemacs -eq 1 ] ; then
  cd
  echo ";; Set SBCL as inferior lisp program and setup SLIME
(setq inferior-lisp-program \"sbcl\")
(add-to-list 'load-path (expand-file-name \"~/.emacs.d/slime\"))
(require 'slime)
(slime-setup '(slime-repl))
" >> .emacs
fi

# Setup Git
apt-get install -y git

# Setup cl-pattern
cl_pattern=0
if [ $cl_pattern -eq 1 ] ; then
  cd ~/Lisp/quicklisp/local-projects
  git clone git://github.com/arielnetworks/cl-pattern.git
  cd
fi

