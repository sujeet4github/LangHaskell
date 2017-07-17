# LangHaskell
Language Study Notes - Haskell

To update to github
===================
git
> push
> sujeet4github
> for password use personal access token in keepass

Quickstart
==========
# Use Sandboxes - (Related Warning) - Do not install haskell platform
# Reason why:  https://mail.haskell.org/pipermail/haskell-community/2015-September/000014.html
Older Instructions: README-1-SettingupSandboxesUsingCabal.txt
	the above txt file also has notes on the "CIS194 course"

(Updated - using stack - as per https://github.com/simonmichael/haskell-atom-setup)
1. install stack (Haskell build tool): http://haskell-lang.org/get-started
2. add to path: Eg: echo 'export PATH=\$HOME/.local/bin:\$PATH' >> ~/.bashrc
3. Install a default instance of GHC (Haskell compiler) for your user: stack setup

Editor -> Atom:
==============
1. Install Atom (text editor & IDE): http://atom.io
2. install plugins: Atom Preferences -> Install
	install language-haskell, ide-haskell, ide-haskell-repl and haskell-ghc-mod
	install term3
3. start atom from command line:
	windows: stack exec atom.cmd
	mac: open -a Atom
