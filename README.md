# LangHaskell
Language Study Notes - Haskell

Quickstart
----------
1. mkdir foo && cd foo
2. cabal sandbox init
3. cabal init
4. echo 'main = putStrLn "Hello World"' > Main.hs
5. touch LICENSE
6. sed -i 's/-- main-is:/main-is: Main.hs/' foo.cabal
7. cabal configure && cabal build && cabal run

# (Related Warning) - Do not install haskell platform

Reason why:  https://mail.haskell.org/pipermail/haskell-community/2015-September/000014.html

Instead: See the install instructions for how to get GHC & Cabal installed.
https://github.com/bitemyapp/learnhaskell/blob/master/install.md
