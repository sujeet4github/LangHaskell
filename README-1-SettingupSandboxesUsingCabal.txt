Initial setup (no GHC install directly, use cabal)
Note: superseded this - using stack - more current (see README.md)

1. mkdir foo && cd foo
2. cabal sandbox init
3. cabal init
4. echo 'main = putStrLn "Hello World"' > Main.hs
5. touch LICENSE
6. sed -i 's/-- main-is:/main-is: Main.hs/' foo.cabal
7. cabal configure && cabal build && cabal run


Instead: See the install instructions for how to get GHC & Cabal installed.
https://github.com/bitemyapp/learnhaskell/blob/master/install.md


Sandbox Creation (For CIS194 learning)
================
Sandbox created at top level directory as per instructions in Quickstart below.
To start repl:
		cabal repl
To load a file
		:l  cis194-fall14\hw\HW06_Ex1.hs
	Or, Better
		:cd cis194-fall14\\hw\\
		:l HW06.hs
		:!pwd  -- to verify current directory
Libraries Added in sandbox:
1. aeson and text added for cis194-fall14 - HW06
2. random added for cis194-fall14 - HW07
3. hunit and quickcheck for cis194-fall14 - week 9
4. Gloss for cis194-fall14 - Week 10
	To fix - failed due to dependency on GLUT:
	use freeglut: this is a OpenGL library for use with MSVC & MinGW
	1. download Glut (from freeglut - http://www.transmissionzero.co.uk/software/freeglut-devel/)
		in C:\s\opt\freeglut
	2. I made a copy of C:\S\opt\devtools\freeglut\bin\x64\freeglut.dll in the same directory, naming it glut32.dll.
	3. install glut using this command line:
		cabal --extra-lib-dirs=C:\S\opt\devtools\freeglut\bin\x64 install GLUT
	4. cabal install gloss
	5. CABAL build is not working
	apparently setting up GHC platform we dont have to do all these
