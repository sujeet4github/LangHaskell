-- http://learnyouahaskell.com/zippers
--
import Tree

-- if we start at the root of the tree and move either left or right
-- one step at a time and sort of leave breadcrumbs?
type SimpleBreadcrumbs = Directions
-- different type than Directions defined in module Tree, directions
-- in this list will now be reversed, since we are leaving them as we go
-- down the list

goLeft1 :: (Tree t, SimpleBreadcrumbs) -> (Tree t, SimpleBreadcrumbs)
goLeft1 (Node _ l _, bs) = (l, L:bs)

goRight1 :: (Tree t, SimpleBreadcrumbs) -> (Tree t, SimpleBreadcrumbs)
goRight1 (Node _ _ r, bs) = (r, R:bs)

-- allows us to apply functions to values by first writing the value,
-- then writing a -: and then the function
x -: f = f x

testIt =  (freeTree, []) -: goRight1 -: goLeft1

{-
What if we now want to go back up in our tree?
From our breadcrumbs we know that the current tree is the left sub-tree
of its parent and that it is the right sub-tree of its parent, but that's
it. They don't tell us enough about the parent of the current sub-tree
for us to be able to go up in the tree. It would seem that apart from the
direction that we took, a single breadcrumb should also contain all other
data that we need to go back up. In this case, that's the element in the
parent tree along with its right sub-tree.

In general, a single breadcrumb should contain all the data needed to
reconstruct the parent node. So it should have the information from all
the paths that we didn't take and it should also know the direction that
we did take, but it must not contain the sub-tree that we're currently
focusing on. That's because we already have that sub-tree in the first
component of the tuple, so if we also had it in the breadcrumbs, we'd
have duplicate information.

-}

data Crumb a = LeftCrumb a (Tree a)
                | RightCrumb a (Tree a)
                deriving Show

-- In essence, every breadcrumb is now like a tree node with a hole in it.
-- When we move deeper into a tree, the breadcrumb carries all the
-- information that the node that we moved away from carried except the
-- sub-tree that we chose to focus on. It also has to note where the hole
-- is. In the case of a LeftCrumb, we know that we moved left, so the
-- sub-tree that's missing is the left one.

type Breadcrumbs a = [Crumb a]

goLeft2 :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft2 (Node v l r, bs) = (l, (LeftCrumb v r):bs)

goRight2 :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight2 (Node v l r, bs) = (r, (RightCrumb v l):bs)

goUp1 :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp1 (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp1 (t, RightCrumb x l:bs) = (Node x l t, bs)

{-
With a pair of Tree a and Breadcrumbs a, we have all the information to
rebuild the whole tree and we also have a focus on a sub-tree. This scheme
also enables us to easily move up, left and right. Such a pair that
contains a focused part of a data structure and its surroundings is called
a zipper, because moving our focus up and down the data structure resembles
the operation of a zipper on a regular pair of pants.
So it's cool to make a type synonym as such:
-}
type Zipper a = (Tree a, Breadcrumbs a)

-- take a tree and a zipper and return a new zipper that has its focus
-- replaced with the supplied tree.
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)
-- if we now go up, the tree t will be attached to the original tree,
-- replacing what was there before

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost $ goUp1 z

-- handling failures
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just(l, LeftCrumb x r:bs)
goLeft (Empty, _)       = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just(r, RightCrumb x l:bs)
goRight (Empty, _)       = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs)  = Just(Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just(Node x l t, bs)
goUp (_, [])                = Nothing

--
-- to do the following we need monadic operators now, use >>= instead of -:
-- let newFocus = (freeTree,[]) -: goLeft -: goRight
-- let newFocus = return (freeTree,[]) >>= goLeft >>= goRight
-- We used return to put a zipper in a Just and then used >>= to feed
-- that to our goRight function
-------------------------------------------------------

-- Zippers can be used with pretty much any data structure...
type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs,bc) = (xs,x:bc)
goBack :: ListZipper a -> ListZipper a
goBack (xs,bc:bcs) = (bc:xs,bcs)

-------------------------------------------------------

{-
A Very Simple File System
-------------------------

-}

type Name = String
type Data = String
data FSItem = File Name Data
                | Folder Name [FSItem]
                deriving Show

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

-- Bread Crumb should contain
-- 1. the name of parent folder
-- 2. fsItems prior to the item on focus
-- 3. fsItems after the item on focus
-- By keeping separate lists for the items that come before the item
-- that we're focusing and for the items that come after it, we know
-- exactly where to place it once we move back up. So this way, we
-- know where the hole is.
data FSCrumb = FSCrumb {
                    parentFolder    :: Name,
                    preItems        ::[FSItem],
                    postItems       ::[FSItem]
                }
                deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- searches for a file in the current folder
-- break takes a predicate and a list and returns a pair of lists
--  fst is the list for which the predicate returned False
--  snd is the remainder of the list, starting with the first item that the
--   predicate was True
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, cs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:cs)

-- Let's start at the root and walk to the file "skull_man(scary).bmp":
-- let newFocus = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- fst newFocus
-- Let's move up and then focus on its neighboring file "watermelon_smash.gif":
-- let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"
-- fst newFocus2


-- rename the currently focussed file or folder
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

-- rename our "pics" folder to "cspi":
-- let newFocus = (myDisk,[]) -: fsTo "pics" -: fsRename "cspi" -: fsUp

-- new item in the current folder
-- would crash if the current focussed item were not a folder
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)

-- Let's add a file to our "pics" folder and then move back up to the root
-- let newFocus = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp
