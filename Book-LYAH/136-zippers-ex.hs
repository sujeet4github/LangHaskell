
{-
Our file system also has a lot of cases where an operation could fail,
such as trying to focus on a file or folder that doesn't exist.
As an exercise, you can equip our file system with functions that
fail gracefully by using the Maybe monad
 -}
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
fsTo :: Name -> FSZipper -> FSZipper
fsRename :: Name -> FSZipper -> FSZipper
fsNewFile :: FSItem -> FSZipper -> FSZipper

-- TODO
