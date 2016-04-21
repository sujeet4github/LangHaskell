{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/10-gloss.html

Suggested Reading:
The gloss Package
The gloss GitHub repo, with lots of examples

https://github.com/benl23x5/gloss

Example:
import Graphics.Gloss
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)

-}
import Graphics.Gloss
import Data.Monoid

main = display (InWindow "Hello, world!" (200, 200) (200, 200))
               white
               (circle 50 <>
                (translate (-20) 10    $ circle 10) <>
                (translate 20    10    $ circle 10) <>
                (translate 0     (-15) $ scale 1 0.7 $ arc 180 360 20))