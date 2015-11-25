{-
You need a monad transformer to combine Monads.
Monad transformer libraries such as mtl allow you to compose different monads
to make a new version.
-}
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

newtype BattlefieldWithLog bf
instance Show Battlefield where
        show bf = "Battlefield (Attackers:\t"
                    ++ show (attackers bf)
                    ++ ", Defenders:\t"
                    ++ show (defenders bf)
                    ++ ")\n"
                    ++ (battleLogs bf)
