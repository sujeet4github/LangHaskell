module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / planetsOrbitalPeriodInSeconds planet

planetsOrbitalPeriodInSeconds :: Planet -> Float
planetsOrbitalPeriodInSeconds planet = planetsOrbitalPeriodDividedByEarths planet * earthOrbitalPeriodInSeconds

planetsOrbitalPeriodDividedByEarths :: Planet -> Float
planetsOrbitalPeriodDividedByEarths Mercury  = 0.2408467
planetsOrbitalPeriodDividedByEarths Venus    = 0.61519726
planetsOrbitalPeriodDividedByEarths Mars     = 1.8808158
planetsOrbitalPeriodDividedByEarths Jupiter  = 11.862615
planetsOrbitalPeriodDividedByEarths Saturn   = 29.447498
planetsOrbitalPeriodDividedByEarths Uranus   = 84.016846
planetsOrbitalPeriodDividedByEarths Neptune  = 164.79132
planetsOrbitalPeriodDividedByEarths _        = 1

earthOrbitalPeriodInSeconds :: Float
earthOrbitalPeriodInSeconds = 31557600
