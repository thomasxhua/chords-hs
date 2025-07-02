module Music.Triad
    ( Triad(..)
--    , species
--    , isValid
    , parallel
    , relative
    , leitton
    ) where

import Music.PitchClass

type Triad = (PitchClass, PitchClass, PitchClass)

data TriadSpecies = TSMinor|TSMajor|TSNone
    deriving (Eq)

invertOnce :: Triad -> Triad
invertOnce (a,b,c) = (b,c,a)

--species :: Triad -> TriadSpecies
--species (a,b,c)
--  | intervalFromRoot a c == PInterval Fifth PPerfect =
--      if (

--species tr0 = 
--    where
--        tr1 = invertOnce tr0
--        tr2 = invertOnce . invertOnce tr0

--isValid :: Triad -> Bool
--isValid = ((/=) TSNone) . triadSpecies

parallel :: Triad -> Triad
parallel = id

relative :: Triad -> Triad
relative = id

leitton :: Triad -> Triad
leitton = id

