module Music.Triad
    ( Triad(..)
    , chordify
    , TriadSpecies(..)
    , triadFromRoot
    , species
    , speciesify
    , isValid
    , parallel
    , relative
    , leitton
    ) where

import Music.PitchClass
import Music.Chord
import Music.Interval

type Triad = (PitchClass, PitchClass, PitchClass)

instance Chordify Triad where
    chordify (a,b,c) = (a,[b,c])

data TriadSpecies = TSMinor|TSMajor|TSNone
    deriving (Eq,Show)

triadFromRoot :: PitchClass -> TriadSpecies -> Maybe Triad
triadFromRoot root tr = case tr of
    TSNone  -> Nothing
    TSMinor -> Just (root, thirdify SMinor, fifth)
    TSMajor -> Just (root, thirdify SMajor, fifth)
    where
        thirdify = (+) root . toPitchClass . semitones . SInterval Third
        fifth = root + (toPitchClass . semitones $ PInterval Fifth PPerfect)

invertOnce :: Triad -> Triad
invertOnce (a,b,c) = (b,c,a)

class Species a where
    species :: a -> TriadSpecies

instance Species Triad where
    species (a,b,c)
      | not $ PInterval Fifth PPerfect `elem` intervalsFromRoot a c = TSNone
      | SInterval Third SMinor `elem` intervalsFromRoot a b         = TSMinor
      | SInterval Third SMajor `elem` intervalsFromRoot a b         = TSMajor
      | otherwise                                                   = TSNone

instance Species Chord where
    species c
      | all (`elem` ints) [SInterval Third SMajor, PInterval Fifth PPerfect] = TSMajor
      | all (`elem` ints) [SInterval Third SMinor, PInterval Fifth PPerfect] = TSMinor
      | otherwise                                                            = TSNone
      where
          ints = intervals c

speciesify :: Chord -> Maybe Triad
speciesify c@(root,_) = triadFromRoot root $ species c

isValid :: Triad -> Bool
isValid = ((/=) TSNone) . species

parallel :: Triad -> Triad
parallel = id

relative :: Triad -> Triad
relative = id

leitton :: Triad -> Triad
leitton = id

