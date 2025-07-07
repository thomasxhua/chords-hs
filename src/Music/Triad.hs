module Music.Triad
    ( Triad(..)
    , TriadSpecies(..)
    , chordify
    , species
    , major
    , minor
    , parallel
    , relative
    , leittonw
    , NROperation(..)
    ) where

import Music.PitchClass
import Music.Chord
import Music.Interval

data TriadSpecies = TSMinor|TSMajor
    deriving (Eq,Show)

type Triad = (PitchClass, TriadSpecies)

instance Chordify Triad where
    chordify (root,spec) = (root, set)
        where
            thirdify = (+) root . toPitchClass . semitones . SInterval Third
            fifth    = (+) root . toPitchClass . semitones . PInterval Fifth $ PPerfect
            set      = case spec of
                TSMinor -> [thirdify SMinor, fifth]
                TSMajor -> [thirdify SMajor, fifth]

species :: Chord -> Maybe TriadSpecies
species c@(root,_)
  | all (`elem` ints) [SInterval Third SMajor, PInterval Fifth PPerfect] = Just TSMajor
  | all (`elem` ints) [SInterval Third SMinor, PInterval Fifth PPerfect] = Just TSMinor
  | otherwise                                                            = Nothing
  where
      ints = intervals c

major :: Triad -> Triad
major (root,_) = (root,TSMajor)

minor :: Triad -> Triad
minor (root,_) = (root,TSMinor)

parallel :: Triad -> Triad
parallel c@(root,spec) = case spec of
    TSMinor -> major c
    TSMajor -> minor c

relative :: Triad -> Triad
relative (root,spec) = case spec of
    TSMinor -> (transposeUp   root $ SInterval Third SMinor, TSMajor)
    TSMajor -> (transposeDown root $ SInterval Third SMinor, TSMinor)

leittonw :: Triad -> Triad
leittonw (root,spec) = case spec of
    TSMinor -> (transposeDown root $ SInterval Third SMajor, TSMajor)
    TSMajor -> (transposeUp   root $ SInterval Third SMajor, TSMinor)

data NROperation = NRParallel|NRRelative|NRLeittonw
    deriving (Eq)

instance Show NROperation where
    show NRParallel = "P"
    show NRRelative = "R"
    show NRLeittonw = "L"

calculatePath :: Triad -> Triad -> [NROperation]
calculatePath a@(_,aSpec) b@(_,bSpec)
  | a == b = []

