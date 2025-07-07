module Music.Triad
    ( Triad(..)
    , TriadSpecies(..)
    , chordify
    , species
    , major
    , minor
    , parallel
    , relative
    , leitton
    ) where

import Music.PitchClass
import Music.Chord
import Music.Interval

data TriadSpecies = TSMinor|TSMajor|TSNone
    deriving (Eq,Show)

type Triad = (PitchClass, TriadSpecies)

instance Chordify Triad where
    chordify (root,spec) = (root, set)
        where
            thirdify = (+) root . toPitchClass . semitones . SInterval Third
            fifth    = (+) root . toPitchClass . semitones . PInterval Fifth $ PPerfect
            set      = case spec of
                TSNone  -> []
                TSMinor -> [thirdify SMinor, fifth]
                TSMajor -> [thirdify SMajor, fifth]

species :: Chord -> TriadSpecies
species c@(root,_)
  | all (`elem` ints) [SInterval Third SMajor, PInterval Fifth PPerfect] = TSMajor
  | all (`elem` ints) [SInterval Third SMinor, PInterval Fifth PPerfect] = TSMinor
  | otherwise                                                            = TSNone
  where
      ints = intervals c

major :: Triad -> Triad
major (root,_) = (root,TSMajor)

minor :: Triad -> Triad
minor (root,_) = (root,TSMinor)

parallel :: Triad -> Triad
parallel c@(root,spec) = case spec of
    TSNone  -> (root,TSNone)
    TSMinor -> major c
    TSMajor -> minor c

relative :: Triad -> Triad
relative (root,spec) = case spec of
    TSNone  -> (root,TSNone)
    TSMinor -> (transposeUp   root $ SInterval Third SMinor, TSMajor)
    TSMajor -> (transposeDown root $ SInterval Third SMinor, TSMinor)

leitton :: Triad -> Triad
leitton (root,spec) = case spec of
    TSNone  -> (root,TSNone)
    TSMinor -> (transposeDown root $ SInterval Third SMajor, TSMajor)
    TSMajor -> (transposeUp   root $ SInterval Third SMajor, TSMinor)

