module Music.Triad
    ( Triad(..)
    , TriadSpecies(..)
    , chordify
    , major
    , minor
    , parallel
    , relative
    , leitton
    ) where

import Music.PitchClass
import Music.Chord
import Music.Interval

data TriadSpecies = TSMinor|TSMajor
    deriving (Eq,Show)

type Triad = (PitchClass, TriadSpecies)

instance Chordify Triad where
    chordify (root, tr) = case tr of
        TSMinor -> (root, [thirdify SMinor, fifth])
        TSMajor -> (root, [thirdify SMajor, fifth])
        where
            thirdify = (+) root . toPitchClass . semitones . SInterval Third
            fifth = root + (toPitchClass . semitones $ PInterval Fifth PPerfect)

triadify :: Chord -> Maybe Triad
triadify c@(root,_)
  | all (`elem` ints) [SInterval Third SMajor, PInterval Fifth PPerfect] = Just (root, TSMajor)
  | all (`elem` ints) [SInterval Third SMinor, PInterval Fifth PPerfect] = Just (root, TSMinor)
  | otherwise                                                            = Nothing
  where
      ints = intervals c

major :: Triad -> Triad
major (root,_) = (root,TSMajor)

minor :: Triad -> Triad
minor (root,_) = (root,TSMinor)

parallel :: Triad -> Triad
parallel c@(_,spec) = case spec of
    TSMinor -> major c
    TSMajor -> minor c

relative :: Triad -> Triad
relative (root,spec) = case spec of
    TSMinor -> (transposeUp root $ SInterval Third SMinor, TSMajor)
    TSMajor -> (transposeDown root $ SInterval Third SMinor, TSMinor)

leitton :: Triad -> Triad
leitton (root,spec) = case spec of
    TSMinor -> (transposeDown root $ SInterval Third SMajor, TSMajor)
    TSMajor -> (transposeUp root $ SInterval Third SMajor, TSMinor)

