module Music.Chord
    ( Chord(..)
    , Chord2(..)
    , Chord3(..)
    , Chord4(..)
    , Chord5(..)
    , Chord6(..)
    , Chord7(..)
    , chord2
    , chord3
    , chord4
    , chord5
    , chord6
    , chord7
    , intervals
    ) where

import Music.PitchClass
import Music.Interval
import Music.Prettify

type Chord = (PitchClass, [PitchClass])

intervals :: Chord -> [Interval]
intervals (root, set) = foldr (++) [] $ map (fromSemitones . fromEnum . flip (-) root) set

data Chord2 = C2Major|C2Minor|C2Augmented|C2Major7|C2Minor7|C2Augmented7|C2None
    deriving (Eq)

instance Show Chord2 where
    show C2Minor      = "b2"
    show C2Major      = "2"
    show C2Augmented  = "#2"
    show C2Minor7     = "b9"
    show C2Major7     = "9"
    show C2Augmented7 = "#9"
    show C2None       = ""

chord2 :: Chord -> Chord2
chord2 chord
    | chord3 chord /= C3Sus2
      && SInterval Second SMajor `elem` ints     = if hasSeventh ints
                                                      then C2Major7
                                                      else C2Major
    | chord3 chord /= C3Sus2
      && SInterval Second SMinor `elem` ints     = if hasSeventh ints
                                                      then C2Minor7
                                                      else C2Minor
    | chord3 chord /= C3Sus2
      && SInterval Second SAugmented `elem` ints = if hasSeventh ints
                                                      then C2Augmented7
                                                      else C2Augmented
    | otherwise                                  = C2None
    where
        ints = intervals $ chord

data Chord3 = C3Sus2|C3Minor|C3Major|C3Sus4|C3No3|C3None
    deriving (Eq)

instance Show Chord3 where
    show C3Minor = "-"
    show C3Major = ""
    show C3Sus4  = "sus4"
    show C3Sus2  = "sus2"
    show C3None  = "no3"

chord3 :: Chord -> Chord3
chord3 = f . intervals
    where
        f ints
          | SInterval Third SMajor `elem` ints    = C3Major
          | SInterval Third SMinor `elem` ints    = C3Minor
          | PInterval Fourth PPerfect `elem` ints = C3Sus4
          | SInterval Second SMajor `elem` ints   = C3Sus2
          | otherwise                             = C3None

data Chord4 = C4Perfect|C4Augmented|C4Perfect7|C4Augmented7|C4None
    deriving (Eq)

instance Show Chord4 where
    show C4Perfect    = "4"
    show C4Augmented  = "#4"
    show C4Perfect7   = "11"
    show C4Augmented7 = "#11"
    show C4None       = ""

chord4 :: Chord -> Chord4
chord4 chord
    | hasThird ints
      && PInterval Fourth PPerfect `elem` ints   = if hasSeventh ints
                                                      then C4Perfect7
                                                      else C4Perfect
    | hasThird ints
      && PInterval Fourth PAugmented `elem` ints = if hasSeventh ints
                                                      then C4Augmented7
                                                      else C4Augmented
    | otherwise                                  = C4None
    where
        ints = intervals $ chord

data Chord5 = C5Diminished|C5Perfect|C5Augmented|C5None
    deriving (Eq)

instance Show Chord5 where
    show C5Diminished = "b5"
    show C5Perfect    = ""
    show C5Augmented  = "#5"
    show C5None       = ""

chord5 :: Chord -> Chord5
chord5 = f . intervals
    where
        f ints
          | PInterval Fifth PPerfect `elem` ints    = C5Perfect
          | PInterval Fifth PDiminished `elem` ints = C5Diminished
          | PInterval Fifth PAugmented `elem` ints  = C5Augmented
          | otherwise                               = C5None

data Chord6 = C6Minor|C6Major|C6Minor7|C6Major7|C6None
    deriving (Eq)

instance Show Chord6 where
    show C6Minor      = "b6"
    show C6Major      = "#6"
    show C6Minor7     = "b13"
    show C6Major7     = "13"
    show C6None       = ""

chord6 :: Chord -> Chord6
chord6 chord
    | SInterval Sixth SMinor `elem` ints = if hasSeventh ints
                                              then C6Minor7
                                              else C6Minor
    | SInterval Sixth SMajor `elem` ints = if hasSeventh ints
                                              then C6Major7
                                              else C6Major
    | otherwise                          = C6None
    where
        ints = intervals $ chord

data Chord7 = C7Diminished|C7Dominant|C7Major|C7None
    deriving (Eq)

instance Show Chord7 where
    show C7Dominant   = "7"
    show C7Major      = "maj7"
    show C7Diminished = "dim7"
    show C7None       = ""

chord7 :: Chord -> Chord7
chord7 = f . intervals
    where
        f ints
          | SInterval Seventh SMinor `elem` ints      = C7Dominant
          | SInterval Seventh SMajor `elem` ints      = C7Major
          | SInterval Seventh SDiminished `elem` ints = C7Diminished
          | otherwise                                 = C7None

hasThird :: [Interval] -> Bool
hasThird ints = any (`elem` ints) [SInterval Third SMajor, SInterval Third SMinor]

hasSeventh :: [Interval] -> Bool
hasSeventh ints = any (`elem` ints) [SInterval Seventh SMajor, SInterval Seventh SMinor]

