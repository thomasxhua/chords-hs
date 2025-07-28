module Music.Triad
    ( Triad(..)
    , TriadSpecies(..)
    , chordify
    , species
    , major
    , minor
    , parallel
    , leittonw
    , relative
    , NROperation(..)
    , generateNRTree
    , transformNRTree
    , traverseDFSNRTree
    , applyNROperation
    , applyNRTree
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
parallel (root,spec) = case spec of
    TSMinor -> (root, TSMajor)
    TSMajor -> (root, TSMinor)

leittonw :: Triad -> Triad
leittonw (root,spec) = case spec of
    TSMinor -> (transposeDown root $ SInterval Third SMajor, TSMajor)
    TSMajor -> (transposeUp   root $ SInterval Third SMajor, TSMinor)

relative :: Triad -> Triad
relative (root,spec) = case spec of
    TSMinor -> (transposeUp   root $ SInterval Third SMinor, TSMajor)
    TSMajor -> (transposeDown root $ SInterval Third SMinor, TSMinor)

data NROperation = NRParallel|NRRelative|NRLeittonw|NRNone
    deriving (Eq)

instance Show NROperation where
    show NRParallel = "P"
    show NRRelative = "R"
    show NRLeittonw = "L"
    show NRNone     = ""

    {-
calculatePath :: Triad -> Triad -> [NROperation]
calculatePath a b
  | a == b    = []
  | aP == b   = [NRParallel]
  | aR == b   = [NRRelative]
  | aL == b   = [NRLeittonw]
  | otherwise = 
      -}

data NRTree a
    = NREmpty
    | NRNode a [NRTree a]
    deriving (Eq)

instance Show a => Show (NRTree a) where
  show NREmpty       = ""
  show (NRNode x xs) = show x ++ f xs
    where
      f [] = ""
      f ts = "(" ++ unwords (map show ts) ++ ")"

generateNRTree :: Int -> NRTree NROperation
generateNRTree h = case h of
    0 -> NREmpty
    _ -> NRNode NRNone [ generateNRNode NRParallel (h-1)
                       , generateNRNode NRLeittonw (h-1)
                       , generateNRNode NRRelative (h-1)
                       ]
    where
        generateNRNode op n
          | n == 0           = NRNode op []
          | op == NRParallel = NRNode op [ generateNRNode NRLeittonw (n-1)
                                         , generateNRNode NRRelative (n-1)
                                         ]
          | op == NRLeittonw = NRNode op [ generateNRNode NRParallel (n-1)
                                         , generateNRNode NRRelative (n-1)
                                         ]
          | op == NRRelative = NRNode op [ generateNRNode NRParallel (n-1)
                                         , generateNRNode NRLeittonw (n-1)
                                         ]

transformNRTree :: (a -> b) -> NRTree a -> NRTree b
transformNRTree _ NREmpty       = NREmpty
transformNRTree f (NRNode x xs) = NRNode (f x) (map transformNRTree f xs)

traverseDFSNRTree :: (a -> b -> b) -> b -> NRTree a -> NRTree b
traverseDFSNRTree _ _ NREmpty       = NREmpty
traverseDFSNRTree f y (NRNode x xs) = NRNode y' (map traverseDFSNRTree f y' xs)
    where
        y' = f x y

applyNROperation :: NROperation -> Triad -> Triad
applyNROperation op tr = case op of
    NRNone     -> tr
    NRParallel -> parallel tr
    NRLeittonw -> leittonw tr
    NRRelative -> relative tr

applyNRTree :: Triad -> NRTree NROperation -> NRTree Triad
applyNRTree = traverseDFSNRTree applyNROperation

