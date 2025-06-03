data PitchClass = C|Cs|D|Ds|E|F|Fs|G|Gs|A|As|B
    deriving (Eq,Enum,Bounded,Show)

fl :: PitchClass -> PitchClass
fl = flip (-) (toEnum 1)

sh :: PitchClass -> PitchClass
sh = (+) (toEnum 1)

instance Num PitchClass where
    a + b         = toEnum $ (fromEnum a + fromEnum b) `mod` 12
    a - b         = toEnum $ (fromEnum a - fromEnum b) `mod` 12
    a * b         = toEnum $ (fromEnum a * fromEnum b) `mod` 12
    negate a      = toEnum $ (- fromEnum a) `mod` 12
    abs           = id
    signum        = const 1
    fromInteger a = toEnum (fromInteger a `mod` 12)

data IntervalPerfect        = Unison|Fourth|Fifth
    deriving (Eq,Show)
data IntervalStepped        = Second|Third|Sixth|Seventh
    deriving (Eq,Show)
data IntervalPerfectQuality = PDiminished|PPerfect|PAugmented
    deriving (Eq,Show)
data IntervalSteppedQuality = SDiminished|SMinor|SMajor|SAugmented
    deriving (Eq,Show)

data Interval
    = PInterval IntervalPerfect IntervalPerfectQuality
    | SInterval IntervalStepped IntervalSteppedQuality
    deriving (Eq,Show)

class Semitones a where
    semitones :: a -> Int

instance Semitones IntervalPerfect where
    semitones Unison = 0
    semitones Fourth = 5
    semitones Fifth  = 7

instance Semitones IntervalStepped where
    semitones Second  = 2
    semitones Third   = 4
    semitones Sixth   = 9
    semitones Seventh = 11

instance Semitones IntervalPerfectQuality where
    semitones PDiminished  = -1
    semitones PPerfect     = 0
    semitones PAugmented   = 1

instance Semitones IntervalSteppedQuality where
    semitones SDiminished  = -2
    semitones SMinor       = -1
    semitones SMajor       = 0
    semitones SAugmented   = 1

instance Semitones Interval where
    semitones (PInterval interval quality) = (semitones interval + semitones quality) `mod` 12
    semitones (SInterval interval quality) = (semitones interval + semitones quality) `mod` 12

fromSemitones :: Int -> [Interval]
fromSemitones n = f n'
    where
        n' = n `mod` 12
        f x
          | x == 0  = [ PInterval Unison  PPerfect
                      , SInterval Second  SDiminished
                      , SInterval Seventh SAugmented ]
          | x == 1  = [ PInterval Unison  PAugmented
                      , SInterval Second  SMinor ]
          | x == 2  = [ SInterval Second  SMajor
                      , SInterval Third   SDiminished ]
          | x == 3  = [ SInterval Second  SAugmented
                      , SInterval Third   SMinor ]
          | x == 4  = [ SInterval Third   SMajor
                      , PInterval Fourth  PDiminished ]
          | x == 5  = [ SInterval Third   SAugmented
                      , PInterval Fourth  PPerfect ]
          | x == 6  = [ PInterval Fourth  PAugmented
                      , PInterval Fifth   PDiminished ]
          | x == 7  = [ PInterval Fifth   PPerfect
                      , SInterval Sixth   SDiminished ]
          | x == 8  = [ PInterval Fifth   PAugmented
                      , SInterval Sixth   SMinor ]
          | x == 9  = [ SInterval Sixth   SMajor
                      , SInterval Seventh SDiminished ]
          | x == 10 = [ SInterval Sixth   SAugmented
                      , SInterval Seventh SMinor ]
          | x == 11 = [ SInterval Seventh SMajor
                      , PInterval Unison  PDiminished ]

type Chord = (PitchClass, [PitchClass])

intervals :: Chord -> [[Interval]]
intervals (root, set) = map (fromSemitones . fromEnum . flip (-) root) set

flatten :: [[a]] -> [a]
flatten = foldr (++) []

data Chord3 = C3Sus2|C3Minor|C3Major|C3Sus4|C3No3|C3None
    deriving (Eq)

instance Show Chord3 where
    show C3Minor = "-"
    show C3Major = ""
    show C3Sus4  = "sus4"
    show C3Sus2  = "sus2"
    show C3None  = "no3"

chord3 :: Chord -> Chord3
chord3 = f . flatten . intervals
    where
        f ints
          | SInterval Third SMajor `elem` ints    = C3Major
          | SInterval Third SMinor `elem` ints    = C3Minor
          | PInterval Fourth PPerfect `elem` ints = C3Sus4
          | SInterval Second SMajor `elem` ints   = C3Sus2
          | otherwise                             = C3None

data Chord5 = C5Diminished|C5Perfect|C5Augmented|C5None
    deriving (Eq)

instance Show Chord5 where
    show C5Diminished = "b5"
    show C5Perfect    = ""
    show C5Augmented  = "#5"
    show C5None       = ""

chord5 :: Chord -> Chord5
chord5 = f . flatten . intervals
    where
        f ints
          | PInterval Fifth PPerfect `elem` ints    = C5Perfect
          | PInterval Fifth PDiminished `elem` ints = C5Diminished
          | PInterval Fifth PAugmented `elem` ints  = C5Augmented
          | otherwise                               = C5None

data Chord7 = C7Diminished|C7Dominant|C7Major|C7None
    deriving (Eq)

instance Show Chord7 where
    show C7Dominant   = "7"
    show C7Major      = "maj7"
    show C7Diminished = "dim7"
    show C7None       = ""

chord7 :: Chord -> Chord7
chord7 = f . flatten . intervals
    where
        f ints
          | SInterval Seventh SMinor `elem` ints      = C7Dominant
          | SInterval Seventh SMajor `elem` ints      = C7Major
          | SInterval Seventh SDiminished `elem` ints = C7Diminished
          | otherwise                                 = C7None

class Prettify a where
    prettify :: a -> [String]

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

hasThird :: [Interval] -> Bool
hasThird ints = any (`elem` ints) [SInterval Third SMajor, SInterval Third SMinor]

hasSeventh :: [Interval] -> Bool
hasSeventh ints = any (`elem` ints) [SInterval Seventh SMajor, SInterval Seventh SMinor]

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
        ints   = flatten . intervals $ chord

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
        ints = flatten . intervals $ chord

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
        ints = flatten . intervals $ chord

instance Prettify PitchClass where
    prettify C  = ["C"]
    prettify Cs = ["C#","Db"]
    prettify D  = ["D"]
    prettify Ds = ["D#","Eb"]
    prettify E  = ["E"]
    prettify F  = ["F"]
    prettify Fs = ["F#","Gb"]
    prettify G  = ["G"]
    prettify Gs = ["G#","Ab"]
    prettify A  = ["A"]
    prettify As = ["A#","Bb"]
    prettify B  = ["B"]

type JazzChord = (PitchClass, Chord2, Chord3, Chord4, Chord5, Chord6, Chord7)

jazzify :: Chord -> JazzChord 
jazzify c@(root, _) = (root, chord2 c, chord3 c, chord4 c, chord5 c, chord6 c, chord7 c)

instance Prettify JazzChord where
    prettify (root, _, C3Minor, _, C5Diminished, _, C7Diminished) = map f $ prettify root
        where
            f r = r ++ show C7Diminished
    prettify (root, c2, c3, c4, c5, c6, c7) = map f $ prettify root
        where
            alts = foldr (\a b -> if null a then "" else ',' : a ++ b) "" $ [show c2, show c4, show c6]
            f r
              | c3 `elem` [C3Sus4, C3Sus2, C3None] = r ++ show c7 ++ show c5 ++ show c3 ++ alts
              | otherwise                          = r ++ show c3 ++ show c7 ++ show c5 ++ alts

instance Prettify Chord where
    prettify = prettify . jazzify

c = [ (fl E, [D, fl B, G, F, A])
    , (fl E, [D, fl B, F, fl A])
    , (fl E, [fl $ fl D, fl $ fl B, fl G])
    , (fl E, [fl D, fl $ fl B, fl G])
    , (G, [F,A,B,E])
    , (G, [F,A,E])
    , (G, [D])
    , (G, [D,F])
    ]

