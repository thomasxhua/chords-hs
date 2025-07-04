module Music.Interval
    ( Interval(..)
    , IntervalPerfect(..)
    , IntervalStepped(..)
    , IntervalPerfectQuality(..)
    , IntervalSteppedQuality(..)
    , Semitones(..)
    , fromSemitones
    , intervalsFromRoot
    , transposeDown
    , transposeUp
    ) where

import Music.PitchClass

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

intervalsFromRoot :: PitchClass -> PitchClass -> [Interval]
intervalsFromRoot root pc = fromSemitones $ (fromEnum pc + (if (root > pc) then 12 else 0)) - fromEnum root

transposeDown :: PitchClass -> Interval -> PitchClass
transposeDown pc = ((-) pc) . toPitchClass . semitones

transposeUp :: PitchClass -> Interval -> PitchClass
transposeUp pc = ((+) pc) . toPitchClass . semitones

