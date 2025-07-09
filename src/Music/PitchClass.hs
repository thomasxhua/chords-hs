module Music.PitchClass
    ( PitchClass(..)
    , flat
    , sharp
    , toPitchClass
    ) where

import Music.Prettify

data PitchClass = C|Cs|D|Ds|E|F|Fs|G|Gs|A|As|B
    deriving (Eq,Ord,Enum,Bounded,Show)

flat :: PitchClass -> PitchClass
flat = flip (-) 1

sharp :: PitchClass -> PitchClass
sharp = (+) 1

toPitchClass :: Int -> PitchClass
toPitchClass n = toEnum (n `mod` 12)

instance Num PitchClass where
    a + b         = toEnum $ (fromEnum a + fromEnum b) `mod` 12
    a - b         = toEnum $ (fromEnum a - fromEnum b) `mod` 12
    a * b         = toEnum $ (fromEnum a * fromEnum b) `mod` 12
    negate a      = toEnum $ (- fromEnum a) `mod` 12
    abs           = id
    signum        = const 1
    fromInteger a = toEnum (fromInteger a `mod` 12)

instance Prettify PitchClass where
    prettify C  = ["C"]
    prettify Cs = ["Db","C#"]
    prettify D  = ["D"]
    prettify Ds = ["Eb","D#"]
    prettify E  = ["E"]
    prettify F  = ["F"]
    prettify Fs = ["Gb","F#"]
    prettify G  = ["G"]
    prettify Gs = ["Ab","G#"]
    prettify A  = ["A"]
    prettify As = ["Bb","A#"]
    prettify B  = ["B"]

