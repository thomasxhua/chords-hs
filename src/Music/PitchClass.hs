module Music.PitchClass
    ( PitchClass(..)
    , flat
    , sharp
    , toPitchClass
    ) where

import Music.Prettify

data PitchClass = C|Cs|D|Ds|E|F|Fs|G|Gs|A|As|B
    deriving (Eq,Enum,Bounded,Show)

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

