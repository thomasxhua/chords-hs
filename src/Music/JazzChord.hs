module Music.JazzChord
    ( JazzChord
    , jazzify
    , chordify
    , prettify
    ) where

import Music.PitchClass
import Music.Chord
import Music.Prettify

type JazzChord = (PitchClass, Chord2, Chord3, Chord4, Chord5, Chord6, Chord7)

jazzify :: Chord -> JazzChord 
jazzify c@(root, _) = (root, chord2 c, chord3 c, chord4 c, chord5 c, chord6 c, chord7 c)

chordify :: JazzChord -> Chord
chordify (root,c2,c3,c4,c5,c6,c7) = (root, filter ((/=) root) [f c2, f c3, f c4, f c5, f c6, f c7])
    where
        f c = root + (toPitchClass $ semitones c)

instance Prettify JazzChord where
    prettify (root, _, C3Minor, _, C5Diminished, _, C7Diminished) = map f $ prettify root
        where
            f r = r ++ show C7Diminished
    prettify (root, c2, c3, c4, c5, c6, c7) = map f $ prettify root
        where
            alts = foldr (\a b -> (if null a then "" else ',' : a) ++ b) "" $ [show c2, show c4, show c6]
            f r
              | c3 `elem` [C3Sus4, C3Sus2, C3None] = r ++ show c7 ++ show c5 ++ show c3 ++ alts
              | otherwise                          = r ++ show c3 ++ show c7 ++ show c5 ++ alts

instance Prettify Chord where
    prettify = prettify . jazzify

