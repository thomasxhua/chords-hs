module Main where

import Music.PitchClass
import Music.Interval
import Music.Chord
import Music.JazzChord

c = [ (flat E, [D, flat B, G, F, A])
    , (flat E, [D, flat B, F, flat A])
    , (flat E, [flat $ flat D, flat $ flat B, flat G])
    , (flat E, [flat D, flat $ flat B, flat G])
    , (G, [F,A,B,E])
    , (G, [F,A,E])
    , (G, [D])
    , (G, [D,F])
    , (G, [B,D])
    , (G, [C,E])
    ]

main :: IO ()
main = do
    putStrLn $ show $ map prettify c

