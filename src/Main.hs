module Main where

import Data.List (intersect)

import Music.PitchClass
import Music.Interval
import Music.Chord
import Music.JazzChord
import Music.Triad
import Music.Terz

cs = [ (flat E, [D, flat B, G, F, A])
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

ts = [ (C, TSMajor)
     , (F, TSMajor)
     , (G, TSMajor)
     ]

tvs = [ _Tp, _Tg, _TP, _TG, _tG, _tP, _tg, _tp]

ton = map ($ (C,TSMajor)) tvs
sub = map ($ (F,TSMajor)) tvs
dom = map ($ (G,TSMajor)) tvs

main :: IO ()
main = do
    putStrLn $ show $ map prettify cs

