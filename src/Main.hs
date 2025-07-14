module Main where

import Data.List (intersect)

import Music.PitchClass
import Music.Interval
import Music.Chord
import Music.JazzChord
import Music.Triad
import Music.Terz

import Midi.FFI
import Midi.Interface

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
     , (C, [E,A])
     , (C, [E,G,A])
     , (C, [E,G,flat B])
     , (C, [Cs,D,Ds,E,F,Fs,G,Gs,A,As,B])
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
    print $ map prettify cs
    print ""
    print $ intersect ton sub
    print $ intersect ton dom
    print $ intersect sub dom
    print $ transformNRTree (head . prettify . chordify) $ applyNRTree (C,TSMajor) (generateNRTree 3)
    midiInterface $ \midi -> do
        setPort midi
        keysWatcher midi
