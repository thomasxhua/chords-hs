module Main where

import Midi.Interface

main :: IO ()
main = do
    withMidiInterface $ \midi -> do
        setPort midi
        keysWatcher midi

