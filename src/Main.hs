module Main where

import Midi.Interface

main :: IO ()
main = do
    withMidiInterface $ \midi -> do
        setPortSucess <- setPort midi
        if setPortSucess
            then keysWatcher midi
            else return ()

