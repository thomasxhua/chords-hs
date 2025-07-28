module Main where

import Control.Monad (when)

import Midi.Interface

main :: IO ()
main = do
    withMidiInterface $ \midi -> do
        setPortSucess <- setPort midi
        when setPortSucess $ keysWatcher midi

