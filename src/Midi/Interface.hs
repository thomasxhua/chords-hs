module Midi.Interface
    ( printPort
    , setPort
    , midiInterface
    , keysWatcher
    ) where

import Foreign.Storable (peek)
import Foreign.Ptr (nullPtr)
import Foreign.C.String (peekCString)
import Control.Monad (unless, forM_)
import Control.Exception (bracket)
import System.IO (hReady, hFlush, stdin, stdout)
import Data.Bits (testBit)
import Control.Concurrent (threadDelay)

import Midi.FFI
import Music.PitchClass
import Music.Chord
import Music.JazzChord

printPort :: Ptr MidiInterface -> Word64 -> IO String
printPort midi i = do
    cStr <- ffi_midi_interface_get_port_name midi i
    name <- peekCString cStr
    return $ "(" ++ show i ++ ") " ++ name

setPort :: Ptr MidiInterface -> IO ()
setPort midi
    | midi == nullPtr = return ()
    | otherwise       = do
        portCount <- ffi_midi_interface_get_port_count midi
        if portCount == 0
           then do
                putStrLn "No MIDI ports found."
            else do
                forM_ [0 .. portCount-1] $ \i -> do
                    line <- printPort midi i
                    putStrLn line
                putStr $ "Select from " ++ show portCount ++ " MIDI ports: "
                hFlush stdout
                input <- getLine
                ffi_midi_interface_set_port midi (read input :: Word64)

midiInterface :: (Ptr MidiInterface -> IO a) -> IO a
midiInterface = bracket ffi_midi_interface_new ffi_midi_interface_free

midiDeviceKeysToPitchClasses :: MidiDeviceKeys -> [PitchClass]
midiDeviceKeysToPitchClasses (MidiDeviceKeys low high) =
    [toPitchClass i | i <- [0..63], testBit low i] ++
    [toPitchClass i + 64 | i <- [0..53], testBit high i]

midiDeviceKeysToChord :: MidiDeviceKeys -> Maybe Chord
midiDeviceKeysToChord = pitchClassesToChord . midiDeviceKeysToPitchClasses

keysWatcher :: Ptr MidiInterface -> IO ()
keysWatcher midi = do
    device <- ffi_midi_interface_get_device midi
    let loop = do
            inputting <- hReady stdin
            unless inputting $ do
                keysPtr <- ffi_midi_device_get_keys device
                keys    <- peek keysPtr
                putStr . show $ keys
                putStrLn (maybe "/" (head . prettify) $ midiDeviceKeysToChord keys)
                threadDelay 20000
                loop
    loop

