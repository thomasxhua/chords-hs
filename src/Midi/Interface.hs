module Midi.Interface
    ( printPort
    , setPort
    , withMidiInterface
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
import Text.Read (readMaybe)

import Midi.FFI
import Midi.Constants
import Music.PitchClass
import Music.Chord
import Music.JazzChord

printPort :: Ptr MidiInterface -> Word64 -> IO String
printPort midi i = do
    cStr <- ffi_midi_interface_get_port_name midi i
    name <- peekCString cStr
    return $ "(" ++ show i ++ ") " ++ name

setPort :: Ptr MidiInterface -> IO Bool
setPort midi
    | midi == nullPtr = return False
    | otherwise       = do
        portCount <- ffi_midi_interface_get_port_count midi
        if portCount == 0
           then do
                putStrLn "No MIDI ports found."
                return False
            else do
                forM_ [0 .. portCount-1] $ \i -> do
                    line <- printPort midi i
                    putStrLn line
                putStr $ "Select from " ++ show portCount ++ " MIDI ports: "
                hFlush stdout
                input <- getLine
                case readMaybe input :: Maybe Word64 of
                    Just portNumber | portNumber < portCount -> do
                        ffi_midi_interface_set_port midi portNumber
                        return True
                    _ -> do
                        putStrLn "Invalid port selection."
                        return False

withMidiInterface :: (Ptr MidiInterface -> IO a) -> IO a
withMidiInterface = bracket ffi_midi_interface_new ffi_midi_interface_free

midiDeviceKeysToPitchClasses :: MidiDeviceKeys -> [PitchClass]
midiDeviceKeysToPitchClasses (MidiDeviceKeys low high)
    =  [toPitchClass i                      | i <- [0..(messageValLowMax-1)], testBit low i]
    ++ [toPitchClass $ i + messageValLowMax | i <- [0..(messageValLowMax-1)], testBit high i]

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

