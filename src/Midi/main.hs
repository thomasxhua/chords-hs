{-# LANGUAGE ForeignFunctionInterface #-}

module Midi.FFI
    ( MidiInterface(..)
    , MidiDevice(..)

import Foreign
import Foreign.Ptr (nullPtr)
import Foreign.C.String (CString, peekCString)
import Data.Word
import Data.Bits (testBit)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, forM_)
import Control.Exception (bracket)
import System.IO (hReady, hFlush, stdin, stdout)

data MidiInterface
data MidiDevice

foreign import ccall "ffi_midi_interface_new"
    ffi_midi_interface_new :: IO (Ptr MidiInterface)

foreign import ccall "ffi_midi_interface_free"
    ffi_midi_interface_free :: Ptr MidiInterface -> IO ()

foreign import ccall "ffi_midi_interface_set_port"
    ffi_midi_interface_set_port :: Ptr MidiInterface -> Word64 -> IO ()

foreign import ccall "ffi_midi_interface_get_port_count"
    ffi_midi_interface_get_port_count :: Ptr MidiInterface -> IO Word64

foreign import ccall "ffi_midi_interface_get_port_name"
    ffi_midi_interface_get_port_name :: Ptr MidiInterface -> Word64 -> IO CString

foreign import ccall "ffi_midi_interface_get_device"
    ffi_midi_interface_get_device :: Ptr MidiInterface -> IO (Ptr MidiDevice)

data MidiDeviceKeys = MidiDeviceKeys Word64 Word64

instance Storable MidiDeviceKeys where
    sizeOf _    = sizeOf (undefined :: Word64) * 2
    alignment _ = alignment (undefined :: Word64)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word64
        low  <- peekElemOff ptr' 0
        high <- peekElemOff ptr' 1
        return $ MidiDeviceKeys low high
    poke ptr (MidiDeviceKeys low high) = do
        let ptr' = castPtr ptr :: Ptr Word64
        pokeElemOff ptr' 0 low
        pokeElemOff ptr' 1 high

instance Show MidiDeviceKeys where
    show (MidiDeviceKeys low high) = showKeysHalf low ++ showKeysHalf high
        where
            showKeysHalf w = [ if testBit w i then 'x' else ' ' | i <- [0..63] ]

foreign import ccall "ffi_midi_device_get_keys"
    ffi_midi_device_get_keys :: Ptr MidiDevice -> IO (Ptr MidiDeviceKeys)

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

keysWatcher :: Ptr MidiInterface -> IO ()
keysWatcher midi = do
    device <- ffi_midi_interface_get_device midi
    let loop = do
            inputting <- hReady stdin
            unless inputting $ do
                keys <- ffi_midi_device_get_keys device
                keysStr <- peek keys
                print keysStr
                threadDelay 20000
                loop
    loop

midiInterface :: (Ptr MidiInterface -> IO a) -> IO a
midiInterface = bracket ffi_midi_interface_new ffi_midi_interface_free

