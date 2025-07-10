{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.Ptr
import Data.Word
import Data.Bits (testBit)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import System.IO (hReady, stdin)

foreign import ccall "ffi_fibonacci"
    ffi_fibonacci :: Word64 -> IO Word64

data MidiInterface
data MidiDevice

foreign import ccall "ffi_midi_interface_new"
    ffi_midi_interface_new :: IO (Ptr MidiInterface)

foreign import ccall "ffi_midi_interface_free"
    ffi_midi_interface_free :: Ptr MidiInterface -> IO ()

foreign import ccall "ffi_midi_interface_set_port"
    ffi_midi_interface_set_port :: Ptr MidiInterface -> Word64 -> IO ()

foreign import ccall "ffi_midi_interface_get_device"
    ffi_midi_interface_get_device :: Ptr MidiInterface -> IO (Ptr MidiDevice)

foreign import ccall "ffi_midi_device_get_keys_low"
    ffi_midi_device_get_keys_low :: Ptr MidiDevice -> IO Word64

foreign import ccall "ffi_midi_device_get_keys_high"
    ffi_midi_device_get_keys_high :: Ptr MidiDevice -> IO Word64

showKeysHalf :: Word64 -> String
showKeysHalf w = [ if testBit w i then 'x' else ' ' | i <- [0..63] ]

main :: IO ()
main = do
    midi   <- ffi_midi_interface_new
    ffi_midi_interface_set_port midi 0
    device <- ffi_midi_interface_get_device midi
    let loop = do
            inputting <- hReady stdin
            unless inputting $ do
                low    <- ffi_midi_device_get_keys_low device
                high   <- ffi_midi_device_get_keys_high device
                print $ showKeysHalf low ++ showKeysHalf high
                threadDelay 20000
                loop
    loop
    ffi_midi_interface_free midi

