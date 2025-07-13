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

main :: IO ()
main = do
    midi <- ffi_midi_interface_new
    ffi_midi_interface_set_port midi 0
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
    ffi_midi_interface_free midi

