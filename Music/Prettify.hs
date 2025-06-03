module Music.Prettify
    ( Prettify(..)
    ) where

class Prettify a where
    prettify :: a -> [String]

