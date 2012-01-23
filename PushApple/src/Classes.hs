{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
module ApplePush2.Classes where
import Text.Parsec
import Control.Monad
import Control.Applicative
import Data.Functor.Identity
    
class Equivalent a where
    (=~=) :: a -> a -> Bool
    default (=~=) :: (Eq a) => a -> a -> Bool
    x =~= y = x == y 
    
class FromCommandLine a where
    fromCommandLine :: ParsecT String u Identity a
    --I would like to make a default implementation

class ToCommandLine a where
    toCommandLine :: a -> String
    default toCommandLine :: Show a => a -> String
    toCommandLine = show 
    
    
