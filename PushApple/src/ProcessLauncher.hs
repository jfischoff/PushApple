{-# LANGUAGE RankNTypes #-}
module ApplePush2.ProcessLauncher where
    
import Control.Arrow 
import Control.Monad
import Data.Thrist
import Control.Applicative
import qualified Data.ByteString as BS
import Control.Functor.Contra
import Data.Monoid
import Control.Concurrent
import Data.Maybe

--Success or some error
--Such bad odds    
type Process a b =  (a -> b)

type IOOutput b = IO (Maybe b)

type IOProcess a b = (a -> IOOutput b)

-- so I can convert between different settings
type WrappedProcess a b = Thrist (->) a b

type IOWrappedProcess a b = WrappedProcess a (IOOutput b)

--convert a function to a thrist and append
wrap_process :: (a -> b) -> Process b c -> WrappedProcess a c
wrap_process x y = appendThrist (arr x) (arr y)

--
type ProcessBundle a b c d = ([WrappedProcess a b], c -> d)

type IOProcessBundle a b = ProcessBundle a (IOOutput b) b  (IO ())
--
apply_thrist thristOfFuncs = foldlThrist (flip(.)) id thristOfFuncs

--
launch_processes :: a -> IOProcessBundle a b -> IO ()
launch_processes settings (processes, error_handler) = 
    mapM_ (\p -> start_process settings p error_handler) processes

--I need to rewrite this

--
start_process :: a -> IOWrappedProcess a b -> (b -> IO ()) -> IO ThreadId
start_process = undefined
{-start_process settings process error_handler = do
    forkIO $ process_loop where    
            result <- (apply_thrist process) settings
            if isJust result
                then error_handler $ fromJust result
                else process_loop
        
    
-}



