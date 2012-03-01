{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module PushApple.AppContext where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Control.Monad.Trans

newtype AppContext a b c d = AppContext {runApp :: (WriterT c (ReaderT a (StateT b IO)) d)}
    deriving (Monad, MonadIO, MonadReader a,
                    MonadState b, MonadWriter c, Functor)

run_context default_state default_config commands = 
    (runStateT (runReaderT (runWriterT commands) default_config) 
            default_state)
            
data Program a b c = Program
    {
        commands :: [AppContext a b c ()],
        state    :: AppContext a b c ()
    }
    

