module ProducerConsumer where
    
import Data.Char
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State

to_producer f c = forever $ do 
    e <- f 
    writeChan c e 

to_consumer f cs output = mapM_ (f output) cs

main_loop' p c = main_loop (to_producer p) (to_consumer c)

main_loop producer consumer input output = do
    cs <- liftIO $ getChanContents input     
    liftIO $ forkIO (producer input)         
    consumer cs output