module AutoNotifier where
import ApplePush
import qualified Data.ByteString as BS
import Control.Concurrent.Chan
import Control.Concurrent
import Text.JSON
import Control.Monad
import Data.List.Split
import qualified Data.ByteString.Char8 as BSC
import Network.Socket

--this should just take a notification and send it

start :: FilePath -> FilePath -> String -> Integer -> NotificationServiceChan -> IO ()
start private_key cert address port input = do
    c <- newChan
    connectToNotificationService private_key cert address port c
    connection_callback input c
	
--the idea is that this thing eventually gives me a channel
--that I need to give to something that creates notifications	
connection_callback :: NotificationServiceChan -> NotificationCallbackChan -> IO ()
connection_callback input c = do
	msg <- readChan c
	case msg of
		(NotificationServerConnected service_channel) -> do {
		    launch_notification_consumer input service_channel;
		}
		(NotificationServerUnableToConnect error_message) -> print error_message
		NotificationServerDisconnected -> print "NotificationServerDisconnected"

send_notification :: NotificationServiceMsg -> NotificationServiceChan -> IO ()
send_notification m c = do
    writeChan c m
    
launch_notification_consumer :: NotificationServiceChan -> NotificationServiceChan -> IO ()
launch_notification_consumer input output = do
    tid <- forkIO (notification_consumer input output)
    return ()

notification_consumer input output = forever $ do
    message <- readChan input
    send_notification message output
     

        
        
    
    
    
    
    
    
    


    