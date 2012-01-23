module PushApple.PushApple (
    PushFunctions,
    connectToService,
    module ApplePush2.Types,
    module ApplePush2.Service,
    send_notification,
    connect_push_sandbox,
    connect_push_production,
    get_sandbox_feedback,
    get_production_feedback
) where
import ApplePush2.Service   
import ApplePush2.Types     
import ApplePush2.Instances 
import qualified Data.Binary as B          
import qualified OpenSSL.Session as SSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL
import OpenSSL.EVP.PKey
import Debug.Trace.Helpers
import Debug.Trace
import qualified Data.Aeson as A
import Control.Applicative
   
send_notification_internal :: SSL.SSL -> Payload -> IO ()
send_notification_internal ssl (Payload device_token notification) = do
    SSL.write ssl $ BS.pack $ BSL.unpack $ B.encode $ Payload device_token notification
    
data PushFunctions = PushFunctions 
    {
        send_notification :: (Payload -> IO ())
    }
    
connectToService :: SSLServerConfig -> IO (Either PushFunctions SSLConfigError)
connectToService config = do
    ssl_result <- start_write_only config
    case ssl_result of
        Left ssl -> return (Left $ PushFunctions (send_notification_internal ssl))
        Right err -> return (Right $ err)
                
get_feedback :: SSLServerConfig -> IO (Either Feedback SSLConfigError)
get_feedback config = do 
    result <- start_read_only_and_download config
    case result of
        Right x -> return $ Right x
        Left bytes -> return $ Left $ B.decode bytes
     
    
sandbox_feedback_config c p = SSLServerConfig c p "feedback.sandbox.push.apple.com" 2196
production_feedback_config c p = SSLServerConfig c p "feedback.push.apple.com" 2196

sandbox_push_config c p = SSLServerConfig c p "gateway.sandbox.push.apple.com" 2195
production_push_config c p = SSLServerConfig c p "gateway.push.apple.com" 2195

connect_push_sandbox :: FilePath -> FilePath -> IO (Either PushFunctions SSLConfigError)
connect_push_sandbox certificate private_key = connectToService (sandbox_push_config certificate private_key)

connect_push_production :: FilePath -> FilePath -> IO (Either PushFunctions SSLConfigError)
connect_push_production certificate private_key = connectToService (production_push_config certificate private_key)

--connect_and_send :: FilePath -> FilePath -> Payload -> IO ()
--connect_and_send c p payload = 


get_sandbox_feedback :: FilePath -> FilePath -> IO (Either Feedback SSLConfigError)
get_sandbox_feedback certificate private_key = get_feedback (sandbox_feedback_config certificate private_key)

get_production_feedback :: FilePath -> FilePath -> IO (Either Feedback SSLConfigError)
get_production_feedback certificate private_key = get_feedback (sandbox_feedback_config certificate private_key)

{-

I am struggling here
start from the top and work down

-}


send_notification_cmd :: SendInput -> IO ()
send_notification_cmd = undefined






    