module Main where
import ApplePush2.ApplePush2
import Control.Monad
import qualified OpenSSL as SSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import ApplePush.Helpers
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import ApplePush2.ProcessLauncher
import Control.Arrow
import Control.Concurrent
import Control.Applicative
import Debug.Trace
import ApplePush2.CommandLineParser
import Text.Parsec hiding (token)
{-

processes = ([launch_if' push_service_active (arr launch_push_service),
              launch_if' feedback_service_active (arr launch_feedback_service) 
             ], print)


    
launch_if :: Settings -> (Settings -> Bool) -> IOWrappedProcess a BS.ByteString -> IOWrappedProcess a BS.ByteString
launch_if s f p = if (f s)
                    then p
                    else arr $ const $ return Nothing
        
launch_if' = launch_if settings

-}

data Settings = Settings 
    {
        certificate_file_path :: String,
        private_key_file_path :: String
    } 
    
settings = Settings "PushChatCert.pem" "PushChatKey.pem"

--token = DeviceToken $ hexTokenToByteString "f677806c16bb0b4f2cbd9ada93edadb39c7c5bc5dd60d78e886f9de52abb9df3"

token = DeviceToken $ hexTokenToByteString "0000806c16bb0b4f2cbd9ada93edadb39c7c5bc5dd60d78e886f9de52abb0000"

--process_line push_functions = do
--    line <- getLine
--    let notification = Notification (Just (Simple line)) Nothing Nothing $ H.empty
--    let send_notification' = send_notification push_functions
--    send_notification' token notification
--    return Nothing
    
--message_loop push_functions = forever (process_line push_functions) 

line_process = (
    const (),
    \_ -> getLine,
    const ()
    )
    
-- need to go from a line to 
type LineHandler m a b = (String -> ((a -> m b), a))

handle_line handler input = result where
    (command, command_input) = handler input
    result = command command_input
    
to_updater updater = (const (), updater, const ()) 

command_line_processor = to_updater . handle_line

parseCommandLineIO parser input = do
    result <- parseCommandLine parser input
    case result of
        Left x -> print $ "error " ++ (show x)
        Right x -> print $ "success " ++ (show x)

cmd_list_parser = command_list_parser top_command_parser



cmd_parser = parseCommandLineIO cmd_list_parser
cmd_process = to_updater . cmd_parser 



--I need to go from a list of functions with names to parser that returns those functions and the rest of the string
--then I need a parser that depending on the command parses the input



        
--launch_push_service :: Settings -> IO (Maybe String)
--launch_push_service settings = do
--    let config = push_service_config settings
--    connection_result <- connectToService config 
--    case connection_result of
--        Left push_functions -> message_loop $ push_functions 
--        Right ssl_error     -> return $ Just $ show $ ssl_error

--main = SSL.withOpenSSL $ do 
--    launch_push_service settings
     
