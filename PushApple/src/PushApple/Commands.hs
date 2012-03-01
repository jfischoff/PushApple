module PushApple.Commands where  
import PushApple
import PushApple.AppContext
    
data OutgoingPayload = OutgoingPayload
    {
        outgoing_payload_device_token          :: DeviceToken,
        outgoing_payload_payload               :: Payload
    }
    
data PayloadConfig a = PayloadConfig 
    {
        payload_config   ::  a,
        outgoing_payload :: Payload
    }
    
data TopLevel = TopLevel
    {
        env_certificate_file_path :: String,
        env_private_key_file_path :: String,
        env_gateway_options       :: ConnectOptions,
        env_gateway_strings       :: [(ConnectOptions, String, Int)]
    }

data MainInput      = SendInput (PayloadConfig TopLevel) | FeedbackInput TopLevel 
--type SendEnv        = PayloadConfig SSLConfig
--type PushServiceEnv = PayloadConfig SSL

type MainContext l = AppContext MainInput () l

to_send_command_input = undefined

--main_command :: MainContext l
--main_command = do
--    local $ \input -> 
--                case input of
--                    SendInput x     -> run_app_context def input  
--                    FeedbackInput x -> run_app_context def input  
    

--send_command :: PushAppleContext l
--send_command = push_context to_send_command_input send_command'

--the basic idea is that I am rewriting things so that 
--the commands are built up from sub commands


--connect_to_service :: AppContext SendEnv () l
--connect_to_service = connect_to_service push_payload

--send_command' :: AppContext SendEnv () l
--send_command' = connect_to_service push_payload
    
    
    
    