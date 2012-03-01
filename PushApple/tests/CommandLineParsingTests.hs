{-#LANGUAGE NoMonomorphismRestriction #-}
module CommandLineParsingTests where
import PushApple
import qualified Data.ByteString as BS
import Data.Default
import PushApple.CommandLineParser
import PushApple.AppContext

--okay think about this
--do I want to abstract this
--how much is it worth to just get this thing done?

--here is the idea
--every sub command creates readonly info for its children




    
instance Default Env where
    def = Env "" "" def def

key_file_path_test  = "key_file_path"
cert_file_path_test = "cert_file_path" 

device_token_string = "0000806c16bb0b4f2cbd9ada93edadb39c7c5bc5dd60d78e886f9de52abb0000"

notification_string = "-a test"

payload_string = "-t " ++ device_token_string ++ " -n" ++ notification_string 

send_command_string = "send " ++ " -k " ++ key_file_path_test ++
    " -c " ++ cert_file_path_test ++ " -p " ++ payload_string

command_line_tests = [
    ("send", send_command_string, def :: Env, def :: Env),
    ("feedback", "feedback", def, def)
    --("interactive", )
    --("set", )
    --("from_file", )
    ]

--write this out
--I have two things for a program
--the commands
--and the initial state
--so everything should be in the state command
--so every command 

test (name, input, start_env, end_env) = do
    print name
    print =<< parseCommandLine (command_list_parser top_command_parser) input

main = do
    mapM_ test command_line_tests