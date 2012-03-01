module Main where
    
import PushApple
import PushApple.CommandLineParser
import System.Environment
import Control.Applicative ((<$>))

main = do
    args <- concat <$> getArgs
    print =<< parseCommandLine (command_list_parser top_command_parser) args