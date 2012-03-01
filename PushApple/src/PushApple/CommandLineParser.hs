{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances #-}
module PushApple.CommandLineParser where
import PushApple.Types
import Text.Parsec 
import Text.Parsec.Char
import Control.Applicative ((<$>))
import Data.Maybe
import PushApple.Classes
import PushApple.Instances
import Data.Functor.Identity
import PushApple


data Data = S SendInput
          | Blank
    deriving(Show)

type Command m = CommandArg m -> m (CommandArg m)
    
instance Monad m => Show (CommandArg m -> m (CommandArg m)) where
    show x = " command "

data CommandList m = Cons (CommandArg m) (CommandList m)
                 | Nil (CommandArg m)
            deriving(Show)
                          
data CommandArg m = C (Command m)
                | D Data
            deriving(Show)
                
toData (D x) = x    
     
run_command' :: (Monad m, Functor m) => CommandList m -> m Data
run_command' x = toData <$> run_command x
    
--run_command :: Monad m => CommandList m -> m (CommandArg m)
run_command (Cons (C f) xs) = f =<< run_command xs
run_command (Nil x) = return x
run_command _ = error "only commands till the end"

--parseCommandLine :: (Stream String Identity Char, Monad n, Functor n) =>  
--                    ParsecT String () Identity (CommandList n) -> String -> n (Either ParseError Data)
parseCommandLine parser input = do
    let cmd_list = runParser parser () "" input
    case cmd_list of 
        Left x ->     return $ Left x
        Right list -> Right <$> (run_command' list)

type SubCommandParser s u m n = ParsecT s u m (CommandArg n, Maybe (ParsecT s u m ( CommandList n)))

command_list_parser :: SubCommandParser s u m n -> ParsecT s u m (CommandList n)
command_list_parser sub_command_parser = do
    (cmdarg, parser) <- sub_command_parser
    x <- if isJust parser
                then fromJust parser 
                else return $ Nil cmdarg
    return (Cons cmdarg x)
    



--my special case

--top_command_parser :: (Stream s m Char, Monad n) => SubCommandParser s u m n
top_command_parser = try send_parser <|> feedback_parser

--send_parser :: Stream s m Char => SubCommandParser s u m
send_parser = do
    string "send"
    spaces
    return (C send_command, Just send_input_parser)
    
--send_command :: CommandArg -> CommandArg
send_command (D (S send_input)) = do
    print $ "send_input " ++ (show send_input)
    return $ D $ Blank
    
--payload_parser :: ParsecT s u m CommandList
send_input_parser = do
    send_input <- fromCommandLine
    return (Nil $ D $ S send_input)


--feedback_parser :: Stream s m Char => SubCommandParser s u m
feedback_parser = do
    string "feedback"
    spaces
    return (C feedback_cmd, Nothing)

feedback_cmd _ = do
    print "feedback"
    return $ D $ Blank
    

    




