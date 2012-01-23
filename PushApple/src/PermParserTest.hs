{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}
module PermParserTest where
import Data.Char
    
import Text.Parsec
import Text.Parsec.Perm

test_0 :: String
test_0 = " -a hey -b you"

test_1 :: String
test_1 = "abbaab" 

word = many1 (satisfy (not . isSpace))

a_parser = do 
    spaces
    char '-'
    char 'a'
    spaces
    word
    
b_parser = do 
    spaces
    char '-'
    char 'b'
    spaces
    word

parser_0 = permute (
                        (,) <$$> try a_parser
                            <||> try b_parser
                    )
                      
                      
run_parser = runParser parser_0 () "" test_0


    
                        