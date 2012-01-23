{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, TupleSections, OverlappingInstances #-}
module ApplePush2.Instances where
    
import ApplePush2.Types
import qualified Data.Binary as B
import Data.DeriveTH
import Language.Haskell.TH
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative ((<$>), (<*>))
import qualified Data.Aeson as A
--import qualified Data.Aeson.Types as AT
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Monad
import Test.QuickCheck
import qualified Data.Attoparsec.Number as N
import qualified Data.Attoparsec as AT
import Data.List
import ApplePush2.Classes
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Pos
import Text.Parsec.Token
import Data.Word
import Data.Functor.Identity
import Text.Parsec.Error
import Text.Parsec.Perm
import Data.Char
    
-- | Converts a hex string to a device token

hexTokenToByteString [] = BS.empty
hexTokenToByteString (a:[]) = BS.empty
hexTokenToByteString (a:b:r) = do
	BS.append (BS.pack [(read (concat $ ["0x", [a], [b]]) :: Word8)]) $ hexTokenToByteString r
    
instance Equivalent Bool where

instance FromCommandLine Bool where
    fromCommandLine = try (do {try (string "True") <|> try (string "TRUE") <|> string "true"; return True}) 
                   <|> do {try (string "False") <|> try (string "FALSE") <|> string "false"; return False}  
                   
instance ToCommandLine Bool where    
    
instance Equivalent Char where    
    
instance FromCommandLine Char where
    fromCommandLine = charLiteral haskell
    
instance ToCommandLine Char where 
    
word = many1 (satisfy (not . isSpace))    

instance FromCommandLine String where
    fromCommandLine = try (stringLiteral haskell) <|> word

instance Equivalent Int where

instance FromCommandLine Int where
    fromCommandLine = do 
        value <- integer haskell
        return $ fromInteger value

instance ToCommandLine Int where
    
instance FromCommandLine a => FromCommandLine (Maybe a) where
    fromCommandLine = optionMaybe fromCommandLine     
    
instance FromCommandLine a => FromCommandLine [a] where
    fromCommandLine = (brackets haskell) (fromCommandLine `sepBy` (char ','))

instance (Equivalent a) => Equivalent ([a]) where
    x =~= y | length x /= length y = False
            | otherwise = all id $ zipWith (\ex ey -> ex =~= ey) x y
            
instance (Equivalent a) => Equivalent (Maybe a) where
    (Just x) =~= (Just y) = x =~= y
    Nothing =~= Nothing = True
    _ =~= _ = False 

approxEq :: Double -> Double -> Bool
approxEq a b = a == b ||
               d < maxAbsoluteError ||
                 d / max (abs b) (abs a) <= maxRelativeError
    where d = abs (a - b)
          maxAbsoluteError = 1e-15
          maxRelativeError = 1e-15

instance Equivalent Double where
    (=~=) = approxEq
    
 

split_in_half list = result where
    half_length = (length list) `div` 2
    result = (\(x, y) -> [x, y]) $ splitAt half_length list
    
split_to_empty list = if length list > 1 
        then split_in_half list
        else []

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink text = map T.pack $ split_to_empty $ T.unpack text
     
instance Equivalent T.Text where
    
instance FromCommandLine T.Text where
    fromCommandLine = do
        value <- stringLiteral haskell
        return $ T.pack value
    
instance Equivalent BSC.ByteString where   
instance Equivalent DeviceToken where

instance FromCommandLine DeviceToken where
    fromCommandLine = do
        as_string <- count 64 (oneOf "0123456789abcdef")
        return $ DeviceToken $ hexTokenToByteString as_string

instance Arbitrary DeviceToken where
    arbitrary = do
        bytes <- vectorOf 32 arbitrary
        return $ DeviceToken $ BSC.pack bytes
        
instance Equivalent A.Value where
    (A.Object x) =~= (A.Object y)  = x =~= y
    (A.Array x)  =~= (A.Array y)   = x =~= y
    (A.String x) =~= (A.String y)  = x =~= y
    (A.Number x) =~= (A.Number y)  = x =~= y
    (A.Bool x) =~= (A.Bool y)      = x =~= y
    (A.Null)     =~= (A.Null)      = True
    _ =~= _                        = False
    
instance FromCommandLine A.Value where
    fromCommandLine = atto_to_parser A.json
  
instance FromCommandLine A.Object where
    fromCommandLine = atto_to_parser $ ((\(A.Object x) -> x) <$> A.json) 
  
to_parser :: (String -> Either String (a, String)) -> ParsecT String u Identity a
to_parser f = do
    input <- getInput
    let result = f input
    case result of
        (Left message) -> fail message
        (Right (value, new_input)) -> do
                                        setInput new_input
                                        return value
                        
atto_parse :: AT.Parser a -> String -> Either String (a, String)
atto_parse atto input_string = result where
        parse_result = AT.parse atto (BSC.pack input_string)
        result = case parse_result of
                    (AT.Done bs r) -> Right (r, BSC.unpack bs)
                    (AT.Fail _ contexts error) -> Left $ "Attoparsec error: " ++ error ++ " in " ++ (concat contexts)
    
atto_to_parser :: AT.Parser a -> ParsecT String u Identity a
atto_to_parser atto = to_parser (atto_parse atto)
    
    
instance (Equivalent a, Equivalent b) => Equivalent (a, b) where
    (x, y) =~= (x', y') = x =~= x' && y =~= y'

instance Equivalent (H.HashMap T.Text A.Value) where
    x =~= y = (H.toList x) =~= (H.toList y)
            
instance Arbitrary (H.HashMap T.Text A.Value) where
    arbitrary = sized $ \n -> resize (n `div` 2) $ 
        do
            pairs <- arbitrary
            return $ H.fromList pairs
    shrink hmap = map H.fromList $ split_to_empty $ H.toList hmap
    
instance Equivalent (V.Vector A.Value) where
    x =~= y = (V.toList x) =~= (V.toList y)
        
instance Arbitrary (V.Vector A.Value) where
    arbitrary = sized vec_n_arb
    shrink array = map V.fromList $ split_to_empty $ V.toList array
        
vec_n_arb n = resize (n `div` 2) $ 
    do 
        elements <- arbitrary
        return $ V.fromList elements
        
instance Equivalent Integer where
            
instance Equivalent (N.Number) where
    (N.D x) =~= (N.D y) = x =~= y
    (N.I x) =~= (N.I y) = x =~= y
        
instance Arbitrary N.Number where
    arbitrary = do
        is_int <- arbitrary
        if is_int 
            then N.I <$> arbitrary
            else N.D <$> arbitrary
        
instance Arbitrary A.Value where
    arbitrary = do
        option <- choose(0, 5) :: Gen Int
        case option of
            0 -> A.Object <$> arbitrary
            1 -> A.Array <$> arbitrary
            2 -> A.String <$> arbitrary
            3 -> A.Number <$> arbitrary
            4 -> A.Bool <$> arbitrary
            5 -> return A.Null
        
    shrink (A.Object v) = map A.Object $ shrink v
    shrink (A.Array v)  = map A.Array $ shrink v
    shrink (A.String v) = map A.String $ shrink v
    shrink (A.Number v) = []
    shrink (A.Bool v)   = []
    shrink A.Null       = []
    
list_or_value total_length list value = if total_length > 0 && length list == 0 then [value] else list     

instance Equivalent Alert where
    (Simple x) =~= (Simple y) = x =~= y
    (Complex x y z w) =~= (Complex x' y' z' w') = x =~= x' &&
        y =~= y' && z =~= z' && w =~= w'
    _ =~= _ = False
    
runParser' p input = runParser p () "" input

runShortFlag short p input = runParser' (short_flag_parser short p) input

runShortFlagFrom short input = runShortFlag short (fromCommandLine) input

g_flag_parser prefix name p = do
    spaces
    string (prefix ++ name)
    spaces
    p    
    
short_flag_parser = g_flag_parser "-"

long_flag_parser = g_flag_parser "--"
    
runFlagFrom long short input = runParser' (flag_parser long short fromCommandLine) input 

flag_parser long short p = try (short_flag_parser short p) <|> long_flag_parser long p
    
optional_flag_parser long short p = (Nothing, try (flag_parser long short p))

runFrom input = runParser' fromCommandLine input

type ParseResult a = Either ParseError a
    
complex_alert_parser = permute (Complex <$$> try (flag_parser "body" "b" fromCommandLine)
                                        <|?> optional_flag_parser "action_loc_key" "a" fromCommandLine
                                        <|?> optional_flag_parser "loc_args" "l" fromCommandLine
                                        <||> try (flag_parser "image" "i" fromCommandLine)
                                )
                                                                     
    
simple_alert_parser = do
    message <- fromCommandLine
    return $ Simple message
    
instance FromCommandLine Alert where
    fromCommandLine = (try complex_alert_parser) <|> simple_alert_parser
    

instance Arbitrary Alert where
    arbitrary = do
        is_simple <-arbitrary
        if is_simple 
            then Simple <$> arbitrary
            else Complex <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (Simple message) = map Simple $ shrink message
    shrink (Complex body action_loc_key loc_args launch_image) = result where
        
        bs   = shrink body
        as   = shrink action_loc_key
        locs = shrink loc_args
        lis  = shrink launch_image
        
        total_length = length bs + length as + length locs + length lis
        
        bs' = list_or_value total_length bs body
        as' = list_or_value total_length as action_loc_key
        locs' = list_or_value total_length locs loc_args
        lis' = list_or_value total_length lis launch_image
        
        result = [ Complex b a loc li | 
                        b   <- bs',
                        a   <- as',
                        loc <- locs',
                        li  <- lis'
                    ]
               
instance Equivalent Notification where
    (Notification x y z w) =~= (Notification x' y' z' w') = x =~= x' &&
        y =~= y' && z =~= z' && w =~= w'

instance Arbitrary Notification where
    arbitrary = Notification <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (Notification alert badge sound extra) = result where
        as = shrink alert
        ss = shrink sound
        es = shrink extra
        bs = shrink badge
        
        total_length = length as + length ss + length es + length bs
        
        as' = list_or_value total_length as alert
        ss' = list_or_value total_length ss sound
        es' = list_or_value total_length es extra
        bs' = list_or_value total_length bs badge
        
        result = [ Notification a b s e | 
                    a <- as',
                    s <- ss',
                    b <- bs',
                    e <- es'
                  ] 
        
instance FromCommandLine Notification where
    fromCommandLine = permute (Notification <$?> optional_flag_parser "alert" "a" fromCommandLine
                                            <|?> optional_flag_parser "badge" "b" fromCommandLine
                                            <|?> optional_flag_parser "sound" "s" fromCommandLine
                                            <|?> (H.empty, try (flag_parser     "etc"   "e" fromCommandLine))
                               )  

$(derive makeArbitrary ''Payload) 

instance FromCommandLine Payload where
    fromCommandLine = permute (Payload <$$> try (flag_parser "token" "t" fromCommandLine)
                                       <||> try (flag_parser "notification" "n" fromCommandLine))

instance Equivalent Payload where         
    (Payload x y) =~= (Payload x' y') = (x =~= x') && (y =~= y')
    
instance B.Binary Feedback where
    put (Feedback time payload) = do
        B.put time
        B.put payload
        
    get = do
        time <- B.get
        payload <- B.get
        return $ Feedback time payload
    
instance B.Binary Payload where
    put (Payload (DeviceToken token) notification) = do 
        P.putWord8 0
        P.putWord16be 32
        P.putByteString token
        B.put notification

    get = do 
        command      <- G.getWord8
        dummy0       <- G.getWord16be
        token        <- G.getByteString 32
        notification <- B.get
        return (Payload (DeviceToken token) notification)

instance B.Binary Notification where
    put notification = do
        let byte_string = A.encode notification
        P.putWord16be (fromIntegral $ BSL.length $ byte_string)
        P.putLazyByteString $ byte_string

    get = do
        len       <- G.getWord16be;
        byte_string <- G.getLazyByteString $ fromIntegral len
        return $ fromJust $ A.decode $ byte_string 
        
mk_string x = A.String $ T.pack x
        
        
instance A.ToJSON Notification where
    toJSON (Notification alert badge sound extra_data) = result where
        aps = H.fromList ["aps" A..= (A.object $
                   (maybe [] (\a -> ["alert" A..= A.toJSON a]) alert) ++ 
                   (maybe [] (\b -> ["badge" A..= A.toJSON b]) badge) ++ 
                   (maybe [] (\s -> ["sound" A..= A.toJSON s]) sound))
                   ]
            
        combined = H.union aps extra_data
        result = A.Object combined
            
            
instance A.ToJSON Alert where
    toJSON (Simple message) = mk_string message
    toJSON (message@(Complex _ _ _ _)) = A.object $
        [
            ("body", mk_string $ body message),
            ("launch_image", mk_string $ launch_image message)
        ] 
        ++ 
        (maybe [] (\loc_key -> [("action_loc_key", mk_string $ loc_key)]) $ 
            action_loc_key message)
        ++
        (maybe [] (\loc_args -> [("loc_args", A.Array $ V.fromList $ map mk_string $ 
            loc_args)]) $
            loc_args message)
    
instance A.FromJSON Notification where
    parseJSON (A.Object v) = do 
        aps         <- v A..: "aps"
        alert_value <- aps A..:? "alert"
        badge_value <- aps A..:? "badge"
        sound_value <- aps A..:? "sound"
        
        let not_aps = not . ("aps"==) . fst
            extra_data = H.fromList $ filter not_aps $ H.toList v
        
        
        return $ Notification alert_value badge_value sound_value $ extra_data
    parseJSON _ = mzero
        
instance A.FromJSON Alert where 
    parseJSON (A.Object alert_object) = do
        body <- alert_object A..: "body"
        launch_image <- alert_object A..: "launch_image"
        action_loc_key <- alert_object A..:? "action_loc_key"
        loc_args <- alert_object A..:? "loc_args"
        return $ Complex body action_loc_key loc_args launch_image 
        
        
    parseJSON (A.String message) = return $ Simple $ T.unpack message
    parseJSON _ = mzero
        
        
try_flag_parser l s p = try $ flag_parser l s p
        
instance FromCommandLine ConnectOptions where
    fromCommandLine = try ( flag_parser "production" "p" (return Production)) <|> return Sandbox

instance FromCommandLine SendInput where 
        fromCommandLine = permute ( SendInput <$$> try fromCommandLine
                                              <||> try_flag_parser "cert_file_path" "c" fromCommandLine
                                              <||> try_flag_parser "key_file_path" "k" fromCommandLine
                                              <||> try fromCommandLine
                                  )
            

