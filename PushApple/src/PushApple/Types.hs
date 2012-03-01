module PushApple.Types where
import Data.ByteString	
import Control.Concurrent.Chan
import Data.Aeson 
import Data.Word
import qualified Data.HashMap.Strict as H


data Input = FeedBackInput 
           | SendInputType
           
data FeedBackInput = FeedbackInput

data ConnectOptions = Production
                    | Sandbox
                deriving(Show)

data SendInput = SendInput 
    {
        gateway :: String,
        certificate_file_path :: FilePath,
        key_file_path :: FilePath,
        payload :: Payload
    }
        deriving(Show)

-- | Device Token
newtype DeviceToken = DeviceToken ByteString
    deriving(Show, Read, Eq, Ord)
    

data Notification = Notification 
    {
        alert      :: Maybe Alert,
        badge      :: Maybe Int,
        sound      :: Maybe String,
        extra_data :: Object
    }
    deriving(Show, Eq)
    

              
data Alert = Simple String 
           | Complex 
             {
                body           :: String,
                action_loc_key :: Maybe String,
                loc_args       :: Maybe [String],
                launch_image   :: String
             }
            deriving(Show, Read, Eq, Ord)
         
data Payload = Payload DeviceToken Notification
            deriving(Show, Eq)
            
data Feedback = Feedback Word32 Payload
    deriving(Show, Eq)
            
