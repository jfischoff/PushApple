-- | Types for the Apple Push Service
module ApplePush.Types (
DeviceToken,
NotificationServiceMsg(..),
NotificationServiceChan,
NotificationCallbackMsg(..),
NotificationCallbackChan,
NotificationPayload,
NotificationAction
) where
	
import Data.ByteString	
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.State as SM
import Text.JSON

-- | Device Token
type DeviceToken = ByteString

-- | These messages are sent to the Apple Push Notification Service
data NotificationServiceMsg = NotificationServiceSend DeviceToken String | NotificationServiceExit

type NotificationServiceChan = Chan NotificationServiceMsg


-- | Callback messages from the Apple Push Notification Service
data NotificationCallbackMsg = NotificationServerConnected NotificationServiceChan | 
	NotificationServerUnableToConnect String | NotificationServerDisconnected

type NotificationCallbackChan = Chan NotificationCallbackMsg


-- | Notification payload is a JSON Object
type NotificationPayload = JSObject JSValue

-- | Notification actions
type NotificationAction = JSObject JSValue