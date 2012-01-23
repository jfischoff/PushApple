-- | This module implements the Apple Push Notification Service 
-- <http://developer.apple.com/iPhone/library/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Introduction/Introduction.html#//apple_ref/doc/uid/TP40008194-CH1-SW1>
--
-- The notification service uses 'Control.Concurrent.Chan' for asynchronous communication.
-- Call 'connectToNotificationService' and pass it a 'NotificationCallbackChan'. 
-- The notification service will post a 'NotificationServerConencted' message, with a channel that you should use to send notifications with.
module ApplePush (
module ApplePush.Types,
module ApplePush.Helpers,
module ApplePush.Notification,
connectToNotificationService
) where
	
import ApplePush.Types
import ApplePush.Notification
import ApplePush.Helpers

import Control.Monad (liftM)
import Network.BSD

import Network.Socket
import qualified OpenSSL.Session as SSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import System.IO
import Data.Binary.Put
import Control.Concurrent.Chan
import Control.Concurrent	
import qualified Control.Exception(catch)
	
handleMsg :: SSL.SSL -> NotificationServiceMsg -> IO ()	
handleMsg hdl (NotificationServiceSend token payload) = do
	let y = BS.pack $ BSL.unpack $ runPut $ do
		putWord8 0 {- command -}
		putWord16be 32 {- token -}
		putByteString token
		putWord16be (fromIntegral $ length payload) {- payload length -}
		putByteString $ BSC.pack payload
	SSL.write hdl y
	
	--hFlush hdl
handleMsg hdl NotificationServiceExit = do
	return () 
handleMsg hdl msg = return ()

notificationServiceHandler :: SSL.SSL -> NotificationServiceChan -> NotificationCallbackChan -> IO ()
notificationServiceHandler hdl sc cc = do
	msg <- readChan sc
	handleMsg hdl msg
	notificationServiceHandler hdl sc cc

onConnect :: SSL.SSL -> NotificationCallbackChan -> IO ()
onConnect hdl callback = do
	c <- newChan
	writeChan callback (NotificationServerConnected c)
	notificationServiceHandler hdl c callback `catch` (\e -> writeChan callback NotificationServerDisconnected)
	
getConnection :: FilePath -> FilePath -> String -> Integer -> IO (Either String SSL.SSL)
getConnection private_key cert h p = withSocketsDo $ do

    sock <- socket AF_INET Stream 0
    addrs <- liftM hostAddresses $ getHostByName h
    connect sock $ SockAddrInet (fromIntegral p) (head addrs)
    --setSocketOption sock ReuseAddr 1
    print "hey"
    ssl_context <- SSL.context
    SSL.contextSetCertificateFile ssl_context cert
    SSL.contextSetPrivateKeyFile ssl_context private_key
    SSL.contextSetCiphers ssl_context "DEFAULT"
    SSL.contextCheckPrivateKey ssl_context >>= print
    ssl_connection <- SSL.connection ssl_context sock
    SSL.connect ssl_connection
    return $ Right ssl_connection
	
-- | Connects to the notification service for the host and port specified.
connectToNotificationService :: FilePath -> FilePath -> String -> Integer -> NotificationCallbackChan -> IO ()
connectToNotificationService private_key cert host port callback = do
	tid <- forkIO f 
	return ()
	where 
		f = do
			hdl <- getConnection private_key cert host port `catch` (\e -> return $ Left $ show e)
			case hdl of
				Right handle -> onConnect handle callback
				Left msg -> writeChan callback (NotificationServerUnableToConnect msg)
	
