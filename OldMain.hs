module Main where
import ProducerConsumer
--import qualified Notifier as N
import Control.Monad.State
import Test.QuickCheck.Checkers
import Control.Concurrent
import Control.Applicative
import Control.Concurrent.Thread.Delay
import AutoNotifier
import ApplePush
import ApplePush.Notification
import OpenSSL
import qualified OpenSSL.Session as SSL

import Data.List.Split
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Network.BSD
import Network.Socket

token = BS.pack [0xa5, 0xee, 0xf4, 0x86, 0x33, 0x2e, 
                 0x8e, 0x41, 0x7b, 0x5b, 0x35, 0xbd,
                 0xae, 0x89, 0x7c, 0x0e, 0xde, 0x5f, 
                 0xad, 0x50, 0x2a, 0xb5, 0xf5, 0xe9,
                 0xb5, 0x44, 0xc1, 0xe6, 0x17, 0x53,
                 0xcd, 0x5b]

--token = BS.pack $ take 32 (cycle [0])
                 
send_message = do 
    c <- newChan 
    start "PushChatKey.pem" "PushChatCert.pem" "gateway.sandbox.push.apple.com" 2195 c
    forever $ do
        line <- getLine
        let message = makeNotification (Just'- $ Left line) Nothing Nothing Nothing
        send_notification (NotificationServiceSend token message) c

receive_feedback = do    
    sock <- socket AF_INET Stream 0
    addrs <- liftM hostAddresses $ getHostByName "feedback.sandbox.push.apple.com"
    setSocketOption sock ReuseAddr 1    
    connect sock $ SockAddrInet (fromIntegral 2196) (head addrs)
    --listen sock 1
    --(sock', sockaddr) <- accept sock
    --print $ "Accepted connection from " ++ show sockaddr 

    ssl_context <- SSL.context
    SSL.contextSetCertificateFile ssl_context "cert_nokey.pem"
    SSL.contextSetPrivateKeyFile ssl_context "key_nokey.pem"
    SSL.contextSetCiphers ssl_context "DEFAULT"
    SSL.contextCheckPrivateKey ssl_context >>= print
    ssl_connection <- SSL.connection ssl_context sock
    SSL.connect ssl_connection
    results <- SSL.lazyRead ssl_connection
    print results

main = withOpenSSL $ do
    tid' <- forkIO receive_feedback
    send_message
    --print "hey"
