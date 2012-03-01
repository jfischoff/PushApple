module PushApple.Service where

import Network.BSD
import Network.Socket
import qualified OpenSSL.Session as SSL
import OpenSSL.X509
import OpenSSL.EVP.PKey
import Control.Monad (liftM, forever)
import Debug.Trace.Helpers
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import Control.Applicative

    
data SSLServerConfig = SSLServerConfig
    {
        cert_file_path   :: FilePath,
        private_key_path :: FilePath,
        host        :: String,
        port        :: Int
    } 
    deriving(Show, Read, Eq, Ord)

data SSLConfigError = InvalidPrivateKey
    deriving(Show, Read, Eq, Ord)
    
connect_to_host :: String -> Int -> IO Socket    
connect_to_host host port = do
    sock <- socket AF_INET Stream 0
    addrs <- liftM hostAddresses $ getHostByName $ host
    connect sock $ SockAddrInet (fromIntegral port) (head $ addrs)
    return $ sock
    
create_ssl_connection :: Socket -> FilePath -> FilePath -> IO (Either SSL.SSL SSLConfigError)
create_ssl_connection sock cert_file_path private_key_path = do
    --create a secure socket
    ssl_context <- SSL.context
    SSL.contextSetCertificateFile ssl_context cert_file_path
    SSL.contextSetPrivateKeyFile ssl_context private_key_path
    SSL.contextSetCiphers ssl_context "DEFAULT"
    valid_private_key <- SSL.contextCheckPrivateKey ssl_context
    if valid_private_key == False
        then return $ Right $ InvalidPrivateKey
        else do 
                ssl_connection <- SSL.connection ssl_context sock
                return $ Left ssl_connection

create_socket_and_ssl_connection :: SSLServerConfig -> IO (Either SSL.SSL SSLConfigError)
create_socket_and_ssl_connection config = do
    sock <- connect_to_host (host config) (port config)
    create_ssl_connection sock (cert_file_path config) (private_key_path config)
    

start_write_only :: SSLServerConfig -> IO (Either SSL.SSL SSLConfigError)
start_write_only config = do
    --connect to the host
    result <- create_socket_and_ssl_connection config
    case result of
            Right x -> return $ Right x
            Left connection -> do
                SSL.connect connection
                return $ Left connection
                
                
start_read_only :: SSLServerConfig -> IO (Either SSL.SSL SSLConfigError)
start_read_only config = do 
    result <- create_socket_and_ssl_connection config
    case result of
        Right x -> return $ Right x
        Left connection -> do
            SSL.connect connection
            return $ Left connection
            
start_read_only_and_download :: SSLServerConfig -> IO (Either BSL.ByteString SSLConfigError)
start_read_only_and_download config = do 
    result <- start_read_only config
    case result of
        Right x -> return $ Right x
        Left connection -> Left <$> SSL.lazyRead connection
 
--What I am say here, or implying is that the callback is checked for an error, which is rethrown
--to the caller of this function. 
--in otherwords the service is taken down on an error
--it is the callers job to restart            
start_read_only_with_a_callback :: SSLServerConfig -> (BSL.ByteString -> IO (Maybe BS.ByteString)) -> IO (Maybe BS.ByteString)
start_read_only_with_a_callback config callback = do
    connection_result <- start_read_only config
    case connection_result of
        Right x -> return $ Just $ BSC.pack $ show x
        Left connection -> read_loop connection callback

read_loop c callback = do
    bytes <- SSL.lazyRead c
    result <- if BSL.length bytes > 0
                then callback bytes
                else return Nothing
    if isJust result 
     then return $ result
     else read_loop c callback
     
shutdown_read = undefined
               
read_only_process = (
    start_read_only,
    read_loop, 
    shutdown_read
    )
                












                