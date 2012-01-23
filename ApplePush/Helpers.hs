-- | Helpers for the Apple Push Service
module ApplePush.Helpers (
hexTokenToByteString,
tokenToString,
byteStringToHex
) where	

import qualified Data.ByteString as BS
import Data.Word
import Numeric

import ApplePush.Types

byteStringToHex :: BS.ByteString -> String
byteStringToHex bs = do
	let s = concat $ map (\x -> (toHex x) ++ " ") (BS.unpack bs)
	take ((length s) - 1) s

toHex :: Word8 -> String
toHex b = case showHex b "" of
	      	a:b:[] -> a:b:[]
	      	a:[] -> '0':a:[]

-- | Converts a device token to a hex string
tokenToString :: DeviceToken -> String
tokenToString y = (concat $ map (\x -> showHex x "") (BS.unpack y))

-- | Converts a hex string to a device token
hexTokenToByteString :: String -> DeviceToken
hexTokenToByteString [] = BS.empty
hexTokenToByteString (a:[]) = BS.empty
hexTokenToByteString (a:b:r) = do
	BS.append (BS.pack [(read (concat $ ["0x", [a], [b]]) :: Word8)]) $ hexTokenToByteString r
	