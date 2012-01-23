-- | Creates JSON notification payloads for the Apple Push Notification Service
module ApplePush.Notification (
makeRawNotification,
makeNotification
) where
	
import Text.JSON

import ApplePush.Types


-- | Makes a notification, takes an alert(string/dictionary), integer, string for the sound, and any user data (key/value dictionary.)
makeNotification :: Maybe (Either String [(String, JSValue)]) -> Maybe Integer -> Maybe String -> Maybe [(String, JSValue)] -> String
makeNotification alert badge sound ud = do
	encodeStrict $ makeObj $ [("aps", makeObj ((mkAlert alert) ++ (mkBadge badge) ++ (mkSound sound)))] ++ (mkUD ud)
	where
		mkSound (Just x) = [("sound", showJSON $ x)]
		mkSound Nothing = []
		mkBadge (Just x) = [("badge", showJSON $ x)]
		mkBadge Nothing = []
		mkUD (Just x) = x
		mkUD Nothing = []
		mkAlert (Just (Right x)) = mkRawAlert $ makeObj x
		mkAlert (Just (Left x)) = mkRawAlert $ showJSON x
		mkAlert Nothing = []
		mkRawAlert x = [("alert", x)]

makeNotification Nothing Nothing Nothing Nothing = makeRawNotification [("aps", showJSON $ "")]


-- | Takes a list of key, JSValue pairs and converts them to a string
makeRawNotification :: [(String, JSValue)] -> String
makeRawNotification x = do
	encodeStrict $ makeObj x