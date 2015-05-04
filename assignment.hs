{--# LANGUAGE OverloadedStrings #--}

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Text
import Foreign.Marshal.Unsafe
import GHC.Generics
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, uriRest, seeOther)
import Network.HTTP.Conduit (simpleHttp)

data Temperature = Temperature {
	date		:: String,
	temperature	:: Int
} deriving (Show,Generic)

instance FromJSON Temperature

instance ToJSON Temperature

-- File Path for local JSON data.
defaultFile = "default.json"

-- Gets JSON from website.
getURL :: String -> B.ByteString
getURL n = B.reverse $ B.drop 2 $ B.reverse $ B.drop 16 $ unsafeLocalState $ simpleHttp n

-- Gets JSON from local file.
getLocal :: B.ByteString
getLocal = unsafeLocalState $ B.readFile defaultFile

-- Gets JSON from another file.
getFile :: String -> B.ByteString
getFile n = unsafeLocalState $ B.readFile n

-- Reads JSON.
readJSON :: B.ByteString -> IO ()
readJSON n = Prelude.putStr $ show json
	where
		json = decode n :: Maybe [Temperature]

-- Prepares JSON to be appended to file.
prepJSON :: B.ByteString -> B.ByteString
prepJSON n = B.drop 2 n

-- Prepares JSON in file.
prepFile :: B.ByteString -> IO B.ByteString
prepFile n = evaluate $ B.append (B.reverse $B.drop 2 $ B.reverse n) (C.singleton ',')

--Creates new JSON file.
createJSON :: String -> B.ByteString -> IO ()
createJSON y n = B.appendFile y n

-- Appends JSON to file.
appendJSON :: B.ByteString -> IO ()
appendJSON n = B.appendFile defaultFile (prepJSON n)

-- Creates and writes to new file.
writeJSON :: B.ByteString -> IO ()
writeJSON n = B.writeFile defaultFile n

-- Appends JSON to file in a JSON readable format.
appends :: B.ByteString -> IO ()
appends n = do 
	new <- prepFile $ getLocal
	writeJSON new
	appendJSON n

-- HTTP Happstack Server
indexMessage :: String
indexMessage = "Instructions:\n\
				\ Append from URL - Change directory to /appendfromurl/URL\n\
				\ Append from File - Change directory to /appendfromfile/FILE\n\
				\ Create new file (can be used once only << to show functionality) - Change directory to /createfile/URL\n\
				\  Replacing URL with the actual URL and FILE with the file.\n\n" ++ "Local File:\n" ++ show getLocal

appendsURL :: String -> IO String
appendsURL x = do
	e <- try (appends $ getURL $ Prelude.drop 1 x) :: IO (Either SomeException ())
	case e of
		Left errorMessage -> return $ show errorMessage
		Right _ -> return "Appended"

appendsFile :: String -> IO String
appendsFile x = do
	e <- try (appends $ getFile x) :: IO (Either SomeException ())
	case e of
		Left errorMessage -> return $ show errorMessage
		Right _ -> return "Appended"

createFile :: String -> IO String
createFile x = do
	e <- try (createJSON "new.json" $ getURL $ Prelude.drop 1 x) :: IO (Either SomeException ())
	case e of
		Left errorMessage -> return $ show errorMessage
		Right _ -> return "Created"	

main :: IO ()
-- Create : createJSON "FILENAME" $ getURL "URLNAME"
-- Append from URL: appends $ getURL "URLNAME"
-- Append from File: appends $ getFile "FILENAME"
-- Read local File : readJSON getLocal
-- Read external File : readJSON $ getFile "FILENAME"
-- Read URL : readJSON $ getURL "URLNAME"
-- unsafeLocalState should be avoided since you need to manually restart the server again to see the changes.
main = simpleHTTP nullConf $
 	msum [ dir "index" $ ok $ indexMessage,
 		dir "appendfromurl" $ uriRest $ \x -> ok $ unsafeLocalState $ appendsURL x,
 		dir "appendfromfile" $ path $ \x -> ok $ unsafeLocalState $ appendsFile x,
 		dir "createfile" $ uriRest $ \x -> ok $ unsafeLocalState $ createFile x,
 		seeOther "index" "index"
        ]