{--# LANGUAGE OverloadedStrings #--}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Text
import Foreign.Marshal.Unsafe
import GHC.Generics
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, seeOther)
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

-- Gets JSON from file.
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

-- Creates new JSON file. CURRENTLY NOT USED
createJSON :: String -> B.ByteString -> IO ()
createJSON y n = B.appendFile y n

-- Appends JSON to file.
appendJSON :: String -> B.ByteString -> IO ()
appendJSON y n = B.appendFile y (prepJSON n)

-- Creates and writes to new file.
writeJSON :: String -> B.ByteString -> IO ()
writeJSON y n = B.writeFile y n

-- Appends JSON to file in a JSON readable format.
appends :: String -> B.ByteString -> IO ()
appends y n = do 
	new <- prepFile $ getFile y
	writeJSON y new
	appendJSON y n

-- HTTP Happstack Server
data ExternalPath = File | URL

indexMessage :: String
indexMessage = "Instructions:\n\
--                \Create new JSON file from another file - Change directory to \\newjson\\FILENAME\\FILENAME\n\
--                \Create new JSON file from a URL - Change directory to \\newjson\\FILENAME\\URL\n\
				\Append to JSON file from another file - Change directory to \\append\\FILENAME\n\
				\Append to JSON file from a URL - Change directory to \\append\\URL\n\
				\Replacing FILENAME with the filename and URL with the URL"

main :: IO ()
-- Create : createJSON "FILENAME" $ getURL "URLNAME"
-- Append from URL: appends "FILENAME" getURL "URLNAME"
-- Append from File: appends "FILENAME" getFile "FILENAME"
-- Read File : readJSON $ getFile "FILENAME"
-- Read URL : readJSON $ getURL "URLNAME"
main = simpleHTTP nullConf $
 	msum [ dir "index" $ ok $ indexMessage,
 		--dir "newjson" path $ \ExternalPath,
 		--dir "append" path $ \ExternalPath, 
 		seeOther "index" "index"
        ]