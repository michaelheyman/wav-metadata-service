{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import qualified Data.Text.IO             as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart
import           Sound.Wav.Parser
import           Sound.Wav.Parser.Types
import           Stubs

data User = User
    { userId        :: Int
    , userFirstName :: String
    , userLastName  :: String
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance ToJSON Wav where
    toJSON (Wav riffChunk fmtChunk dataChunk) =
        object []

type API = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [Wav]

startApp :: IO ()
startApp = do
    putStrLn message
    run port app
  where
    message = "Running server: " ++ "http://localhost:" ++ show port
              ++ "\nRunning API: " ++ "\thttp://localhost:" ++ show port ++ "/upload"
    port = 8080

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server multipartData = liftIO $
    forM (files multipartData) $ \file -> do
        let wavFile = fdPayload file
        result <- parseWavFile wavFile
        case result of
            Left e    -> return emptyWav
            Right wav -> do
                putStrLn $ "Retrieved wav file" ++ show wav
                return wav

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
