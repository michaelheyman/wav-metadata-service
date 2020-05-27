{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server
    ( startApp
    , app
    ) where

import           Control.Monad            (forM)
import           Control.Monad.IO.Class   (liftIO)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setLogger, setPort)
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           Servant.Multipart
import           Sound.Wav.Parser         (parseWavFile)
import           Types

type API = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [WavResponse]

startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
    let settings = setPort port $ setLogger logger defaultSettings
    putStrLn runningMessage
    runSettings settings app
  where
    runningMessage = "Running server: " ++ baseUrl ++ ":" ++ show port
    baseUrl = "http://localhost"
    port = 8080

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = parseMultipart

parseMultipart :: MultipartData Tmp -> Handler [WavResponse]
parseMultipart multipartData = waves
    where waves = liftIO . forM (files multipartData) $ \file -> do
            let fileName = fdFileName file
            let wavFile = fdPayload file
            result <- parseWavFile wavFile
            case result of
                Left e    -> do
                    putStrLn $ "Error parsing " ++ show fileName ++ ": " ++ e
                    return $ WavResponse fileName (Left "Not a valid WAV file")
                Right wav -> return $ WavResponse fileName (Right wav)
