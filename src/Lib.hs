{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Text.Encoding
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart
import           Sound.Wav.Parser
import           Sound.Wav.Parser.Types

instance ToJSON Riff where
    toJSON (Riff chunkID chunkSize chunkFormat) =
        object [ "id" .= decodeUtf8 chunkID
               , "size" .= chunkSize
               , "format" .= decodeUtf8 chunkFormat
               ]

instance ToJSON Format where
    toJSON (Format chunkID chunkSize audioFormat numChannels sampleRate byteRate blockAlign bitsPerSample) =
        object [ "id" .= decodeUtf8 chunkID
               , "size" .= chunkSize
               , "audioFormat" .= audioFormat
               , "numChannels" .= numChannels
               , "sampleRate" .= sampleRate
               , "byteRate" .= byteRate
               , "blockAlign" .= blockAlign
               , "bitsPerSample" .= bitsPerSample
               ]

instance ToJSON Data where
    toJSON (Data chunkID chunkSize chunkData) =
        object [ "id" .= chunkID
               , "size" .= chunkSize
               , "data" .= ("<omitted>" :: Text)
               ]

instance ToJSON Wav where
    toJSON (Wav riffChunk fmtChunk dataChunk) =
        object [ "riff" .= riffChunk
               , "fmt" .= fmtChunk
               , "data" .= dataChunk
               ]

data WavResponse = WavResponse
    { fileName :: Text
    , metadata :: Either String Wav
    }

instance ToJSON WavResponse where
    toJSON (WavResponse fileName (Left metadata)) =
        object [ fileName .= metadata ]
    toJSON (WavResponse fileName (Right metadata)) =
        object [ fileName .= metadata ]

type API = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [WavResponse]

startApp :: IO ()
startApp = do
    putStrLn runningMessage
    run port app
  where
    runningMessage = "Running server: " ++ "http://localhost:" ++ show port
                     ++ "\nRunning API: " ++ "\thttp://localhost:" ++ show port ++ "/upload"
    port = 8080

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server multipartData = waves
    where waves = liftIO . forM (files multipartData) $ \file -> do
            let fileName = fdFileName file
            putStrLn $ "Received request to parse " ++ show fileName
            let wavFile = fdPayload file
            result <- parseWavFile wavFile
            case result of
                Left e    -> do
                    putStrLn $ "Error parsing " ++ show fileName ++ ": " ++ e
                    return $ WavResponse fileName (Left "Not a valid WAV file")
                Right wav -> do
                    putStrLn $ "Successfully parsed " ++ show fileName
                    return $ WavResponse fileName (Right wav)
