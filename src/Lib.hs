{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where

import           Control.Monad            (forM)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (ToJSON (..), object, toJSON, (.=))
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setLogger, setPort)
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           Servant.Multipart
import           Sound.Wav.Parser         (parseWavFile)
import           Sound.Wav.Parser.Types   (Data (..), Format (..), Riff (..),
                                           Wav (..))

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
    deriving (Eq, Show)

instance ToJSON WavResponse where
    toJSON (WavResponse fileName (Left metadata)) =
        object [ fileName .= metadata ]
    toJSON (WavResponse fileName (Right metadata)) =
        object [ fileName .= metadata ]

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
