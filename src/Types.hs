
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson             (ToJSON (..), object, toJSON, (.=))
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import           Sound.Wav.Parser.Types

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
