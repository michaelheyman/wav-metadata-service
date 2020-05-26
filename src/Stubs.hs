{-# LANGUAGE OverloadedStrings #-}

module Stubs where

import           Sound.Wav.Parser.Types

emptyWav :: Wav
emptyWav = Wav riff format dataChunk

riff :: Riff
riff = Riff { riffChunkID = "ex"
    , riffChunkSize = 10
    , riffChunkFormat = "aa"
    }

dataChunk :: Data
dataChunk = Data
    { dataChunkID = 10
    , dataChunkSize  = 10
    , dataData       = "a"
    }

format :: Format
format = Format
    { formatChunkID       = "a"
    , formatChunkSize     = 10
    , formatAudioFormat   = 10
    , formatNumChannels   = 10
    , formatSampleRate    = 10
    , formatByteRate      = 10
    , formatBlockAlign    = 10
    , formatBitsPerSample = 10
    }
