{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String

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
server multipartData = return str
    where str = "The form was submitted with "
             ++ show nInputs ++ " textual inputs and "
             ++ show nFiles  ++ " files."
          nInputs = length (inputs multipartData)
          nFiles  = length (files multipartData)
