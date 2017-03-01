{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Optimization
import qualified DamagePen as DP
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

data Setting = Setting { blink :: Bool, ward :: Bool } deriving (Show, Generic)
instance ToJSON Setting
instance FromJSON Setting

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do

    -- served like /index.html
    middleware $ staticPolicy (noDots >-> addBase "static/html")
    middleware $ staticPolicy (noDots >-> addBase "static/dist")

    get "/hello/:name" $ do
        name <- param "name"
        text ("hello " <> name <> "!")

    post "/dps" $ do
      setting <- jsonData :: ActionM Setting
      json $ DP.maxDps (ward setting) (blink setting)

    post "/test" $ do
      w <- (param "ward") `rescue` (\x -> return False)
      blink <- (param "blink") `rescue` (\x -> return False)
      let ward  = if w then "yes" else "no"
      let blinke  = if blink then "yes" else "no"
      text ("Ward: " <> ward <> "  -  Blink: " <> blinke )

    get "/users/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allUsers)
