{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Control.Monad.Trans
import qualified Optimization as OP
import qualified DamagePen as DP
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))

data Setting = Setting { blink :: Bool, ward :: Bool } deriving (Show, Generic)
instance ToJSON Setting
instance FromJSON Setting

-- postsIndex :: ActionT T.Text ConfigM ()
-- postsIndex = do
--     posts <- runDb (selectList [] [])
--     html $ "This many posts!<br>" <> T.pack (show (length (posts :: [Entity BlogPost])))

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do

    -- served like /index.html
    middleware $ staticPolicy (noDots >-> addBase "static/html")
    middleware $ staticPolicy (noDots >-> addBase "static/dist")

    post "/dps" $ do
      setting <- jsonData :: ActionM Setting
      json $ DP.maxDps (ward setting) (blink setting)

    post "/optimize" $ do
      build <- jsonData :: ActionM OP.Build
      r <- liftIO $ OP.optimize build
      json r

