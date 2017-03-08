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

data Setting = Setting { hero_name :: String
                       , blink :: Bool
                       , ward :: Bool
                       , lifesteal :: Integer}
             deriving (Show, Generic)
instance ToJSON Setting
instance FromJSON Setting

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do

    -- served like /index.html
    middleware $ staticPolicy (noDots >-> addBase "static/html")
    middleware $ staticPolicy (noDots >-> addBase "static/dist")

    post "/dps" $ do
      setting <- jsonData :: ActionM Setting
      json $ DP.maxDps  (ward setting) (blink setting) (lifesteal setting) (hero_name setting)

    post "/optimize" $ do
      build <- jsonData :: ActionM OP.Build
      r <- liftIO $ OP.optimize build
      json r

