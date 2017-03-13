{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Types
import Control.Monad.Trans
import qualified Optimization as OP
import qualified DamagePen as DP
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do

    -- served like /index.html
    middleware $ staticPolicy (noDots >-> addBase "static/html")
    middleware $ staticPolicy (noDots >-> addBase "static/dist")

    post "/dps" $ do
      setting <- jsonData :: ActionM UISetting
      json $ DP.maxDps (has_ward setting) (has_blink setting) (desired_lifesteal setting) (hero_name setting) 0

    post "/optimize" $ do
      build <- jsonData :: ActionM OP.Build
      r <- liftIO $ OP.optimize build
      json r

