{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

{-# OPTIONS -Wno-unused-matches   #-}
{-# OPTIONS -Wno-unused-top-binds #-}

-- Depend on yesod-core directly rather than the `yesod` metapackage :
-- the metapackage transitively pulls in yesod-persistent and, from
-- there, the deprecated persistent-template ( now shipped on Hackage
-- with an empty exposed-modules list, which cabal treats as an
-- unbuildable library and refuses to solve under GHC 9.14.1 ). The
-- kbgen service has never used forms or DB persistence -- only
-- routing, TH sugar, JSON handling, all of which live in yesod-core.
-- Yesod.Core re-exports ToJSON / Value / object / .= from aeson, so
-- no separate aeson import is needed here.
import Yesod.Core
import Prelude
import GHC.Generics

-- project imports
import Factify
import Callable

data Healthy = Healthy Bool deriving ( Generic )

instance ToJSON Healthy where toJSON (Healthy status) = object [ "healthy" .= status ]

data App = App

mkYesod "App" [parseRoutes|
/kbgen HomeR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postHomeR :: Handler Value
postHomeR = do
    callable <- requireCheckJsonBody :: Handler Callable
    returnJson $ factify callable

main :: IO ()
main = warp 3000 App
