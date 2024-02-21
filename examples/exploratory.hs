{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Maybe (fromMaybe)
import Music.Prelude
import System.Environment (getEnvironment)

import ExploratoryRhythms (rhythms)
import ExploratoryScales (scales)
import ExploratorySolfeggio (solfeggio)
import ExploratoryStrings (strings)

-- run this if adding more modules: gen-hie > hie.yaml

exercises :: String -> IO Music
exercises =
  fromMaybe (pure scales)
    . flip
      lookup
      [ ("scales", pure scales)
      , ("strings", pure strings)
      , ("rhythms", rhythms)
      , ("solfeggio", solfeggio)
      ]

main :: IO ()
main = defaultMain =<< exercises . fromMaybe "scale" . lookup "e" =<< getEnvironment
