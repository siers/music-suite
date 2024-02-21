{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExploratoryScales (scales) where

import Control.Lens (Lens')
import qualified Data.List.NonEmpty as N
import Music.Prelude

info :: (HasMeta c, HasPosition c, Transformable c) => Title -> KeySignature -> TimeSignature -> c -> c
info title' key sig =
  title title'
    . composer "-"
    . keySignature key
    . tempo (metronome (1 / 4) 69)
    . timeSignature sig

type M = Score StandardNote

vln1 :: Part
vln1 = head $ divide 2 violins

modeIntervals :: Lens' (Mode v p) (NonEmpty v)
modeIntervals f (Mode is) = Mode <$> f is

scaleSeq :: Scale Interval Pitch -> M
scaleSeq s = pseq ((|/ 2) . fromPitch <$> scaleToList s)

genScale3Oct :: Pitch -> Mode Interval Pitch -> M
genScale3Oct p scale' = scaleSeq (scale p (over modeIntervals mapScale scale')) |> (fromPitch p |* 3)
 where
  mapScale s = (s <> s <> s) <> N.reverse (N.map negate (s <> s <> s))

scales :: Music
scales =
  info "Scales" (key g MajorMode) (12 / 8) . compress 4 $
    set parts' vln1 (genScale3Oct g_ harmonicMinorScale)
