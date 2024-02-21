{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExploratoryStrings (strings) where

import Music.Prelude

info :: (HasMeta c, HasPosition c, Transformable c) => Title -> KeySignature -> TimeSignature -> c -> c
info title' key sig =
  id
    . title title'
    . composer "-"
    . keySignature key
    . tempo (metronome (1 / 4) 69)
    . timeSignature sig

type M = Score StandardNote

vln1 :: Part
vln1 = head $ divide 2 violins

scaleSeq :: Scale Interval Pitch -> M
scaleSeq s = pseq ((|/ 2) . fromPitch <$> scaleToList s)

stringNotes :: Pitch -> M
stringNotes p = scaleSeq (scale p (Mode [_P5, _P4, _P5, _P4, _P5]))

strings :: Music
strings =
  info "Strings" (key c MajorMode) (4 / 4) $
    set parts' vln1 (pseq $ stringNotes <$> [g_, d, a, e'])
