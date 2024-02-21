{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExploratorySolfeggio (solfeggio) where

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
type ListScale = [Pitch]

modeIntervals :: Lens' (Mode v p) (NonEmpty v)
modeIntervals f (Mode is) = Mode <$> f is

scaleSeq :: Scale Interval Pitch -> M
scaleSeq s = pseq ((|/ 2) . fromPitch <$> scaleToList s)

mseq :: (Monoid c, HasPosition c, Transformable c, IsPitch c) => [Pitch] -> c
mseq = pseq . fmap ((|/ 2) . fromPitch)

scaleUpDown' :: Pitch -> Mode Interval Pitch -> Scale Interval Pitch
scaleUpDown' p scale' = scale p (over modeIntervals mapScale scale')
 where
  mapScale s = s <> N.reverse (N.map negate s)

diatonicJumps :: Pitch -> DiatonicSteps -> ListScale -> ListScale
diatonicJumps p steps scale = scale >>= (\a -> [a, upDiatonic p steps a])

solfeggio :: IO Music
solfeggio =
  pure $
    info "Scales" (key pitch MajorMode) (4 / 4) . compress 4 $
      mseq $
        [1, 4, 2, 5, 3, 6] >>= \n ->
          diatonicJumps pitch n scale
 where
  pitch = b
  scale = scaleToList $ scaleUpDown' pitch mode
  mode = majorScale
