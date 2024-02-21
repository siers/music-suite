{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExploratoryRhythms (rhythms) where

import Data.Functor.Foldable (Base, hylo)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.List (partition)
import Data.Ratio
import Music.Prelude
import System.Random

info :: (HasMeta c, HasPosition c, Transformable c) => Title -> KeySignature -> TimeSignature -> c -> c
info title' key sig =
  id
    . title title'
    . composer "-"
    . keySignature key
    . tempo (metronome (1 / 4) 69)
    . timeSignature sig

data Bin a = BinL | BinB a (Bin a) (Bin a)

makeBaseFunctor ''Bin

treeFromList :: [Integer] -> Base (Bin Integer) [Integer]
treeFromList [] = BinLF
treeFromList (n : ns) = BinBF n l r
 where
  (l, r) = partition (> n) ns

divisions :: Base (Bin Integer) (Integer -> [Integer]) -> Integer -> [Integer]
divisions BinLF d = [1 * d]
divisions (BinBF _ l r) d' = let d = (d' * 2) in l d ++ r d

rhythms :: IO Music
rhythms = do
  let measures = 100
  let biggest = 4 -- = 1/biggest
  -- let smallest = 32 -- = 1/smallest
  let beat = randomRIO (0, 3) -- second number is log2(smallest) - log2(biggest)
  subdivisons <- replicateM (measures * biggest) (beat >>= flip replicateM randomIO) :: IO [[Integer]]
  let rhythms' = subdivisons >>= (\r -> hylo divisions treeFromList r biggest)
  rhythms <- traverse (\n -> (,n) <$> randomIO :: IO (Integer, Integer)) rhythms'

  let intToPitch n = if mod n 8 == 0 then rest else fromPitch b
  let toNotes (r, n) = intToPitch r |/ fromRational (n % 1)
  pure . info "Rhythm exercises" (key c MajorMode) (4 / 4) $ pseq (toNotes <$> rhythms)
