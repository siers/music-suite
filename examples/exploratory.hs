{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

import Data.Ratio
import Control.Lens (Lens', over)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromMaybe)
import Music.Prelude
import Music.Prelude.Standard (ScaleChord (ScaleChord))
import Music.Time.Types
import qualified Music.Score
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Random
import Data.Functor.Foldable.TH (MakeBaseFunctor(makeBaseFunctor))
import Data.Functor.Foldable (Base, Corecursive (ana), Recursive (cata), hylo)
import Data.List (partition)

info title' key sig =
  id
    . title title'
    . composer "-"
    . keySignature key
    . tempo (metronome (1 / 4) 69)
    . timeSignature sig

type M = Score StandardNote

[vln1, vln2] = divide 2 violins

modeIntervals :: Lens' (Mode v p) (NonEmpty v)
modeIntervals f (Mode is) = Mode <$> f is

scaleSeq :: Scale Interval Pitch -> M
scaleSeq s = pseq ((|/ 2) . fromPitch <$> scaleToList s)

genScale :: Pitch -> Mode Interval Pitch -> M
genScale p scale' = scaleSeq (scale p (over modeIntervals mapScale scale')) |> (fromPitch p |* 3)
 where
  mapScale s = (s <> s <> s) <> N.reverse (N.map negate (s <> s <> s))

stringNotes p = scaleSeq (scale p (Mode [_P5, _P4, _P5, _P4, _P5]))

scales :: Music
scales =
  info "Scales" (key g MajorMode) (12 / 8) . compress 4 $
    set parts' vln1 (genScale g_ harmonicMinorScale)

strings :: Music
strings =
  info "Strings" (key c MajorMode) (4 / 4) $
    set parts' vln2 (pseq $ stringNotes <$> [g_, d, a, e'])

--

data Bin a = BinL | BinB a (Bin a) (Bin a)

makeBaseFunctor ''Bin

treeFromList :: [Integer] -> Base (Bin Integer) [Integer]
treeFromList [] = BinLF
treeFromList (n:ns) = BinBF n l r
  where (l, r) = partition (> n) ns

divisions :: Base (Bin Integer) (Integer -> [Integer]) -> Integer -> [Integer]
divisions BinLF d = [1 * d]
divisions (BinBF n l r) d' = let d = (d' * 2) in l d ++ r d

rhythms :: IO Music
rhythms = do
  let measures = 100
  let biggest = 2 -- = 1/biggest
  let smallest = 32 -- = 1/smallest
  let beat = randomRIO (0, 4) -- second number is log2(smallest) - log2(biggest)

  subdivisons <- replicateM (measures * biggest) (beat >>= flip replicateM randomIO) :: IO [[Integer]]
  let rhythms' = subdivisons >>= (\r -> hylo divisions treeFromList r biggest)
  rhythms <- traverse (\n -> (, n) <$> randomIO :: IO (Integer, Integer)) rhythms'

  let intToPitch n = if mod n 8 == 0 then rest else fromPitch b
  let toNotes (r, n) = intToPitch r |/ fromRational (n % 1)
  pure . info "Rhythm exercises" (key c MajorMode) (4 / 4) $ pseq (toNotes <$> rhythms)

--

exercises :: String -> IO Music
exercises =
  fromMaybe (pure scales)
    . flip
      lookup
      [ ("scales", pure scales)
      , ("strings", pure strings)
      , ("rhythms", rhythms)
      ]

main :: IO ()
main = defaultMain =<< exercises . fromMaybe "scale" . lookup "e" =<< getEnvironment
