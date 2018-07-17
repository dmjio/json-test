{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : JSON.Benchmark
-- Description : Benchmarks of JSON parsing
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module JSON.Benchmark where
--------------------------------------------------------------------------------
import           Criterion.Main
import           Data.Aeson           (eitherDecode, Value)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as B
import           System.Environment   (getArgs)
--------------------------------------------------------------------------------
import           JSON.Lexer           (getTokens)
import           JSON.Parser          (parse)
--------------------------------------------------------------------------------
-- | JSON benchmark project
jsonBenchmarks :: IO ()
jsonBenchmarks = do
  file <- L.readFile "example.json"
  defaultMain [
    bgroup "parsing" [
        bench "json-test" $
          whnf parse file
      , bench "aeson"  $
          whnf aesonDecode file
      ]]
    where
      aesonDecode :: L.ByteString -> Either String Value
      aesonDecode = eitherDecode
