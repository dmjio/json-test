module Main where

import JSON.Gen (roundTrip)

main :: IO ()
main = roundTrip
