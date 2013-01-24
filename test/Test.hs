module Main (main) where

import Test.Framework

import Stream
import Array

main = defaultMain
        [ Stream.test
        , Array.test
        ]
