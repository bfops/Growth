module Main (main) where

import Test.Framework

import Array

main = defaultMain
        [ Array.test
        ]
