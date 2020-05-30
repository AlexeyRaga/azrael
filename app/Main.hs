{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Azure.Storage.Blob
import qualified Data.ByteString.Lazy as LBS
import           Options              (Command (..), CopyFile (..),
                                       Options (..), options)
import           Options.Applicative

main :: IO ()
main = do
  opts <- execParser options
  case (cmd opts) of
    Put copy -> putFile (account opts) copy
    Get copy -> getFile (account opts) copy

putFile :: Account
  -> CopyFile
  -> IO ()
putFile account (CopyFile container blob path) = do
  lbs <- LBS.readFile path
  putBlobStream account container blob lbs

getFile :: Account
  -> CopyFile
  -> IO ()
getFile account (CopyFile container blob path) = do
  lbs <- getBlobBS account container blob
  maybe (error "Blob not found") (LBS.writeFile path) lbs

