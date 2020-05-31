{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Azure.Storage.Blob
import           Control.Monad        (void)
import qualified Data.ByteString.Lazy as LBS
import           Options              (Command (..), CopyFile (..),
                                       DeleteFile (..), Options (..), options)
import           Options.Applicative  (execParser)

main :: IO ()
main = do
  opts <- execParser options
  let acc = account opts
  case (cmd opts) of
    Put params    -> putFile acc params
    Get params    -> getFile acc params
    Delete params -> deleteFile acc params


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

deleteFile :: Account
  -> DeleteFile
  -> IO ()
deleteFile account (DeleteFile container blob) =
  void $ deleteBlob account container blob
