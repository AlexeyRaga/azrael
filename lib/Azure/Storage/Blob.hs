{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Azure.Storage.Blob
( Account(..)
, AccountName(..)
, ContainerName(..)
, BlobName(..)
, SAS(..)

, putBlobBS
, putBlobStream
, getBlobBS
, getBlobStream
, deleteBlob
)
where

import           Control.Exception       (throwIO, try)
import           Control.Lens
import           Control.Monad           (mapM, void)
import           Control.Monad.Lazy      (interleaveUnfoldrIO)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Int                (Int64)
import qualified Data.List               as List
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (HttpException (..),
                                          HttpExceptionContent (..), brReadSome,
                                          managerModifyRequest)
import           Network.URI
import qualified Network.Wreq            as Wreq
import qualified Network.Wreq.Types      as Wreq (ResponseChecker)

newtype AccountName = AccountName Text deriving (Show, Eq, Ord, IsString)
newtype ContainerName = ContainerName Text deriving (Show, Eq, Ord, IsString)
newtype BlobName = BlobName Text deriving (Show, Eq, Ord, IsString)
newtype SAS = SAS Text deriving (Show, Eq, Ord, IsString)

data Account = Account
  { accountName :: AccountName
  , accountSAS  :: SAS
  } deriving (Show, Eq, Ord, Generic)

streamBlockSize :: Int64
streamBlockSize = 1024 * 1024 * 10 --10 megabytes chunk

azureOptions :: Wreq.Options
azureOptions =
  Wreq.defaults & Wreq.header "x-ms-version" .~ ["2017-07-29"]
                -- & Wreq.manager . _Left %~ (\s -> s { managerModifyRequest = (\x -> print x >> return x) } )

-- | Uploads a blob to the specified Azure Storage container.

-- The entire blob content will reside in memory at once.
-- If you want constant memory usage, use 'putBlobStream'.
putBlobBS :: Account -> ContainerName -> BlobName -> LBS.ByteString -> IO ()
putBlobBS account container blob payload = do
  let options = azureOptions
  void $ Wreq.putWith options (authorisedBlobUrl account container blob) payload

-- | Streams a blob to the specified Azure Storage container by chunking it into
-- "blocks".
--
-- Reference: https://docs.microsoft.com/en-us/rest/api/storageservices/put-block-list
putBlobStream :: Account -> ContainerName -> BlobName -> LBS.ByteString -> IO ()
putBlobStream account container blob payload = do
  let chunks = chunksOf streamBlockSize payload
  let partIds = fmap blobPartId [1..]
  let partedChunks = zip partIds chunks

  let blobBaseUrl = authorisedBlobUrl account container blob
  let blobTargetUrl = blobBaseUrl <> "&comp=blocklist"

  parts <- mapM (uncurry (uploadPart azureOptions blobBaseUrl)) partedChunks

  let blockList = buildBlockListXml parts

  void $ Wreq.putWith azureOptions blobTargetUrl blockList

  where
    chunksOf n = List.takeWhile (not . LBS.null) . List.unfoldr (Just . LBS.splitAt n)

    uploadPart options blobUrl partId chunk = do
      let partUrl = blobUrl <> "&comp=block&blockId=" <> partId
      void $ Wreq.putWith options partUrl chunk
      pure partId

    buildBlockListXml parts =
      BSBuilder.toLazyByteString $ mconcat
        [ "<?xml version='1.0' encoding='utf-8'?><BlockList>"
        , foldMap (\p -> "<Latest>" <> BSBuilder.string8 p <> "</Latest>") parts
        , "</BlockList>"
        ]

getBlobStream :: Account -> ContainerName -> BlobName -> IO (Maybe LBS.ByteString)
getBlobStream account container blob = do
  let blobUrl = authorisedBlobUrl account container blob
  res <- interleaveUnfoldrIO (\start -> getChunk start streamBlockSize blobUrl) 0
  case res of
    [] -> pure Nothing
    chunks ->
      let builder = mconcat (BSBuilder.lazyByteString <$> chunks)
      in pure $ Just (BSBuilder.toLazyByteString builder)
  where
    getChunk start size blobUrl = do
      let end = start + size
      let range = "bytes=" <> show start <> "-" <> show end
      let options = azureOptions & Wreq.checkResponse ?~ suppressStatusCodes [404, 416]
                                 & Wreq.header "x-ms-range" .~ [C8.pack range]
      resp <- Wreq.getWith options blobUrl

      case resp ^. Wreq.responseStatus . Wreq.statusCode of
        404 -> pure Nothing
        416 -> pure Nothing
        _   -> pure (Just (resp ^. Wreq.responseBody, end + 1))


getBlobBS :: Account -> ContainerName -> BlobName -> IO (Maybe LBS.ByteString)
getBlobBS account container blob = do
  let blobUrl = authorisedBlobUrl account container blob
  let options = azureOptions & Wreq.checkResponse ?~ suppressStatusCodes [404]
  resp <- Wreq.getWith options blobUrl
  if resp ^. Wreq.responseStatus . Wreq.statusCode == 404
    then pure Nothing
    else pure $ Just $ resp ^. Wreq.responseBody

deleteBlob :: Account -> ContainerName -> BlobName -> IO ()
deleteBlob account container blob = do
  let blobUrl = authorisedBlobUrl account container blob
  let options = azureOptions & Wreq.checkResponse ?~ suppressStatusCodes [404]
  void $ Wreq.deleteWith options blobUrl

--------------------------------------------------------------------------------
authorisedBlobUrl :: Account -> ContainerName -> BlobName -> String
authorisedBlobUrl (Account (AccountName a) (SAS sas)) (ContainerName c) (BlobName b) =
  Text.unpack $ mconcat
    [ "https://"
    , a
    , ".blob.core.windows.net/"
    , c
    , "/"
    , b
    , sas
    ]

blobPartId :: Int -> String
blobPartId =
  C8.unpack . B64.encode . pad 5 . C8.pack . show
  where
    pad n s = C8.replicate (n - C8.length s) '0' <> s

suppressStatusCodes :: [Int] -> Wreq.ResponseChecker
suppressStatusCodes expectedErrorCodes req res = do
    let sci = res ^. Wreq.responseStatus . Wreq.statusCode
    if sci >= 200 && sci < 300 || sci `elem` expectedErrorCodes
      then return ()
      else do
        chunk <- brReadSome (res ^. Wreq.responseBody) 1024
        let res' = fmap (const ()) res
        throwIO $ HttpExceptionRequest req $ StatusCodeException res' (LBS.toStrict chunk)
