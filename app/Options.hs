{-# LANGUAGE DuplicateRecordFields #-}
module Options
where

import           Azure.Storage.Blob
import           Options.Applicative

data Options = Options
  { account :: Account
  , cmd     :: Command
  }

data Command
  = Put CopyFile
  | Get CopyFile
  | Delete DeleteFile

data CopyFile = CopyFile
  { container :: ContainerName
  , blobName  :: BlobName
  , localFile :: FilePath
  }

data DeleteFile = DeleteFile
  { container :: ContainerName
  , blobName  :: BlobName
  }

options :: ParserInfo Options
options = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Application to test the library"
  <> header "Test application for Azrail library"
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> accountParser
  <*> commandParser

commandParser :: Parser Command
commandParser =
  subparser
    (  command "get" (info (Get <$> copyParser) (progDesc "Get file from Azure Storage"))
    <> command "put" (info (Put <$> copyParser) (progDesc "Put file to Azure Storage"))
    <> command "delete" (info (Delete <$> deleteParser) (progDesc "Delete file from Azure Storage"))
    )

copyParser :: Parser CopyFile
copyParser = CopyFile
  <$> strOption
        (  long "container-name"
        <> metavar "NAME"
        <> help "Azure Storage container name"
        )
  <*> strOption
        (  long "blob-name"
        <> metavar "PATH"
        <> help "Azure Storage blob name"
        )
  <*> strOption
        (  long "file-path"
        <> metavar "PATH"
        <> help "Local file path"
        )

deleteParser :: Parser DeleteFile
deleteParser = DeleteFile
  <$> strOption
        (  long "container-name"
        <> metavar "NAME"
        <> help "Azure Storage container name"
        )
  <*> strOption
        (  long "blob-name"
        <> metavar "PATH"
        <> help "Azure Storage blob name"
        )

accountParser :: Parser Account
accountParser = Account
  <$> strOption
        (  long "account-name"
        <> metavar "NAME"
        <> help "Azure Storage Account name"
        )
  <*> strOption
        (  long "sas"
        <> metavar "SAS"
        <> help "Azure Storage Account SAS to access the storage with"
        )
