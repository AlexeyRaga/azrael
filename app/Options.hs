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

data CopyFile = CopyFile
  { container :: ContainerName
  , azureBlob :: BlobName
  , localFile :: FilePath
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
    (  command "get" (info (Get <$> copyOptionsParser) (progDesc "Get file from Azure Storage"))
    <> command "put" (info (Put <$> copyOptionsParser) (progDesc "Put file to Azure Storage"))
    )

copyOptionsParser :: Parser CopyFile
copyOptionsParser = CopyFile
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
