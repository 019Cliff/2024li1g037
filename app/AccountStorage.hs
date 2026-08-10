{-# LANGUAGE ScopedTypeVariables #-}

module AccountStorage
  ( SaveError (..),
    defaultAccountIndex,
    loadAccountIndex,
    saveAccountIndex,
    loadAccount,
    saveAccount,
    accountSummary,
    accountDirectory,
  )
where

import AccountTypes
import Control.Exception (IOException, try)
import MetaTypes (nivelJogadorMeta)
import System.Directory
import System.FilePath ((</>), takeDirectory)
import Text.Read (readMaybe)

data SaveError
  = SaveIoError String
  | SaveDecodeError FilePath
  | SaveValidationError String
  deriving (Show, Read, Eq)

indexVersion :: Int
indexVersion = 1

defaultAccountIndex :: AccountIndex
defaultAccountIndex = AccountIndex indexVersion [] Nothing True

accountRoot :: IO FilePath
accountRoot = do
  root <- getAppUserDataDirectory "ImmutableTowers"
  createDirectoryIfMissing True (root </> "accounts")
  return root

accountDirectory :: AccountId -> IO FilePath
accountDirectory aid = do
  root <- accountRoot
  let directory = root </> "accounts" </> accountIdTexto aid
  createDirectoryIfMissing True directory
  return directory

loadAccountIndex :: IO (Either SaveError AccountIndex)
loadAccountIndex = do
  root <- accountRoot
  carregarComBackup (root </> "accounts-index-v1.txt")

saveAccountIndex :: AccountIndex -> IO (Either SaveError ())
saveAccountIndex index = do
  root <- accountRoot
  guardarAtomico (root </> "accounts-index-v1.txt") index

loadAccount :: AccountId -> IO (Either SaveError AccountData)
loadAccount aid = do
  directory <- accountDirectory aid
  carregarComBackup (directory </> "profile-v1.txt")

saveAccount :: AccountData -> IO (Either SaveError ())
saveAccount dataAccount = do
  directory <- accountDirectory (accountId dataAccount)
  guardarAtomico (directory </> "profile-v1.txt") dataAccount

accountSummary :: AccountData -> AccountSummary
accountSummary dataAccount =
  AccountSummary
    { accountIdSummary = accountId dataAccount,
      nomeContaSummary = nomeConta dataAccount,
      nivelSummary = nivelJogadorMeta (progressoConta dataAccount),
      ultimoMapaSummary = Nothing
    }

guardarAtomico :: forall a. (Show a, Read a) => FilePath -> a -> IO (Either SaveError ())
guardarAtomico path value = do
  let temporary = path ++ ".tmp"
      backup = path ++ ".bak"
      conteudo = show value
  resultado <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile temporary conteudo
    validacao <- readFile temporary
    case (readMaybe validacao :: Maybe a) of
      Nothing -> ioError (userError "validacao do ficheiro temporario falhou")
      Just _ -> do
        existe <- doesFileExist path
        if existe then copyFile path backup else return ()
        renameFile temporary path
    return ()
  return $ case resultado of
    Left erro -> Left (SaveIoError (show (erro :: IOException)))
    Right _ -> Right ()

carregarComBackup :: Read a => FilePath -> IO (Either SaveError a)
carregarComBackup path = do
  principal <- ler path
  case principal of
    Right value -> return (Right value)
    Left _ -> do
      backup <- ler (path ++ ".bak")
      case backup of
        Right value -> return (Right value)
        Left _ -> return (Left (SaveDecodeError path))
  where
    ler ficheiro = do
      resultado <- try (readFile ficheiro) :: IO (Either IOException String)
      return $ case resultado of
        Left _ -> Left ()
        Right conteudo -> maybe (Left ()) Right (readMaybe conteudo)
