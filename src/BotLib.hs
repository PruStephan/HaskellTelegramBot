{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module BotLib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp                       as W
import           Servant
import           GHC.Generics hiding (from)
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Text (Text)
import qualified Data.Text                                      as T
import           Network.HTTP.Client (newManager, Manager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings, mkManagerSettings)
import qualified Network.Connection                             as  C
import           Data.Maybe
import           Data.Monoid
import           Web.Telegram.API.Bot
import           System.Environment
import qualified Paths_bookshelf_bot                            as P
import           Data.Version (showVersion)
import           DatabaseShelf
import           Types
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.Label


data Version = Version
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

type BotAPI = "version" :> Get '[JSON] Version
         :<|> "webhook"
              :> Capture "bot742743149:AAGX9LwfLcLDuHsvJLEpgoVp0KLyelXscQo" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()

botApi :: Proxy BotAPI
botApi = Proxy


startApp :: IO ()
startApp = do
    putStrLn "Bookshelf bot is starting..."
    env <- getEnvironment
    connSql <- connectSqlite3 "/home/prustephan/bot/bookshelf-bot/Database/sql3/bookshelf.db"
    --initialize connSql
    manager' <- newManager $ mkManagerSettings simpleSettings (Just proxySettings)  
    putStrLn "Manager and Connection are set"
    let realToken = Token $ T.pack $ "bot742743149:AAGX9LwfLcLDuHsvJLEpgoVp0KLyelXscQo"
        config = BotConfig
            { telegramToken = realToken
            , connSql = connSql
            , manager = manager'
            }
    me <- getMe realToken manager'
    case me of
        Left e -> do
            putStrLn "Request failed"
            print e
        Right Response { result = u } -> do
            putStrLn "Request succeded"
    W.run 12001 $ app config 
    where        
        simpleSettings = C.TLSSettingsSimple 
            { C.settingDisableCertificateValidation = True
            , C.settingDisableSession = False  
            , C.settingUseServerName = False
            }
        proxySettings = (C.SockSettingsSimple "49.ip-51-38-129.eu" 1080) 
        

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO,
                 MonadReader BotConfig, MonadError ServantErr)

data BotConfig = BotConfig
  { telegramToken :: Token
  , connSql :: Connection
  , manager :: Manager
  }

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
          transform config = Nat (flip runReaderT config . runBot)

botServer :: ServerT BotAPI Bot
botServer = returnVersion :<|> handleWebhook
    where version' = Version $ T.pack $ showVersion P.version
          returnVersion :: Bot Version
          returnVersion = return version'

handleWebhook :: Text -> Update -> Bot ()
handleWebhook secret update = do
    Token token <- asks telegramToken
    liftIO $ putStrLn "Handling webhook"
    if EQ == compare secret token
        then handleUpdate update
        else throwError err403

handleUpdate :: Update -> Bot ()
handleUpdate update = do
    case update of
        Update { message = Just msg } -> handleTextMessage msg
        _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update

helpMessage :: ChatId -> SendMessageRequest
helpMessage userId = sendMessageRequest userId $ T.unlines
    [ "/help - show this message"
    , "/books - show list of all books"
    , "/find title - find book by title"
    ]


handleTextMessage :: Message -> Bot ()
handleTextMessage msg = do
    BotConfig{..} <- ask
    let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
        messageText = text msg
        resolveCommand conn chatId (Just (T.stripPrefix "/help" -> Just _)) = do
            sendMessageM (helpMessage chatId) >> return ()
        resolveCommand conn chatId (Just (T.stripPrefix "/findByBookTitle " -> Just name)) = do
            let title = T.unpack name
            maybeBook <- liftIO $ getBookByTitle conn title
            case maybeBook of
                Right book -> (sendBookMessage conn chatId book)
                Left message -> (send chatId $ T.pack message)
            return ()
        resolveCommand conn chatId (Just (T.stripPrefix "/findBookByAuthor " -> Just query)) = do
            let title = T.unpack $ (T.takeEnd (length author + 1) query)
                author = T.unpack $ head $ T.splitOn " "  query
            maybeBook <- liftIO $ getBookByAuthor conn author title
            case maybeBook of
                Right book -> (sendBookMessage conn chatId book)
                Left message -> (send chatId $ T.pack message)
            return ()
    liftIO $ runClient (resolveCommand connSql chatId messageText) telegramToken manager
    return ()


sendBookMessage :: IConnection conn => conn -> ChatId -> Book -> TelegramClient MessageResponse
sendBookMessage conn chatId book = do
    let aId = get authorId book
    
    maybeAuthor <- liftIO $ getAuthor conn aId
    case maybeAuthor of
        Left msg -> send chatId $ T.pack msg
        Right author -> do
            let (msg, fp) = convertToRequestFormat book author
            send chatId $ T.pack msg
            sendFile chatId $ T.pack fp

send :: ChatId -> Text -> TelegramClient MessageResponse
send chatId text = sendMessageM (sendMessageRequest chatId text)

sendFile :: ChatId -> Text -> TelegramClient MessageResponse
sendFile chatId pth = uploadDocumentM (uploadDocumentRequest chatId $ localFileUpload (T.unpack pth))




