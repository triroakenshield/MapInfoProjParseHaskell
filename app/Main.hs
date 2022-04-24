module Main where

import Control.Applicative
import qualified Data.Text as T

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

import Description
import CoordinateReferenceSystemDescription

type Item = T.Text

data Model = Model
  { current :: T.Text
  }

initialModel :: Model
initialModel = Model { current = "test"  }

data Action
  =  Start |
  MiToProj T.Text
  deriving (Show, Read)

testBot1 :: BotApp Model Action
testBot1 = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $ 
          Start    <$  command "start"
      <|> MiToProj <$> command "miToProj"
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of 
        Start -> model <# do 
            reply (toReplyMessage startMessage)    
        MiToProj item -> model <# do 
            reply (toReplyMessage (parse1 item))

    startMessage = T.unlines [ "test1 miToProj" ]
  
parse1 item = T.pack (CoordinateReferenceSystemDescription.toProj (getCRS2 item))

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId testBot1) env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . T.pack <$> getLine
  run token