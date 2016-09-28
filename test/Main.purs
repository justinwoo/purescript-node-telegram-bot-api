module Test.Main where

import Prelude
import Control.Monad.Aff (Aff, later', launchAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Either (fromRight, Either(Right, Left))
import Data.Foreign (parseJSON, ForeignError)
import Data.Foreign.Class (readProp)
import Data.String.Regex (regex)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import TelegramBot (Token, sendMessage, addMessagesListener, connect)
import Test.Unit (test, suite)
import Test.Unit.Main (exit, runTest)

type Config =
  { token :: Token
  , master :: Int
  }

parseConfig :: String -> Either ForeignError Config
parseConfig json = do
  value <- parseJSON json
  token <- readProp "token" value
  master <- readProp "master" value
  pure
    { token: token
    , master: master
    }

getConfig :: forall e. Aff (fs :: FS | e) (Either ForeignError Config)
getConfig = parseConfig <$> readTextFile UTF8 "./config.json"

main = launchAff $ do
  config <- getConfig
  case config of
    Left e -> error "config file is malformed."
    Right x -> do
      void $ liftEff $ runTests x

runTests {token, master} = runTest do
  suite "TelegramBot" do
    test "Can receive messages and send them" do
      bot <- liftEff $ connect token
      let flags = { unicode: true, sticky: false, multiline: false, ignoreCase: true, global: false }
      let pattern = unsafePartial $ fromRight $ regex "^get$" flags
      liftEff $ addMessagesListener bot pattern \m xs -> do
        log "#####Queued up 'get' Message received#####"
      liftEff $ sendMessage bot master "HELLO FROM PURESCRIPT"
      later' 1000 $ liftEff $ exit 0