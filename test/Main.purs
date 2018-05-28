module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), fromRight, isRight)
import Data.Int (round, toNumber)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (RegexFlags(RegexFlags))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, unV)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Global (readInt)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import TelegramBot (Token, connect, getMe, onMessage, onText, sendMessage)
import Test.Unit (success, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (exit, runTest)

type Config =
  { token :: Token
  , master :: Int
  }

getConfig :: Effect (V (NonEmptyList String) Config)
getConfig = do
  token <- lookup "TEST_TELEGRAM_BOT_TOKEN"
  master <- lookup "TEST_TELEGRAM_MASTER"
  pure $ { token: _, master: _ }
    <$> token
    <*> (round <<< readInt 10 <$> master)
  where
    lookup s = do
      m <- lookupEnv s
      pure $ case m of
        Nothing -> invalid $ pure $ "value not found for " <> s
        Just x -> pure x

main :: Effect Unit
main = launchAff_ $ do
  config <- liftEffect $ getConfig
  unV
    (\e -> error $ "config file is malformed: " <> show e)
    (\x -> do
      void <<< liftEffect $ runTests x
      delay <<< wrap $ toNumber 1000
      liftEffect $ exit 0
    )
    config

runTests :: Config -> Effect Unit
runTests {token, master} = runTest do
  suite "TelegramBot" do
    test "Can receive messages and send them" do
      bot <- liftEffect $ connect token
      let flags = RegexFlags { unicode: true, sticky: false, multiline: false, ignoreCase: true, global: false }
      let pattern = unsafePartial $ fromRight $ regex "^get$" flags
      me <- runExcept <$> getMe bot
      assert "Bot GetMe worked" $ isRight me
      liftEffect $ do
        onText bot pattern handleQueuedMessage
        onMessage bot handleQueuedMessage'
        sendMessage bot master "HELLO FROM PURESCRIPT"
      success
  where
    handleQueuedMessage fM fXs = do
      log "#####Queued up 'get' Message received#####"
      case runExcept fM /\ runExcept fXs of
        Right m /\ Right xs -> do
          log "message and matches decoded correctly"
        Left e1 /\ Left e2 -> do
          log "failed decoding:"
          logShow e1
          logShow e2
          error ":("
        _ -> do
          error "something happened with decoding"
    handleQueuedMessage' fM = do
      log "#####Queued up Message received#####"
      case runExcept fM of
        Right m -> do
          log "message and matches decoded correctly"
        Left e1 -> do
          log "failed decoding:"
          logShow e1
          error ":("
