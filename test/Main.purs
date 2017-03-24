module Test.Main where

import Prelude
import Control.Monad.Eff.Console as EffC
import Control.Monad.Aff (Canceler, later', launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), fromRight, isRight)
import Data.Int (round)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Regex (regex)
import Data.String.Regex.Flags (RegexFlags(RegexFlags))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, unV)
import Global (readInt)
import Node.Process (PROCESS, lookupEnv)
import Partial.Unsafe (unsafePartial)
import TelegramBot (TELEGRAM, Token, connect, getMe, onMessage, onText, sendMessage)
import Test.Unit (success, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (exit, runTest)

type Config =
  { token :: Token
  , master :: Int
  }

getConfig :: forall e. Eff (process :: PROCESS | e) (V (NonEmptyList String) Config)
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

main :: forall e.
  Eff
    ( err :: EXCEPTION
    , console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , telegram :: TELEGRAM
    , process :: PROCESS
    | e
    )
    (Canceler
       ( console :: CONSOLE
       , testOutput :: TESTOUTPUT
       , avar :: AVAR
       , telegram :: TELEGRAM
       , process :: PROCESS
       | e
       )
    )
main = launchAff $ do
  config <- liftEff $ getConfig
  unV
    (\e -> error $ "config file is malformed: " <> show e)
    (\x -> do
      void <<< liftEff $ runTests x
      later' 1000 $ liftEff $ exit 0
    )
    config

runTests :: forall e.
  Config
  -> Eff
       ( console :: CONSOLE
       , testOutput :: TESTOUTPUT
       , avar :: AVAR
       , telegram :: TELEGRAM
       | e
       )
       Unit
runTests {token, master} = runTest do
  suite "TelegramBot" do
    test "Can receive messages and send them" do
      bot <- liftEff $ connect token
      let flags = RegexFlags { unicode: true, sticky: false, multiline: false, ignoreCase: true, global: false }
      let pattern = unsafePartial $ fromRight $ regex "^get$" flags
      me <- runExcept <$> getMe bot
      assert "Bot GetMe worked" $ isRight me
      liftEff $ do
        logShow me
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
          EffC.error ":("
        _ -> do
          EffC.error "something happened with decoding"
    handleQueuedMessage' fM = do
      log "#####Queued up Message received#####"
      case runExcept fM of
        Right m -> do
          log "message and matches decoded correctly"
        Left e1 -> do
          log "failed decoding:"
          logShow e1
          EffC.error ":("
