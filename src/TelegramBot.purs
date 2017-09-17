module TelegramBot where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Monoid (mempty)
import Data.String.Regex (Regex)
import Simple.JSON (read)

type TelegramEffects e = (telegram :: TELEGRAM | e)

--| The Telegram Bot API Token.
type Token = String

--| The Telegram Bot options.
type Options =
  { polling :: Boolean
  }

--| The Telegram Message type. See https://core.telegram.org/bots/api#message
type Message =
  { message_id :: Int
  , from :: NullOrUndefined User
  , date :: Int
  , chat :: Chat
  , location :: NullOrUndefined Location
  , text :: NullOrUndefined String
  }

--| The Telegram User type. See https://core.telegram.org/bots/api#user
type User =
  { id :: Int
  , first_name :: NullOrUndefined String
  , last_name :: NullOrUndefined String
  , username :: NullOrUndefined String
  }

--| The Telegram Chat type. See https://core.telegram.org/bots/api#chat
type Chat =
  { id :: Int
  , type :: String
  , first_name :: NullOrUndefined String
  , last_name :: NullOrUndefined String
  , username :: NullOrUndefined String
  }

--| The Telegram Location type. See https://core.telegram.org/bots/api#location
type Location =
  { longitude :: Number
  , latitude :: Number
  }

--| The Regex execution matches. See https://github.com/yagop/node-telegram-bot-api#TelegramBot+onText
type Matches = NullOrUndefined (Array String)

foreign import data TELEGRAM :: Effect
foreign import data Bot :: Type

defaultOptions :: Options
defaultOptions =
  { polling: true
  }

foreign import _connect :: forall e.
  Fn2
    Options
    Token
    (Eff (TelegramEffects e) Bot)
connect :: forall e.
  Token ->
  Eff (TelegramEffects e) Bot
connect = runFn2 _connect defaultOptions

foreign import _sendMessage :: forall e.
  Fn3
    Bot
    Int
    String
    (Eff (TelegramEffects e) Unit)
sendMessage :: forall e.
  Bot ->
  Int ->
  String ->
  Eff (TelegramEffects e) Unit
sendMessage bot id message = do
  runFn3 _sendMessage bot id message

foreign import _onText :: forall e.
  Fn3
    Bot
    Regex
    (Foreign -> Foreign -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)
--| For adding a callback for on text matching a regex pattern.
onText :: forall e.
  Bot ->
  Regex ->
  (F Message -> F Matches -> Eff (TelegramEffects e) Unit) ->
  (Eff (TelegramEffects e) Unit)
onText bot regex handler = do
  onText' bot regex handleMessage
  where
    handleMessage foreignMsg foreignMatches =
      handler (read foreignMsg) (read foreignMatches)

--| For getting the Foreign values directly. The callback is Message -> Matches -> Eff _ Unit.
onText' :: forall e.
  Bot ->
  Regex ->
  (Foreign -> Foreign -> Eff (TelegramEffects e) Unit) ->
  (Eff (TelegramEffects e) Unit)
onText' bot regex handler = do
  runFn3 _onText bot regex handler

foreign import _onMessage :: forall e.
  Fn2
    Bot
    (Foreign -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)

--| For adding a callback for all messages.
onMessage :: forall e.
  Bot ->
  (F Message -> Eff (TelegramEffects e) Unit) ->
  (Eff (TelegramEffects e) Unit)
onMessage bot handler = do
  onMessage' bot $ handler <<< read

--| For getting the Foreign value directly from onMessage
onMessage' :: forall e.
  Bot ->
  (Foreign -> Eff (TelegramEffects e) Unit) ->
  (Eff (TelegramEffects e) Unit)
onMessage' bot handler = do
  runFn2 _onMessage bot handler

foreign import data Promise :: Type -> Type
foreign import runPromise :: forall e a
   . (EffFn1 e Error Unit)
  -> (EffFn1 e a Unit)
  -> Promise a
  -> Eff e Unit

foreign import _getMe :: forall e.
  Fn1
    Bot
    (Eff (TelegramEffects e) (Promise Foreign))
--| For adding a callback for all messages.
getMe :: forall e.
  Bot ->
  (Aff (TelegramEffects e) (F User))
getMe bot = do
  p <- liftEff $ runFn1 _getMe bot
  read <$> makeAff
    (\cb -> pure mempty <* runPromise
      (mkEffFn1 $ cb <<< Left)
      (mkEffFn1 $ cb <<< Right)
      p
    )
