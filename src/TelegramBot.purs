module TelegramBot where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (runFn2, Fn2, Fn3, runFn3)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex)

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
  , from :: User
  , date :: Int
  , chat :: Chat
  }

--| The Telegram User type. See https://core.telegram.org/bots/api#user
type User =
  { id :: Int
  , first_name :: String
  }

--| The Telegram Chat type. See https://core.telegram.org/bots/api#chat
type Chat =
  { id :: Int
  , type :: String
  }

foreign import data TELEGRAM :: !
foreign import data Bot :: *

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

foreign import _addMessagesListener :: forall e.
  Fn3
    Bot
    Regex
    (Message -> Maybe (Array String) -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)
addMessagesListener :: forall e.
  Bot ->
  Regex ->
  (Message -> Maybe (Array String) -> Eff (TelegramEffects e) Unit) ->
  (Eff (TelegramEffects e) Unit)
addMessagesListener = runFn3 _addMessagesListener

