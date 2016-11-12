module TelegramBot where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Null (unNull)
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
newtype Message = Message
  { message_id :: Int
  , from :: Maybe User
  , date :: Int
  , chat :: Chat
  }

--| The Telegram User type. See https://core.telegram.org/bots/api#user
newtype User = User
  { id :: Int
  , first_name :: String
  }

--| The Telegram Chat type. See https://core.telegram.org/bots/api#chat
newtype Chat = Chat
  { id :: Int
  , type :: String
  }

--| The Regex execution matches. See https://github.com/yagop/node-telegram-bot-api#TelegramBot+onText
newtype Matches = Matches (Maybe (Array String))

instance isForeignMessage :: IsForeign Message where
  read json = do
    message_id <- readProp "message_id" json
    from <- unNull <$> readProp "from" json
    date <- readProp "date" json
    chat <- readProp "chat" json
    pure $ Message {message_id, from, date, chat}

instance isForeignUser :: IsForeign User where
  read json = do
    id <- readProp "id" json
    first_name <- readProp "first_name" json
    pure $ User {id, first_name}

instance isForeignChat :: IsForeign Chat where
  read json = do
    id <- readProp "id" json
    t <- readProp "type" json
    pure $ Chat {id, type: t}

instance isForeignMatches :: IsForeign Matches where
  read json = do
    maybeXs <- unNull <$> read json
    pure $ Matches maybeXs

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
  runFn3 _onText bot regex handleMessage
  where
    handleMessage foreignMsg foreignMatches =
      handler (read foreignMsg) (read foreignMatches)

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
  runFn2 _onMessage bot handleMessage
  where
    handleMessage foreignMsg =
      handler (read foreignMsg)
