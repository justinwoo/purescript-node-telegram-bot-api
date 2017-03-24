module TelegramBot where

import Prelude
import Data.Foreign.Generic as DFG
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise, toAff)
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (readGeneric)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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
  , from :: NullOrUndefined User
  , date :: Int
  , chat :: Chat
  , location :: NullOrUndefined Location
  , text :: String
  }

--| The Telegram User type. See https://core.telegram.org/bots/api#user
newtype User = User
  { id :: Int
  , first_name :: NullOrUndefined String
  , last_name :: NullOrUndefined String
  , username :: NullOrUndefined String
  }

--| The Telegram Chat type. See https://core.telegram.org/bots/api#chat
newtype Chat = Chat
  { id :: Int
  , type :: String
  , first_name :: NullOrUndefined String
  , last_name :: NullOrUndefined String
  , username :: NullOrUndefined String
  }

--| The Telegram Location type. See https://core.telegram.org/bots/api#location
newtype Location = Location
  { longitude :: Number
  , latitude :: Number
  }

--| The Regex execution matches. See https://github.com/yagop/node-telegram-bot-api#TelegramBot+onText
newtype Matches = Matches (NullOrUndefined (Array String))

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance isForeignMessage :: IsForeign Message where
  read = readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

derive instance genericUser :: Generic User _
instance showUser :: Show User where
  show = genericShow
instance isForeignUser :: IsForeign User where
  read = readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

derive instance genericChat :: Generic Chat _
instance showChat :: Show Chat where
  show = genericShow
instance isForeignChat :: IsForeign Chat where
  read = readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

derive instance genericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance isForeignLocation :: IsForeign Location where
  read = readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

derive instance genericMatches :: Generic Matches _
instance showMatches :: Show Matches where
  show = genericShow
instance isForeignMatches :: IsForeign Matches where
  read = readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

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
  read <$> toAff p
