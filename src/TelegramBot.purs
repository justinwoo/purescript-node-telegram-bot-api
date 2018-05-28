module TelegramBot where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (F, Foreign)
import Simple.JSON (read')

--| The Telegram Bot API Token.
type Token = String

--| The Telegram Bot options.
type Options =
  { polling :: Boolean
  }

--| The Telegram Message type. See https://core.telegram.org/bots/api#message
type Message =
  { message_id :: Int
  , from :: Maybe User
  , date :: Int
  , chat :: Chat
  , location :: Maybe Location
  , text :: Maybe String
  }

--| The Telegram User type. See https://core.telegram.org/bots/api#user
type User =
  { id :: Int
  , first_name :: Maybe String
  , last_name :: Maybe String
  , username :: Maybe String
  }

--| The Telegram Chat type. See https://core.telegram.org/bots/api#chat
type Chat =
  { id :: Int
  , type :: String
  , first_name :: Maybe String
  , last_name :: Maybe String
  , username :: Maybe String
  }

--| The Telegram Location type. See https://core.telegram.org/bots/api#location
type Location =
  { longitude :: Number
  , latitude :: Number
  }

--| The Regex execution matches. See https://github.com/yagop/node-telegram-bot-api#TelegramBot+onText
type Matches = Maybe (Array String)

foreign import data Bot :: Type

defaultOptions :: Options
defaultOptions =
  { polling: true
  }

foreign import _connect ::
  Fn2
    Options
    Token
    (Effect Bot)
connect ::
  Token ->
  Effect Bot
connect = runFn2 _connect defaultOptions

foreign import _sendMessage ::
  Fn3
    Bot
    Int
    String
    (Effect Unit)
sendMessage ::
  Bot ->
  Int ->
  String ->
  Effect Unit
sendMessage bot id message = do
  runFn3 _sendMessage bot id message

foreign import _onText ::
  Fn3
    Bot
    Regex
    (Foreign -> Foreign -> Effect Unit)
    (Effect Unit)
--| For adding a callback for on text matching a regex pattern.
onText ::
  Bot ->
  Regex ->
  (F Message -> F Matches -> Effect Unit) ->
  (Effect Unit)
onText bot regex handler = do
  onText' bot regex handleMessage
  where
    handleMessage foreignMsg foreignMatches =
      handler (read' foreignMsg) (read' foreignMatches)

--| For getting the Foreign values directly. The callback is Message -> Matches -> Effect _ Unit.
onText' ::
  Bot ->
  Regex ->
  (Foreign -> Foreign -> Effect Unit) ->
  (Effect Unit)
onText' bot regex handler = do
  runFn3 _onText bot regex handler

foreign import _onMessage ::
  Fn2
    Bot
    (Foreign -> Effect Unit)
    (Effect Unit)

--| For adding a callback for all messages.
onMessage ::
  Bot ->
  (F Message -> Effect Unit) ->
  (Effect Unit)
onMessage bot handler = do
  onMessage' bot $ handler <<< read'

--| For getting the Foreign value directly from onMessage
onMessage' ::
  Bot ->
  (Foreign -> Effect Unit) ->
  (Effect Unit)
onMessage' bot handler = do
  runFn2 _onMessage bot handler

foreign import data Promise :: Type -> Type
foreign import runPromise :: forall a
   . (EffectFn1 Error Unit)
  -> (EffectFn1 a Unit)
  -> Promise a
  -> Effect Unit

foreign import _getMe ::
  Fn1
    Bot
    (Effect (Promise Foreign))
--| For adding a callback for all messages.
getMe ::
  Bot ->
  (Aff (F User))
getMe bot = do
  p <- liftEffect $ runFn1 _getMe bot
  read' <$> makeAff
    (\cb -> pure mempty <* runPromise
      (mkEffectFn1 $ cb <<< Left)
      (mkEffectFn1 $ cb <<< Right)
      p
    )
