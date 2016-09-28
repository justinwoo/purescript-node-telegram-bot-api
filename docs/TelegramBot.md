## Module TelegramBot

#### `TelegramEffects`

``` purescript
type TelegramEffects e = (telegram :: TELEGRAM | e)
```

#### `Token`

``` purescript
type Token = String
```

The Telegram Bot API Token.

#### `Options`

``` purescript
type Options = { polling :: Boolean }
```

The Telegram Bot options.

#### `Message`

``` purescript
type Message = { message_id :: Int, from :: User, date :: Int, chat :: Chat }
```

The Telegram Message type. See https://core.telegram.org/bots/api#message

#### `User`

``` purescript
type User = { id :: Int, first_name :: String }
```

The Telegram User type. See https://core.telegram.org/bots/api#user

#### `Chat`

``` purescript
type Chat = { id :: Int, type :: String }
```

The Telegram Chat type. See https://core.telegram.org/bots/api#chat

#### `TELEGRAM`

``` purescript
data TELEGRAM :: !
```

#### `Bot`

``` purescript
data Bot :: *
```

#### `defaultOptions`

``` purescript
defaultOptions :: Options
```

#### `_connect`

``` purescript
_connect :: forall e. Fn2 Options Token (Eff (TelegramEffects e) Bot)
```

#### `connect`

``` purescript
connect :: forall e. Token -> Eff (TelegramEffects e) Bot
```

#### `_sendMessage`

``` purescript
_sendMessage :: forall e. Fn3 Bot Int String (Eff (TelegramEffects e) Unit)
```

#### `sendMessage`

``` purescript
sendMessage :: forall e. Bot -> Int -> String -> Eff (TelegramEffects e) Unit
```

#### `_addMessagesListener`

``` purescript
_addMessagesListener :: forall e. Fn3 Bot Regex (Message -> Maybe (Array String) -> Eff (TelegramEffects e) Unit) (Eff (TelegramEffects e) Unit)
```

#### `addMessagesListener`

``` purescript
addMessagesListener :: forall e. Bot -> Regex -> (Message -> Maybe (Array String) -> Eff (TelegramEffects e) Unit) -> (Eff (TelegramEffects e) Unit)
```


