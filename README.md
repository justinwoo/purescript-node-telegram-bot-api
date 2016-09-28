# purescript-node-telegram-bot-api

This is a very small subset of the [node-telegram-bot-api](https://github.com/yagop/node-telegram-bot-api) API.

## Development

To run the tests, provide a `config.json` with this structure:

```js
{
  "token": "XXXXXXXXXXXXXXXXXXXXXX", // The telegram bot API token
  "master": XXXXXXXXX // Your user ID number
}
```

## Usage

See the [tests](test/Main.purs) and the [docs](docs/TelegramBot.md)