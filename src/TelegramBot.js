var TelegramBot = require('node-telegram-bot-api');

exports._connect = function (options, token) {
  return function () {
    return new TelegramBot(token, options);
  };
}

exports._sendMessage = function(bot, id, message) {
  return function () {
    bot.sendMessage(id, message);
  };
}

exports._addMessagesListener = function (bot, regex, eff) {
  return function () {
    bot.onText(regex, function (msg, match) {
      eff(msg)(match)();
    });
  };
}
