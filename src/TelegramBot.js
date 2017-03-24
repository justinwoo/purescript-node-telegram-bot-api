var TelegramBot = require('node-telegram-bot-api');

exports._connect = function (options, token) {
  return function () {
    return new TelegramBot(token, options);
  };
};

exports._sendMessage = function(bot, id, message) {
  return function () {
    bot.sendMessage(id, message);
  };
};

exports._onMessage = function(bot, eff) {
  return function () {
    bot.on('message', function (msg) {
      eff(msg)();
    });
  };
};

exports._onText = function (bot, regex, eff) {
  return function () {
    bot.onText(regex, function (msg, match) {
      eff(msg)(match)();
    });
  };
};

exports._getMe = function (bot) {
  return function () {
    return bot.getMe();
  };
};
