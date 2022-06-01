library(telegram.bot)
library(tidyverse)
library(RSQLite)
library(DBI)
library(configr)

# переходим в папку проекта
setwd(Sys.getenv('TG_BOT_PATH'))

# читаем конфиг
cfg <- read.config('config.cfg')

# создаём экземпляр бота
updater <- Updater(cfg$bot_settings$bot_token)

# Загрузка компонентов бота
source(file = "c:\\Users\\Fleetlord\\Documents\\CompressionJPEG\\db_bot_functions.R", encoding = "UTF-8") # функции для работы с БД
source(file = "c:\\Users\\Fleetlord\\Documents\\CompressionJPEG\\bot_methods.R", encoding = "UTF-8")  # методы бота
source(file = "c:\\Users\\Fleetlord\\Documents\\CompressionJPEG\\messagefilter.R", encoding = "UTF-8") # фильтры сообщений
source(file = "c:\\Users\\Fleetlord\\Documents\\CompressionJPEG\\handlers.R", encoding = "UTF-8") # обработчики сообщений


# Добавляем обработчики в диспетчер
updater <- updater +
  start_h +
  wait_photo_h +
  state_h +
  reset_h +
  wait_confid_h

# Запускаем бота
updater$start_polling()
