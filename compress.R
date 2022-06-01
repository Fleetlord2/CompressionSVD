library(jpeg)
####### read photo #######
photo <- readJPEG("Doge.jpg")


class(photo)
dim(photo)

photo <- as.integer(photo)



photo_svd <- base::La.svd(photo[,,1])

class(photo_svd)

dim(photo_svd$u)

length(photo_svd$d)

compress <- function(image, n_components = 100, confid = NULL) {
  photo_copy <- image
  sum_full = 0
  sum_comp = 0
  if (is.null(confid)) {
    for (channel in 1:3) {
      photo_svd <- base::La.svd(image[, , channel])
      photo_copy[ , , channel] <- photo_svd$u[,1:n_components] %*% 
        diag(photo_svd$d[1:n_components]) %*% 
        photo_svd$vt[1:n_components,]
      sum_full = sum_full + sum(photo_svd$d)
      sum_comp = sum_comp + sum(photo_svd$d[1:n_components])
    }  
    return(list("compressed_image" = photo_copy, "confid" = sum_comp / sum_full))
  } else {
      photo_svd <- base::La.svd(image[, , 1])
      for (k in 1:length(diag(photo_svd$d))) {
        if (sum(photo_svd$d[1:k])/sum(photo_svd$d)>=confid) {
          comp <- k
          break
        }
      }
      for (channel in 1:3) {
        photo_svd <- base::La.svd(image[, , channel])
        photo_copy[ , , channel] <- photo_svd$u[,1:comp] %*% 
          diag(photo_svd$d[1:comp]) %*% 
          photo_svd$vt[1:comp,]
      }
      return(list("compressed_image" = photo_copy, "n_components" = comp))
  }
}
  
photo_Doge <- readJPEG(source = "Doge.jpg")
result <- compress(photo_Doge, NULL , 0.6)
writeJPEG(image = result$compressed_image, target = "result_Doge.jpg")
1- result$confid


photo_4k <- readJPEG(source = "4kphoto.jpg")

result <- compress(photo_4k, 200)
writeJPEG(image = result$compressed_image, target = "result_4k.jpg")


photo_shrek <- readJPEG(source = "1.jpeg")

result <- compress(photo_shrek, 150)
writeJPEG(image = result$compressed_image, target = "result_shrek.jpg")

length(photo_svd$d)
plot(2:620,photo_svd$d[2:620])



install.packages("telegram.bot")
library(telegram.bot)  

# создаём экземпляр бота
bot <- Bot(token ="5254481322:AAEXPNx5quQQMIlE2IWg6EN4BiXazxcblTw")

# Запрашиваем информацию о боте
print(bot$getMe())

# Получаем обновления бота, т.е. список отправленных ему сообщений
updates <- bot$getUpdates()
View(updates)

# Запрашиваем идентификатор чата
# Примечание: перед запросом обновлений вы должны отправить боту сообщение
chat_id <- updates[[1]][["message"]][["chat_id"]]


path.expand("~")
file.edit(path.expand(file.path("~", ".Renviron")))

updates[[1L]]$message$from

bot$sendMessage(chat_id,
                text = "Привет, *жирный* _уебан_",
                parse_mode = "Markdown"
)

# Имя пользователя с которым надо поздароваться
user_name <- update$message$from$first_name

# Отправка приветственного сообщения
bot$sendMessage(update$message$chat_id, 
                text = paste0("Моё почтение, ", user_name, "!"), 
                parse_mode = "Markdown")

}
updater <- Updater('5254481322:AAEXPNx5quQQMIlE2IWg6EN4BiXazxcblTw')


# Пишем метод для приветсвия
say_hello <- function(bot, update) {
  
  # Имя пользователя с которым надо поздароваться
  user_name <- update$message$from$first_name
  
  # Отправка приветственного сообщения
  bot$sendMessage(update$message$chat_id, 
                  text = paste0("Моё почтение, ", user_name, "!"), 
                  parse_mode = "Markdown")

bot$sendAnimation(update$message$chat_id,
                  animation = "https://c.tenor.com/DBnP_zU4cLIAAAAd/%D1%82%D0%BE%D0%BC%D0%B0%D1%81-%D1%88%D0%B5%D0%BB%D0%B1%D0%B8.gif"
)

bot$sendMessage(update$message$chat_id, 
                text = "/confid - задать процент потери.
/components - задать количество компонент.
/compress - задать фотку для сжатия и ожидать результата.")

}
# создаём обработчик 
hi_hendler <- CommandHandler('hi', say_hello)

# добаляем обработчик в диспетчер
updater <- updater + hi_hendler

# запускаем бота
updater$start_polling()



# создаём методы
## метод для запуска клавиатуры
start <- function(bot, update) {
  
  # создаём клавиатуру
  RKM <- ReplyKeyboardMarkup(
    keyboard = list(
      list(KeyboardButton("Чат ID")),
      list(KeyboardButton("Моё имя")),
      list(KeyboardButton("Мой логин"))
    ),
    resize_keyboard = FALSE,
    one_time_keyboard = TRUE
  )
  
  # отправляем клавиатуру
  bot$sendMessage(update$message$chat_id,
                  text = 'Выберите команду', 
                  reply_markup = RKM)
  
  bot$getMe()
  
}

## метод возвразающий id чата
chat_id <- function(bot, update) {
  
  bot$sendMessage(update$message$chat_id, 
                  text = paste0("Чат id этого диалога: ", update$message$chat_id),
                  parse_mode = "Markdown")
  
}

## метод возвращающий имя
my_name <- function(bot, update) {
  
  bot$sendMessage(update$message$chat_id, 
                  text = paste0("Вас зовут ", update$message$from$first_name),
                  parse_mode = "Markdown")
  
}

## метод возвращающий логин
my_username <- function(bot, update) {
  
  bot$sendMessage(update$message$chat_id, 
                  text = paste0("Ваш логин ", update$message$from$username),
                  parse_mode = "Markdown")
  
}

# создаём фильтры
## сообщения с текстом Чат ID
MessageFilters$chat_id <- BaseFilter(function(message) {
  
  # проверяем текст сообщения
  message$text == "Чат ID"
  
}
)

## сообщения с текстом Моё имя
MessageFilters$name <- BaseFilter(function(message) {
  
  # проверяем текст сообщения
  message$text == "Моё имя"
  
}
)

## сообщения с текстом Мой логин
MessageFilters$username <- BaseFilter(function(message) {
  
  # проверяем текст сообщения
  message$text == "Мой логин"
)

# создаём обработчики
h_start    <- CommandHandler('start', start)
h_chat_id  <- MessageHandler(chat_id, filters = MessageFilters$chat_id)
h_name     <- MessageHandler(my_name, filters = MessageFilters$name)
h_username <- MessageHandler(my_username, filters = MessageFilters$username)

# добавляем обработчики в диспетчер
updater <- updater + 
  h_start +
  h_chat_id +
  h_name +
  h_username

# запускаем бота 
updater$start_polling()



# метод для отправки InLine клавиатуры
test <- function(bot, update) {
  
  # создаём InLine клавиатуру
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton("Да", callback_data = 'yes'),
        InlineKeyboardButton("Нет", callback_data = 'no')
      )
    )
  )
  
  # Отправляем клавиатуру в чат
  bot$sendMessage(update$message$chat_id, 
                  text = "Вы болете коронавирусом?", 
                  reply_markup = IKM)
}

# метод для обработки нажатия кнопки
answer_cb <- function(bot, update) {
  
  # полученные данные с кнопки
  data <- update$callback_query$data
  
  # получаем имя пользователя, нажавшего кнопку
  uname <- update$effective_user()$first_name
  
  # обработка результата
  if ( data == 'no' ) {
    
    msg <- paste0(uname, ", поздравляю, ваш тест на covid-19 отрицательный.")
    
  } else {
    
    msg <- paste0(uname, ", к сожалени ваш тест на covid-19 положительный.")
    
  }
  
  # Отправка сообщения
  bot$sendMessage(chat_id = update$from_chat_id(),
                  text = msg)
  
  # сообщаем боту, что запрос с кнопки принят
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}

# создаём обработчики
inline_h <- CommandHandler('test', test)
query_handler <- CallbackQueryHandler(answer_cb)

# добавляем обработчики в диспетчер
updater <- updater + inline_h + query_handler

# запускаем бота
updater$start_polling()


library(telegram.bot)
library(httr)
library(stringr)

updater <- Updater('5254481322:AAEXPNx5quQQMIlE2IWg6EN4BiXazxcblTw')

# создаём методы
## метод для запуска основной клавиатуры
start <- function(bot, update) {
  
  # создаём клавиатуру
  RKM <- ReplyKeyboardMarkup(
    keyboard = list(
      list(
        KeyboardButton("Погода")
      )
    ),
    resize_keyboard = TRUE,
    one_time_keyboard = TRUE
  )
  
  # отправляем клавиатуру
  bot$sendMessage(update$message$chat_id,
                  text = 'Выберите команду', 
                  reply_markup = RKM)
  
}

## Метод вызова Inine клавиатуры
weather <- function(bot, update) {
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Москва', callback_data = 'Moscow'),
        InlineKeyboardButton(text = 'Санкт-Петербург', callback_data = 'Saint Petersburg'),
        InlineKeyboardButton(text = 'Нью-Йорк', callback_data = 'New York')
      ),
      list(
        InlineKeyboardButton(text = 'Екатеринбург', callback_data = 'Yekaterinburg,ru'),
        InlineKeyboardButton(text = 'Берлин', callback_data = 'Berlin,de'),
        InlineKeyboardButton(text = 'Париж', callback_data = 'Paris,fr')
      ),
      list(
        InlineKeyboardButton(text = 'Рим', callback_data = 'Rome,it'),
        InlineKeyboardButton(text = 'Одесса', callback_data = 'Odessa,ua'),
        InlineKeyboardButton(text = 'Киев', callback_data = 'Kyiv,ua')
      ),
      list(
        InlineKeyboardButton(text = 'Токио', callback_data = 'Tokyo'),
        InlineKeyboardButton(text = 'Амстердам', callback_data = 'Amsterdam,nl'),
        InlineKeyboardButton(text = 'Вашингтон', callback_data = 'Washington,us')
      )
    )
  )
  
  # Send Inline Keyboard
  bot$sendMessage(chat_id = update$message$chat_id, 
                  text = "Выберите город", 
                  reply_markup = IKM)
}

# метод для сообщения погоды
answer_cb <- function(bot, update) {
  
  # получаем из сообщения город
  city <- update$callback_query$data
  
  # отправляем запрос
  ans <- GET('https://api.openweathermap.org/data/2.5/weather', 
             query = list(q     = city,
                          lang  = 'ru',
                          units = 'metric',
                          appid = '4776568ccea136ffe4cda9f1969af340')) 
  
  # парсим ответ
  result <- content(ans)
  
  # формируем сообщение
  msg <- str_glue("{result$name} погода:\n",
                  "Текущая температура: {result$main$temp}\n",
                  "Скорость ветра: {result$wind$speed}\n",
                  "Описание: {result$weather[[1]]$description}")
  
  # отправляем информацию о погоде
  bot$sendMessage(chat_id = update$from_chat_id(),
                  text    = msg)
  
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}

# создаём фильтры
## сообщения с текстом Погода
MessageFilters$weather <- BaseFilter(function(message) {
  
  # проверяем текст сообщения
  message$text == "Погода"
  
}
)

# создаём обработчики
h_start         <- CommandHandler('start', start)
h_weather       <- MessageHandler(weather, filters = MessageFilters$weather)
h_query_handler <- CallbackQueryHandler(answer_cb)

# добавляем обработчики в диспетчер
updater <- updater + 
  h_start +
  h_weather +
  h_query_handler

# запускаем бота
updater$start_polling()


# ###########################################################
# bot methods

# start dialog
start <- function(bot, update) {
  
  # 
  
  # Send query
  bot$sendMessage(update$message$chat_id, 
                  text = "Введи своё имя")
  
  # переключаем состояние диалога в режим ожидания ввода имени
  set_state(chat_id = update$message$chat_id, state = 'wait_name')
  
}

# get current chat state
state <- function(bot, update) {
  
  chat_state <- get_state(update$message$chat_id)
  
  # Send state
  bot$sendMessage(update$message$chat_id, 
                  text = unlist(chat_state))
  
}

# reset dialog state
reset <- function(bot, update) {
  
  set_state(chat_id = update$message$chat_id, state = 'start')
  
}

# enter username
enter_name <- function(bot, update) {
  
  uname <- update$message$text
  
  # Send message with name
  bot$sendMessage(update$message$chat_id, 
                  text = paste0(uname, ", приятно познакомится, я бот!"))
  
  # Записываем имя в глобальную переменную
  #username <<- uname
  set_chat_data(update$message$chat_id, 'name', uname) 
  
  # Справшиваем возраст
  bot$sendMessage(update$message$chat_id, 
                  text = "Сколько тебе лет?")
  
  # Меняем состояние на ожидание ввода имени
  set_state(chat_id = update$message$chat_id, state = 'wait_age')
  
}

# enter user age
enter_age <- function(bot, update) {
  
  uage <- as.numeric(update$message$text)
  
  # проверяем было введено число или нет
  if ( is.na(uage) ) {
    
    # если введено не число то переспрашиваем возраст
    bot$sendMessage(update$message$chat_id, 
                    text = "Ты ввёл некорректные данные, введи число")
    
  } else {
    
    # если введено число сообщаем что возраст принят
    bot$sendMessage(update$message$chat_id, 
                    text = "ОК, возраст принят")
    
    # записываем глобальную переменную с возрастом
    #userage <<- uage
    set_chat_data(update$message$chat_id, 'age', uage) 
    
    # сообщаем какие данные были собраны
    username <- get_chat_data(update$message$chat_id, 'name')
    userage  <- get_chat_data(update$message$chat_id, 'age')
    
    bot$sendMessage(update$message$chat_id, 
                    text = paste0("Тебя зовут ", username, " и тебе ", userage, " лет. Будем знакомы"))
    
    # возвращаем диалог в исходное состояние
    set_state(chat_id = update$message$chat_id, state = 'start')
  }
  
}

# Скрипт создания базы данных
library(DBI)     # интерфейс для работы с СУБД
library(configr) # чтение конфига
library(readr)   # чтение текстовых SQL файлов
library(RSQLite) # драйвер для подключения к SQLite

# директория проекта
setwd(Sys.getenv('TG_BOT_PATH'))

# чтение конфига
cfg <- read.config('config.cfg')

# подключение к SQLite
con <- dbConnect(SQLite(), cfg$db_settings$db_path)

# Создание таблиц в базе
dbExecute(con, statement = read_file('create_db_data.sql'))
dbExecute(con, statement = read_file('create_db_state.sql'))











