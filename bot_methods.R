# ###########################################################
# bot methods

# start dialog
start <- function(bot, update) {
  
  # 
  
  # Send query
  bot$sendMessage(update$message$chat_id, 
                  text = "Привет, я бот и я умею сжимать фото по алгоритму svd.")
  bot$sendMessage(update$message$chat_id, 
                  text = "/enter_confid - задать процент потери.
/enter_components - задать количество компонент.")
  
  set_state(chat_id = update$message$chat_id, state = 'wait_confid')
}

# get current chat state
state <- function(bot, update) {
  
  chat_state <- get_state(update$message$chat_id)
  
  # Send state
  bot$sendMessage(update$message$chat_id, 
                  text = unlist(chat_state))
  
}


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

# reset dialog state
reset <- function(bot, update) {
  
  set_state(chat_id = update$message$chat_id, state = 'start')
  
}

# enter confid
enter_confid <- function(bot, update) {
  
  bot$sendMessage(update$message$chat_id, 
                  text = "Введи долю потери фото.")
  
  uconfid <- update$message$text

  bot$sendMessage(update$message$chat_id, 
                  text = "Принято!")
  
  #set_chat_data(update$message$chat_id, 'confid', uconfid)
  
  bot$sendMessage(update$message$chat_id, 
                  text = "Теперь отправь фото для сжатия.")
  
  # Меняем состояние на ожидание ввода имени
  #set_state(chat_id = update$message$chat_id, state = 'wait_photo')
  
}


# enter components
enter_components <- function(bot, update) {

    bot$sendMessage(update$message$chat_id, 
                  text = "ВВеди количество компонент сжатия.")
  
  ucomponents <- update$message$text
  
  bot$sendMessage(update$message$chat_id, 
                  text = "Принято!")
  
  #set_chat_data(update$message$chat_id, 'components', ucomponents)
  
  bot$sendMessage(update$message$chat_id, 
                  text = "Теперь отправь фото для сжатия.")
  
  # Меняем состояние на ожидание ввода имени
  #set_state(chat_id = bot$message$chat_id, state = 'wait_photo')
  
}


# enter
enter_photo <- function(bot, update) {
  
  uphoto <- update$getFile$chat_id
  
  bot$sendMessage(update$message$chat_id, 
                  text = "ОК, фото принято в обработку, осталось подождать...")
  
  #usercomponents  <- get_chat_data(bot$message$chat_id, 'components')
  #userconfid  <- get_chat_data(bot$message$chat_id, 'confid')
  
  #result <- compress(uphoto, usercomponents , userconfid)
  
  #bot$sendPhoto(update$message$chat_id,photo)
  

  # возвращаем диалог в исходное состояние
  #set_state(chat_id = update$message$chat_id, state = 'start')

}

