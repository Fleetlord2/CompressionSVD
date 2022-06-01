# ###########################################################
# handlers

# command handlers
start_h <- CommandHandler('start', start)
state_h <- CommandHandler('state', state)
reset_h <- CommandHandler('reset', reset)

# message handlers
## !MessageFilters$command - означает что команды данные обработчики не обрабатывают, 
## только текстовые сообщения
wait_photo_h  <- MessageHandler(enter_photo,  MessageFilters$wait_photo  & !MessageFilters$command)
wait_confid_h <- MessageHandler(enter_confid, MessageFilters$wait_data & !MessageFilters$command)