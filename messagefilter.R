# ###########################################################
# message state filters

# фильтр сообщений в состоянии ожидания имени
MessageFilters$wait_confid <- BaseFilter(function(message) {
  get_state( message$chat_id )  == "wait_confid"
}
)

# фильтр сообщений в состоянии ожидания возраста
MessageFilters$wait_photo <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_photo"
}
)
