preprocess <- function(data) {
  data_filtered <- data %>% filter(class %in% c(0, 1))

  data_filtered$class <- as.factor(data_filtered$class)

  data_model <- data_filtered %>% select(stops, duration, days_left, price, class)

  return(data_model)
}