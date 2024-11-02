SEED <- 250

train_model <- function(data, class_tag) {
  data <- data %>% filter(class == class_tag)

  if (nrow(data) < 2) {
    stop("No hay suficientes datos para la clase proporcionada.")
  }

  X <- data %>% select(-price, -class)
  y <- data$price

  set.seed(SEED)

  train_index <- createDataPartition(y, p = 0.85, list = FALSE)
  x_train <- X[train_index, ]
  x_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]

  # Estandarizar
  scaler <- preProcess(x_train, method = c("center", "scale"))
  x_train_scaled <- predict(scaler, x_train)
  x_test_scaled <- predict(scaler, x_test)

  # Linear regression
  lm_model <- lm(y_train ~ .,
                 data = as.data.frame(cbind(y_train, x_train_scaled)))
  test_pred_lm <- predict(lm_model, newdata = as.data.frame(x_test_scaled))

  # Random forest
  rf_model <- randomForest(x = x_train_scaled,
                           y = y_train,
                           ntree = 100,
                           importance = TRUE)
  test_pred_rf <- predict(rf_model, newdata = as.data.frame(x_test_scaled))

  # Decission tree
  dt_model <- rpart(y_train ~ .,
                    data = as.data.frame(cbind(y_train, x_train_scaled)))
  test_pred_dt <- predict(dt_model, newdata = as.data.frame(x_test_scaled))

  return(list(
    lm_model = lm_model,
    lm_pred = test_pred_lm,
    lm_train_score = round(summary(lm_model)$r.squared, 3),
    rf_model = rf_model,
    rf_pred = test_pred_rf,
    rf_train_score = round(rf_model$rsq[2], 3),
    dt_model = dt_model,
    dt_pred = test_pred_dt,
    dt_train_score = round(cor(y_train, predict(dt_model, newdata = as.data.frame(x_train_scaled)))^2, 3),
    test_val = y_test
  ))
}