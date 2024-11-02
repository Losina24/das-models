# Cargar las librerias
source("load_libraries.R")
source("preprocessing.R")
source("model_training.R")
source("test_model.R")

DATASETS_PATH <- "../datasets/"
FLIGHTS_DATASET_PATH <- file.path(DATASETS_PATH, "flights.csv")
ECONOMIC <- 0
BUSINESS <- 1

install_and_load(c("randomForest", "dplyr", "caret", "rpart"))

# 1. Cargar los datos
data <- readr::read_csv(FLIGHTS_DATASET_PATH)

# 2. Preprocessing
data_model <- preprocess(data)

# 3. Entrenar los modelos
business_results <- train_model(data_model, BUSINESS)
economic_results <- train_model(data_model, ECONOMIC)

# 4. Resultados
test_results(business_results, "BUSINESS")
test_results(economic_results, "ECONOMIC")
