#install.packages("FNN")
#install.packages("caret")

library(FNN)
library(caret)

# loading csv data from github
data_url <- "https://raw.githubusercontent.com/jxxtino/tae-0126-guilherme-luana-matheus/refs/heads/main/data/02-processed/df_clean.txt"

# creating df and removing NA values
df <- read.csv(data_url, sep = ";")
df <- na.omit(df)

# target, features vector and formula
target <- "ndvi_class"
features <- c("ndbi_mean", "lst_mean")
formula <- reformulate(features, response = target)

# creating train/test df 
sample_index <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
df_train <- df[sample_index,]
df_test <- df[-sample_index,]
df_test$ndvi_class <- as.factor(df_test$ndvi_class)

# cross validation method
ctrl <- trainControl(method = "cv", number = 10)

# knn train pipeline
knn_model <- train(
  formula,
  data = df_train,
  method = "knn",
  trControl = ctrl,
  tuneGrid = expand.grid(k = 2:20),
  preProcess = c("center", "scale")
)

# accuracy per k
knn_model$results[, c("k", "Accuracy")]

# best k
knn_model$bestTune

# validation on df test
knn_pred <- predict(knn_model, df_test)

# results
confusionMatrix(knn_pred, df_test$ndvi_class)

# visualizing results
df_test$pred <- knn_pred

ggplot(
  df_test, 
  aes(
    x = .data[[features[1]]], 
    y = .data[[features[2]]],
    color = pred)
  ) + geom_point(alpha = 0.5)