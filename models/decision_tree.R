#install.packages("rpart.plot")
#install.packages("caret")

library(caret)
library(rpart)
library(rpart.plot)

# loading csv data from github
data_url <- "https://raw.githubusercontent.com/jxxtino/tae-0126-guilherme-luana-matheus/refs/heads/main/data/02-processed/df_clean.txt"

# creating df and removing NA values
df <- read.csv(data_url, sep = ";")
df <- na.omit(df)

# target, features and formula for decision tree regression
target <- "ndbi_mean"
features <- c("ndvi_mean")
formula = reformulate(features, response = target)

# train/test split
samples_index <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
df_train <- df[samples_index,]
df_test <- df[-samples_index,]

# decision tree model for regression
model <- rpart(
  formula,
  data = df_train,
  method = "anova",
  control = rpart.control(minsplit = 50, cp = 0))

# model predict
model_pred <- predict(model, df_test, method = "anova")

# RMSE (Root Mean Square Error)
rmse <- sqrt(mean((df_test[,target] - model_pred)^2))

# r-squared 
ssr <- sum((df_test[,target] - model_pred)^2) # Sum of Squared Errors / Residuals
sst <- sum((df_test[,target] - mean(df_test[,target]))^2) # Total Sum of Squares
rsq <- 1 - (ssr / sst)

# MAE (Mean Absolute Error)
mae <- mean(abs(model_pred - df_test[,target]))

# metrics
metrics <- round(c(rmse, rsq, mae), 3)
print(paste("RMSE: ", metrics[1], "R^2: ", metrics[2], "MAE: ", metrics[3]))

## ======= MODEL OPTIMIZATION ==================================================

results <- data.frame()
splits <- seq(5, 50, by = 2)
ctrl <- trainControl(method = "cv", number = 10)

for (s in splits){
  opt_model <- train(
    formula,
    data = df_train,
    method = "rpart",
    trControl = ctrl,
    tuneGrid = expand.grid(cp = seq(0.001, 0.05, by = 0.005)),
    control = rpart.control(minsplit = s)
  )
  
  best_index <- which.min(opt_model$results$RMSE)
  result <- opt_model$results[best_index,]
  
  results <- rbind(
    results,
    data.frame(
      MINSPLITS = s,
      CP = result$cp,
      RMSE = result$RMSE,
      R_2 = result$Rsquared,
      MAE = result$MAE
    )
  )
}

best_params <- results[which.min(results$RMSE),]

final_model <- train(
  formula,
  data = df_train,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = expand.grid(cp = best_params$CP),
  control = rpart.control(minsplit = best_params$MINSPLITS)
)

final_model_pred <- predict(final_model, df_test)

final_model_rmse <- sqrt(mean((df_test[,target] - final_model_pred)^2))

final_model_ssr <- sum((df_test[,target] - final_model_pred)^2) 
final_model_sst <- sum((df_test[,target] - mean(df_test[,target]))^2)
final_model_rsq <- 1 - (final_model_ssr / final_model_sst)

final_model_mae <- mean(abs(final_model_pred - df_test[,target]))

rpart.plot(final_model$finalModel, main = paste("Decision Tree for", target))

final_model_metrics <- round(c(final_model_rmse, final_model_rsq, final_model_mae), 3)
print(paste("RMSE: ", final_model_metrics[1], 
            "R2: ", final_model_metrics[2], 
            "MAE: ", final_model_metrics[3]))