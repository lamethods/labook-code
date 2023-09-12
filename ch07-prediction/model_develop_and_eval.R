#
# Function for building a RandomForest classification model through cross-validation
#
build_RF_classification_model <- function(dataset) {
  
  n_features <- ncol(dataset)-1
  default_mtry <- round(sqrt(n_features))
  grid <- expand.grid(.mtry = (default_mtry-1):(default_mtry+1))
  
  ctrl <- trainControl(method = "CV", 
                       number = 10,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
  
  rf <- train(x = dataset |> select(-Course_outcome),
              y = dataset$Course_outcome,
              method = "rf",
              metric = "ROC",
              tuneGrid = grid,
              trControl = ctrl)
  
  rf$finalModel
}


#
# Function that computes four standard metrics for evaluation of 
# a classification model
#
get_classification_evaluation_measures <- function(model, test_data) {
  
  predicted_vals <- predict(model, 
                            test_data |> select(-Course_outcome))
  actual_vals <- test_data$Course_outcome
  
  cm <- table(actual_vals, predicted_vals)
  
  # low achievement the course is considered the positive class
  TP <- cm[2,2]
  TN <- cm[1,1]
  FP <- cm[1,2]
  FN <- cm[2,1]

  accuracy = sum(diag(cm)) / sum(cm)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  c(Accuracy = accuracy, 
    Precision = precision, 
    Recall = recall, 
    F1 = F1)
  
}


#
# Function for computing and plotting importance of variables in a RandomForest model
#
compute_and_plot_variable_importance <- function(rf_model) {
  
  importance(rf_model, type=2) |> 
    as.data.frame() |> 
    (function(x) mutate(x, variable = rownames(x)))() -> var_imp_df
  
  row.names(var_imp_df) <- 1:nrow(var_imp_df)
  colnames(var_imp_df) <- c("importance","variable")
  
  ggplot(var_imp_df, 
         aes(x = reorder(variable, importance), y = importance)) +
    geom_col(width=0.35) +
    labs(x = "", y = "", title = "Feature importance") +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_text(size=10, color="black"))
}


#
# Function for building a RandomForest regression model through cross-validation
#
build_RF_regression_model <- function(dataset) {
  
  n_features <- ncol(dataset)-1
  default_mtry <- round(sqrt(n_features))
  grid <- expand.grid(.mtry = (default_mtry-1):(default_mtry+1))
  
  ctrl <- trainControl(method = "CV", 
                       number = 10)
  
  rf <- train(x = dataset |> select(-Final_grade),
              y = dataset$Final_grade,
              method = "rf",
              metric = "RMSE",
              tuneGrid = grid,
              trControl = ctrl)
  
  rf$finalModel
}


#
# Function that computes four standard metrics for evaluation of 
# a regression model (R2, RMSE, MAE)
#
get_regression_evaluation_measures <- function(model, train_ds, test_data) {
  
  predicted_vals <- predict(model, 
                            test_data |> select(-Final_grade))
  actual_vals <- test_data$Final_grade
  
  #R2 = 1 - RSS/TSS
  RSS <- sum((predicted_vals - actual_vals)^2)
  TSS <- sum((median(train_ds$Final_grade) - actual_vals)^2)
  R2 <- 1 - RSS/TSS
  
  #RMSE = sqrt(RSS/N)
  RMSE <- sqrt(RSS/nrow(test_ds))
  
  #MAE = avg(abs(predicted - actual))
  MAE <- mean(abs(predicted_vals - actual_vals))
  
  c(R2 = R2, RMSE = RMSE, MAE = MAE)
}
