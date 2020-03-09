#' Evaluate a caret classification model using AUC and other metrics.
#'
#' @param trained_model A caret classifier model.
#' @param test_data The test set of data for evaluation.
#' @return a dataframe is returned and a ggplot is printed as a side effect.
#' @export
eval_classifier <- function(trained_model, test_data) {

  outcome_var <- as.character(
    trained_model$terms[[2]]
  )

  y_test <- test_data[[outcome_var]]

  # make predictions and probailities on the test set
  y_pred <- predict(trained_model, test_data, type = "raw")
  y_pred_prob <- predict(trained_model, test_data, type = "prob")
  # spit out the confusion matrix on the test set
  confm <- confusionMatrix(data = y_pred, y_test, positive="diabetes")
  print(confm)

  # make test predictions data frame
  tdf <- tibble(y_pred,
                diabetes = y_pred_prob$diabetes, #B
                normal = y_pred_prob$normal,
                y_test = y_test
  )

  # Select a parameter setting if random forest
  if (trained_model$method == "rf") {
    selectedIndices <- trained_model$pred$mtry == 2
    selected_pred <- trained_model$pred[selectedIndices, ]
  } else {
    selected_pred <- trained_model$pred
  }

  # Get the test set AUC:
  test_auc <- auc(y_test, y_pred_prob$diabetes)

  metrics <- data.frame(
    accuracy = confm[['overall']]['Accuracy'],
    sensitivity = confm[['byClass']]['Sensitivity'],
    specificity = confm[['byClass']]['Specificity'],
    precision = confm[['byClass']]['Precision'],
    recall = confm[['byClass']]['Recall'],
    test_auc = test_auc) %>%
    mutate(f1 = 2*((precision*recall)/(precision+recall)))

  # plot train ROC in red
  p <- ggplot(
    selected_pred,
    aes(m = normal, d = obs, col = "train")
  ) +
    geom_roc(
      hjust = -0.4,
      vjust = 1.5
    ) +
    # add test ROC in blue
    geom_roc(
      hjust = -0.4,
      vjust = 1.5,
      data = tdf,
      mapping = aes(m = normal, d = y_test, col = "test")
    ) +
    scale_color_manual(name="Dataset",
                       values=c(train="red", test="blue")) +
    # make it look prettier
    theme_classic() +
    coord_equal() +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    labs(title = paste0("Test AUC = ", format(round(test_auc, 3), nsmall = 3), " for ", trained_model$modelInfo$label))

  print(p)

  return(invisible(metrics))
}
