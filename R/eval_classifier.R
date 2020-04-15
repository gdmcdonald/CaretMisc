#' Evaluate a caret classification model using AUC and other metrics.
#'
#' @param trained_model A caret classifier model.
#' @param test_data The test set of data for evaluation.
#' @param positive The name of the positive class. Defaults to the first level if positive = NULL.
#' @return a dataframe is returned and a ggplot is printed as a side effect.
#' @export
eval_classifier <- function(trained_model, test_data, positive = NULL) {

  if (!is.null(positive)){
    if (!(positive %in% trained_model$levels)){
      positive <- NULL
      warning("The specified positive class is not in the outcome variable. Defaulting to the first level in the outcome variable.")
    }
  }

  outcome_var <- as.character(
    trained_model$terms[[2]]
  )

  y_test <- test_data[[outcome_var]]

  # make predictions and probailities on the test set
  y_pred <- predict(trained_model, test_data, type = "raw")
  y_pred_prob <- predict(trained_model, test_data, type = "prob")
  # spit out the confusion matrix on the test set
  confm <- caret::confusionMatrix(data = y_pred, y_test, positive=positive)
  print(confm)
  # get the positive and negative class names

  pos_name <- confm$positive
  neg_name <- setdiff(names(y_pred_prob),pos_name)
  # make test predictions data frame
  tdf <- cbind(y_pred, y_pred_prob, y_test)

  # Select a parameter setting if random forest
  if (trained_model$method == "rf") {
    selectedIndices <- trained_model$pred$mtry == 2
    selected_pred <- trained_model$pred[selectedIndices, ]
  } else {
    selected_pred <- trained_model$pred
  }

  # Get the test set AUC:
  test_auc <- pROC::auc((y_test==pos_name)*1, y_pred_prob[[pos_name]])

  metrics <- data.frame(
    accuracy = confm[['overall']]['Accuracy'],
    sensitivity = confm[['byClass']]['Sensitivity'],
    specificity = confm[['byClass']]['Specificity'],
    precision = confm[['byClass']]['Precision'],
    recall = confm[['byClass']]['Recall'],
    test_auc = test_auc) %>%
    mutate(f1 = 2*((precision*recall)/(precision+recall)))

  # plot train ROC in red
  selected_pred$D <- 1*(as.character(selected_pred$obs)==neg_name)
  selected_pred$colour <- "train"
  test_D <- 1*(as.character(y_test)==neg_name)
  test_colour <- rep("test",length(test_D))

  p <- ggplot(selected_pred,
              aes_string(m = neg_name, d = "D", col = "colour")
  ) +
    plotROC::geom_roc(
      hjust = -0.4,
      vjust = 1.5
    ) +
    # add test ROC in blue
    plotROC::geom_roc(
      hjust = -0.4,
      vjust = 1.5,
      data = tdf,
      mapping = aes_string(m = neg_name, d = "test_D", col = "test_colour")
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
