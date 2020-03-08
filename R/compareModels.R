#' Compare evaluation metrics for cross-validation on the training set and on the test set for a caret model. Return the result as a ggplot object.
#'
#' @param model_list A named list of caret models.
#' @param test_data The test set of data for evaluation.
#' @param model_order an evaluation metric as a string, either "RMSE", "MAE" or "Rsquared". Default "RMSE".
#' @param title A string to title the resulting ggplot graph.
#' @return a ggplot object.
#' @examples
#' compareModels(model_list = allModels,
#'               test_data = ames_test_engineered,
#'               model_order = "RMSE")
#' @export
compareModels <- function(model_list, test_data = NULL, model_order = "RMSE", title = "Comparing model metrics"){

  #Plot the result of the compareModelsDf function
  p <- compareModelsDf(model_list = model_list,
                       test_data = test_data,
                       model_order = model_order) %>%
    ggplot2::ggplot(aes(y = value,
                        x = model,
                        color = test_train))+
    ggplot2::geom_boxplot()+
    ggplot2::facet_grid(metric~., scales = "free")+
    ggplot2::theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    ggplot2::labs(x = "Model",
                  y = "Metric",
                  title = title,
                  color = "Train CV / Test")
}
