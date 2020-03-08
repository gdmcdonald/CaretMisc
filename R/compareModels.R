#' Compare evaluation metrics for cross-validation on the training set and on the test set for a caret model.
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
compareModels <- function(model_list, test_data, model_order = "RMSE", title = "Comparing model metrics"){

  #Get outcome variable name
  outcome_var <- as.character(
    model_list[[1]]$terms[[2]]
  )

  # Get training data metrics

  allResamples <- caret::resamples(model_list)

  tidy_train_metrics <-
    lapply(model_list,
           function(x){
             x[["resample"]] %>%
               tidyr::gather(-Resample,key = "metric",value = "value")
           }) %>%
    dplyr::bind_rows(.id = 'model') %>%
    dplyr::mutate(test_train = "1. train")

  # Get testing data metrics

  allPreds<-lapply(model_list,predict,test_data) %>%
    unlist() %>%
    matrix(.,ncol = length(model_list)) %>%
    {colnames(.)<-names(model_list);
    .}

  tidy_test_metrics <- apply(allPreds,
                             2,
                             caret::postResample,
                             obs = test_data[[outcome_var]]) %>%
    dplyr::as_tibble(rownames = "metric") %>%
    tidyr::pivot_longer(-metric,names_to = c("model")) %>%
    dplyr::mutate(test_train = "2. test",
                  Resample = "test")

  # bind metrics together
  metrics <- rbind(tidy_test_metrics,
                   tidy_train_metrics)

  #summary of metrics for ordering
  sm <- metrics %>%
    dplyr::group_by(model,metric) %>%
    dplyr::summarise(mean_metric = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(metric,mean_metric)

  model_order <- sm %>%
    dplyr::filter(metric == model_order) %>%
    {.[["model"]]}

  #order the models and then plot everything
  p <- metrics %>%
    dplyr::mutate(model = factor(model, levels = model_order, ordered = T)) %>%
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
