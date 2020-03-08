#' Compare evaluation metrics for cross-validation on the training set and on the test set for a caret model, return the result as a tibble.
#'
#' @param model_list A named list of caret models.
#' @param test_data The test set of data for evaluation.
#' @param model_order an evaluation metric as a string, either "RMSE", "MAE" or "Rsquared". Default "RMSE".
#' @return a tibble with all the metrics for each model on the training and test data.
#' @examples
#' compareModelsDf(model_list = allModels,
#'               test_data = ames_test_engineered,
#'               model_order = "RMSE")
#' @export
compareModelsDf <- function(model_list, test_data = NULL, model_order = "RMSE"){

  #Get outcome variable name

  outcome_var <- as.character(
    model_list[[1]]$terms[[2]]
  )

  # Get training data metrics

  tidy_train_metrics <-
    lapply(model_list,
           function(x){
             x[["resample"]] %>%
               tidyr::gather(-Resample,key = "metric",value = "value")
           }) %>%
    dplyr::bind_rows(.id = 'model') %>%
    dplyr::mutate(test_train = "1. train")

  # Get testing data metrics if test data was supplied
  if (!is.null(test_data)){

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

  } else {
    metrics <- tidy_train_metrics
  }

  #summary of metrics for ordering
  sm <- metrics %>%
    dplyr::group_by(model,metric) %>%
    dplyr::summarise(median_metric = median(value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(metric,median_metric)

  if (is.null(test_data)){

    model_order <- sm %>%
      dplyr::filter(metric == model_order) %>%
      {.[["model"]]}

  } else {

    model_order <- metrics %>%
      dplyr::filter(metric == model_order,
                    test_train == "2. test") %>%
      dplyr::arrange(value) %>%
      {.[["model"]]}

  }


  #order the models and then output the df.
  out <- metrics %>%
    dplyr::mutate(model = factor(model, levels = model_order, ordered = T))
}
