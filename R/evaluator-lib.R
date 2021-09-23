#' Summarize evaluation results.
#' 
#' @description General helper function to summarize evaluation results in 
#'   a nice, organized table.
#'   
#' @param eval_results A list of result tibbles, as returned by the
#'   \code{Experiment$evaluate()} method.
#' @param id Character string. ID used for naming columns.
#' @param value Character string. Name of column in \code{eval_results} to
#'   summarize.
#' @param summary_funs Character vector specifying how to summarize 
#'   evaluation metrics. Elements of the vector must be one of "mean", "median",
#'   "min", "max", "sd", "raw".
#' @param custom_summary_funs Named list of custom functions to summarize 
#'   results. Names in the list should correspond to the name of the summary
#'   function. Values in the list should be a function that takes in one 
#'   argument, that being the values of the evaluated metrics. See examples.
#' @param na.rm Logical. Should missing values be removed in evaluating summary
#'   functions?
#' 
#' @return A ggplot object or list of ggplot objects.
#' 
#' @export
summarize_eval_results <- function(eval_results, id, value = "value",
                                   summary_funs = c("mean", "median", "min",
                                                    "max", "sd", "raw"), 
                                   custom_summary_funs = NULL,
                                   na.rm = FALSE) {
  
  group_ids <- dplyr::group_vars(eval_results)
  
  # summarize results according to summary_funs
  eval_out <- purrr::map(
    summary_funs,
    function(f) {
      summary_fun <- eval(parse(text = f))
      col_name <- paste0(f, "_", id)
      if (f == "raw") {
        eval_out <- eval_results %>%
          dplyr::summarise(summary = list(.data[[value]]), 
                           .groups = "keep") %>%
          dplyr::rename({{col_name}} := "summary") %>%
          dplyr::ungroup()
      } else {
        eval_out <- eval_results %>%
          dplyr::summarise(summary = summary_fun(.data[[value]], na.rm = na.rm),
                           .groups = "keep") %>%
          dplyr::rename({{col_name}} := "summary")
      }
      return(eval_out)
    }
  ) %>%
    purrr::reduce(dplyr::left_join, by = group_ids) %>%
    dplyr::group_by(dplyr::across({{group_ids}}))
  
  # summarize results according to custom_summary_funs
  if (!is.null(custom_summary_funs)) {
    if (is.null(names(custom_summary_funs))) {
      names(custom_summary_funs) <- paste0(id, "_summary", 
                                           1:length(custom_summary_funs))
    }
    custom_eval_out <- purrr::map(
      1:length(custom_summary_funs),
      function(i) {
        summary_name <- names(custom_summary_funs)[i]
        summary_fun <- custom_summary_funs[[i]]
        eval_results %>% 
          dplyr::summarise({{summary_name}} := summary_fun(.data[[value]]),
                           .groups = "keep")
      }
    ) %>%
      purrr::reduce(dplyr::left_join, by = group_ids)
    eval_out <- dplyr::left_join(eval_out, custom_eval_out, by = group_ids) %>%
      dplyr::group_by(dplyr::across({{group_ids}}))
  }
  
  return(eval_out)
}