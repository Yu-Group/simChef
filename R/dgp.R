#' A data-generating process
#'
#' @export
DGP <- R6::R6Class(
  classname = 'DGP',
  public = list(
    n_obs = NULL,
    dgp_fun = NULL,
    initialize = function(dgp_fun, n_obs, ...) {
      self$dgp_fun <- dgp_fun
      self$n_obs <- n_obs
    },
    generate = function(n_obs, ...) {
      if (missing(n_obs)) {
        # TODO: handle the vector-valued n_obs case
        n_obs <- self$n_obs[1]
      }
      data_list <- self$dgp_fun(n_obs, ...)
      if (is.null(names(data_list))) {
        names(data_list) <- paste0("dataset", 1:length(data_list))
      }
      data_tib <- tibble::tibble(n_obs=n_obs, name=names(data_list), data_list)
      return(data_tib)
    }
  )
)

#' @export
create_dgp <- function(dgp_fun=NULL, ...) {
  return(DGP$new(dgp_fun, ...))
}
