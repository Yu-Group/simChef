betas_params_doc <- function(type = c("obs", "unobs", "corr", "uncorr")) {
  type <- match.arg(type)
  descr_suffix <- switch(
    type,
    "obs" = c("observed design matrix", ""),
    "unobs" = c("unobserved design matrix", "_unobs"),
    "corr" = c("correlated features", "_corr"),
    "uncorr" = c("uncorrelated features", "_uncorr")
  )
  sprintf(c(
    paste(
      "@param betas%2$s Coefficient vector for %1$s. If a",
      "scalar is provided, the coefficient vector is a constant vector. If",
      "\\code{NULL} (default), entries in the coefficient vector are drawn iid from",
      "N(0, \\code{betas%2$s_sd}^2). Can also be a function that generates the",
      "coefficient vector; see \\code{generate_coef()}."),
    paste(
      "@param betas%2$s_sd (Optional) SD of normal distribution from which to draw",
      "\\code{betas%2$s}. Only used if \\code{betas%2$s} argument is \\code{NULL} or",
      "is a function in which case \\code{betas%2$s_sd} is optionally passed to the",
      "function as \\code{sd}; see \\code{generate_coef()}."
    )
  ), descr_suffix[1], descr_suffix[2])
}

#' Arguments that are shared by multiple \code{DGP} library functions.
#'
#' @name shared_dgp_lib_args
#'
#' @eval betas_params_doc()
#' @eval betas_params_doc("unobs")
#' @eval betas_params_doc("corr")
#' @eval betas_params_doc("uncorr")
#' @param data_split Logical; if \code{TRUE}, splits data into training and test
#'   sets according to \code{train_prop}.
#' @param err Function from which to generate simulated error vector. Default is
#'   \code{NULL} which adds no error to the DGP.
#' @param intercept Scalar intercept term.
#' @param n Number of samples.
#' @param p Number of features.
#' @param return_support Logical specifying whether or not to return a vector of
#'   the support column names. If \code{X} has no column names, then the indices
#'   of the support are used.
#' @param return_values Character vector indicating what objects to return in
#'   list. Elements in vector must be one of "X", "y", "support".
#' @param support Vector of feature indices in the true support of the DGP.
#' @param train_prop Proportion of data in training set if \code{data_split =
#'   TRUE}.
#' @param X Data matrix or data frame.
#' @param y Response vector.
#' @param ... Other arguments to pass to err() to generate the error vector.
#'   
#' @returns A list of the named objects that were requested in
#'   \code{return_values}. See brief descriptions below.
#' \describe{
#' \item{X}{A \code{data.frame}.}
#' \item{y}{A response vector of length \code{nrow(X)}.}
#' \item{support}{A vector of feature indices indicating all features used in
#'   the true support of the DGP.}
#' }
#' Note that if \code{data_split = TRUE} and "X", "y" 
#'   are in \code{return_values}, then the returned list also contains slots for
#'   "Xtest" and "ytest".
#'
#' @keywords internal
NULL

#' Helper function to generate a coefficient vector.
#' 
#' @description Generate a coefficient vector with the specified dimensions and
#'   sparsity level.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param betas Coefficient vector or function to generate the coefficients. If
#'   a scalar is provided, the coefficient vector is a constant vector. If
#'   \code{NULL} (default), entries in the coefficient vector are drawn iid from
#'   N(0, \code{sd}^2). If a function, must take the integer argument \code{p}
#'   (and optionally \code{s}, \code{sd}, \code{betas_name}, and other
#'   user-defined args from \code{...}) and return a numeric vector.
#' @param s Sparsity level. Coefficients corresponding to features after the
#'   \code{s}th position (i.e., positions i = \code{s} + 1, ..., \code{p}) are
#'   set to 0.
#' @param sd (Optional) SD of normal distribution from which to draw
#'   \code{betas}. Only used if \code{betas} argument is \code{NULL}.
#' @param betas_name Name of coefficient variable to use in error message.
#' @param ... Additional user arguments to pass to \code{betas} when
#'   \code{betas} is a function.
#' 
#' @returns A vector of length \code{p}.
#' 
#' @examples
#' # generate beta ~ N(0, 1) of dimension 10
#' beta <- generate_coef(p = 10)
#' 
#' # generate beta = [1, 1, 0, 0, 0]
#' beta <- generate_coef(betas = 1, p = 5, s = 2)
#' 
#' # generate beta = [1, 2, 3]
#' beta <- generate_coef(betas = 1:3, p = 3)
#' 
#' @export
generate_coef <- function(betas = NULL, p = 1, s = p, sd = 1,
                          betas_name = "betas", ...) {
  if (s > p) {
    stop(sprintf("Got s=%s, but should be less than or equal to p=%s.", s, p))
  }
  if (is.function(betas)) {
    betas <- R.utils::doCall(betas, ...,
                             args = list(s=s, sd=sd, betas_name=betas_name),
                             alwaysArgs = list(p=p))
    if (!is.numeric(betas)) {
      msg <- sprintf(
        paste("%s is a function but didn't return a numeric vector.",
              "Instead, returned object with class(es): %s."),
        betas_name, paste0(class(betas), collapse=", ")
      )
      stop(msg)
    }
  }
  if (is.numeric(betas)) {
    if (length(betas) == 1 && p > 1) {
      betas <- rep(betas, length.out = min(s, p))
    } else if (length(betas) != p) {
      stop(sprintf("%s must have length 1 or %s.", betas_name, p))
    }
  } else if (!is.null(betas)) {
    msg <- sprintf(
      paste("%s must be NULL, a function that returns a numeric vector,",
            "or a fixed numeric vector, but instead had class: %s."),
      betas_name, paste0(class(betas), collapse=", ")
    )
    stop(msg)
  } else {
    # simulate betas from gaussian by default
    betas <- stats::rnorm(min(s, p), mean = 0, sd = sd)
  }
  if (length(betas) != p) {
      betas <- c(betas, rep(0, p - length(betas)))
  }
  return(betas)
}

#' Developer function to return consistent outputs in DGP.
#' 
#' @description Helper function that returns a consistent output for any DGP
#'   function.
#'   
#' @inheritParams shared_dgp_lib_args
#' 
#' @inherit shared_dgp_lib_args return
#' 
#' @examples 
#' # Return training/test splits using iris data and completely dense support 
#' dgp_out <- return_DGP_output(X = iris %>% dplyr::select(-Species),
#'                              y = iris$Species,
#'                              support = 1:4,
#'                              data_split = TRUE,
#'                              train_prop = 0.5,
#'                              return_values = c("X", "y", "support"))
#' 
#' @export
return_DGP_output <- function(X, y, support, data_split, train_prop,
                              return_values) {
  out <- NULL
  X <- as.data.frame(X)
  if (any(c("X", "y") %in% return_values)) {
    if (data_split) {
      data_out <- split_data(X = X, y = y, train_prop = train_prop)
    } else {
      data_out <- list(X = X, y = y)
    }
    if ("X" %in% return_values) {
      out <- c(out, data_out[intersect(names(data_out), c("X", "Xtest"))])
    }
    if ("y" %in% return_values) {
      out <- c(out, data_out[intersect(names(data_out), c("y", "ytest"))])
    }
  }
  if ("support" %in% return_values) {
    out <- c(out, list(support = support))
  }
  return(out)
}

#' Helper function to split data into training and test sets
#' 
#' @inheritParams shared_dgp_lib_args
#' @param train_prop Proportion of data in training set.
#' 
#' @returns A list of four: "X", "y", "Xtest", and "ytest" containing the
#'   training data, training response, test data, and test response,
#'   respectively.
#' 
#' @keywords internal
split_data <- function(X, y, train_prop = 0.5) {
  n <- nrow(X)
  train_ids <- sample(1:n, size = round(n * train_prop), replace = F)
  out <- list(X = X[train_ids, , drop = FALSE], y = y[train_ids],
              Xtest = X[-train_ids, , drop = FALSE], ytest = y[-train_ids])
  return(out)
}

#' Helper function to compute indicator function in LSS model.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param thresh thresholds
#' @param sgn sign of the interaction
#' 
#' @keywords internal
indicator <- function(X, thresh, sgn) {
  if (any(sgn == -1)) {
    X[, sgn == -1] <- X[, sgn == -1] * -1
    thresh[sgn == -1] <- thresh[sgn == -1] * -1
  }
  thresh_mat <- matrix(thresh, byrow = T, nrow = nrow(X), ncol = ncol(X))
  indic <- apply(X > thresh_mat, 1, all)
  return(as.integer(indic))
}

#' Helper function to process ... args to pass to multiple functions.
#'
#' @param prefix Character vector of prefixes for the \code{*_fun} functions
#'   used by the function that calls \code{dots_to_fun_args()}; e.g., if
#'   \code{x_fun} is one of the functions, then this vector should include
#'   \code{"x"}. Then, this function will look for an argument with name
#'   prefixed by \code{".x_"} in \code{...}.
#' @param ... Named arguments to process.
#'
#' @return A list with some or all of the following:
#' \describe{
#' \item{.x_args}{Args to pass to \code{x_fun}.}
#' \item{.y_args}{Args to pass to \code{y_fun}.}
#' \item{.err_args}{Args to pass to \code{err_fun}.}
#' \item{.betas_args}{Args to pass to \code{betas}.}
#' \item{.optional_args}{Args to pass optionally.}
#' }
#'
#' @keywords internal
dots_to_fun_args <- function(fun_prefix = c("x", "y", "err", "betas"), ...) {
  fun_prefix <- match.arg(fun_prefix, several.ok = TRUE)
  prefixes <- paste0(".", fun_prefix, "_")
  out_list <- vector(mode = "list", length = length(prefixes) + 1)
  names(out_list) <- c(paste0(prefixes, "args"), ".optional_args")
  args_list <- rlang::list2(...)
  if (!identical(args_list, list())) {
    caller_name <- as.character(sys.call(1))[1]
    if (is.null(names(args_list)) ||
          any(names(args_list) %in% c("", prefixes))) {
      stop(sprintf(
        "Additional arguments passed to %s() must be named.",
        caller_name
      ))
    }
    for (arg_name in names(args_list)) {
      if (!startsWith(arg_name, ".")) {
        next
      }
      for (prefix in prefixes) {
        if (startsWith(arg_name, prefix)) {
          out_list_name <- paste0(prefix, "args")
          root_arg_name <- substr(arg_name, nchar(prefix) + 1, nchar(arg_name))
          out_list[[out_list_name]][[root_arg_name]] <- args_list[[arg_name]]
          args_list[[arg_name]] <- NULL
          break
        }
      }
    }
    if (length(args_list) > 0) {
      out_list[[".optional_args"]] <- args_list
    }
  }
  return(out_list)
}
