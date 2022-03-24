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
      "scalar is provided, the coefficient vector is constant. If",
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

#' @keywords internal
dots_doc <- function(prefix = c("X", "U", "y", "err", "betas",
                                "betas_unobs", "betas_corr",
                                "betas_uncorr"),
                     see_also = NULL) {
  n_objs <- length(prefix)
  if (n_objs > 2) {
    objs <- paste0(
      paste0(prefix[-n_objs], collapse = ", "),
      ", and ", prefix[n_objs]
    )
    prefixes <- paste0(
      paste0(".", prefix[-n_objs], "_", collapse = ", "),
      paste0(", or .", prefix[n_objs], "_")
    )
  } else {
    objs <- paste0(prefix, collapse = " and ")
    prefixes <- paste0(".", prefix, "_", collapse = " or ")
  }
  param_string <- sprintf(
    paste(
      "@param ...",
      "Additional arguments to pass to functions that generate %1$s.",
      "If the argument doesn't exist in one of the functions it is ignored.",
      "If two or more of the functions have an argument of the same name but",
      "with different values, then use one of the following prefixes in front",
      "of the argument name (passed via \\code{...}) to differentiate it: %2$s."
    ), objs, prefixes
  )
  if (!is.null(see_also)) {
    n_funs <- length(see_also)
    if (n_funs > 2) {
      see_also_string <- paste0(
        paste0("\\code{", see_also[-n_funs], "()}", collapse = ", "),
        paste0(", and \\code{", see_also[n_funs], "()}")
      )
    } else {
      see_also_string <- paste0(
        "\\code{", see_also, "()}", collapse = " and "
      )
    }
    param_string <- paste(
      param_string,
      sprintf("For additional details, see %s", see_also_string)
    )
  }
  return(param_string)
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
#' @param n,.n Number of samples.
#' @param p,.p Number of features.
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
#' @param .betas Coefficient vector or function to generate the coefficients. If
#'   a scalar is provided, the coefficient vector is a constant vector. If
#'   \code{NULL} (default), \code{.s} non-zero entries in the coefficient vector
#'   are equal to 0.5. If a function, must take one or more of \code{n},
#'   \code{.s}, or \code{.p}, and can optionally take \code{.betas_name}, along
#'   with other user-defined args from \code{...}) and must return a numeric
#'   vector of length 1, \code{.s}, or \code{.p}. If the function take \code{n}
#'   but not \code{.s} or \code{.p}, then \code{.s} is passed as \code{n}.
#' @param .s Sparsity level. Coefficients corresponding to features after the
#'   \code{s}th position (i.e., positions i = \code{s} + 1, ..., \code{p}) are
#'   set to 0.
#' @param .betas_name Name of coefficient variable to use in error message.
#' @param ... Additional user arguments to pass to \code{.betas} when
#'   \code{.betas} is a function.
#'
#' @returns A vector of length \code{.p}.
#'
#' @examples
#' # generate constant beta = 0.5 of dimension 10
#' beta <- generate_coef(.p = 10)
#'
#' # generate beta ~ N(1, 2) of dimension 10
#' beta <- generate_coef(rnorm, .p = 10, mean = 1, sd = 2)
#'
#' # generate beta = [1, 1, 0, 0, 0]
#' beta <- generate_coef(.betas = 1, .p = 5, .s = 2)
#'
#' # generate beta = [1, 2, 3]
#' beta <- generate_coef(.betas = 1:3, .p = 3)
#'
#' # use a function to generate beta
#' beta_fun <- function(.s, df) {
#'   return(rt(n = .s, df = df))
#' }
#' beta <- generate_coef(.betas = beta_fun, .p = 10, .s = 3, df = 10)
#'
#' # we can do the same without wrapping rt
#' beta <- generate_coef(.betas = rt, .p = 10, .s = 3, df = 10)
#'
#' @export
generate_coef <- function(.betas = NULL, .p = 1, .s = .p, .betas_name = "betas",
                          ...) {
  if (.s > .p) {
    stop(sprintf("Got s=%s, but should be less than or equal to p=%s.", .s, .p))
  }
  if (is.null(.betas)) {
    .betas <- rep(0.5, .s)
  } else if (is.function(.betas)) {
    # .betas is a function, which must take n, .s, or .p
    args_intersect <- intersect(c("n", ".s", ".p"), methods::formalArgs(.betas))
    if (length(args_intersect) == 0) {
      msg <- paste0(
        .betas_name,
        " is a function but doesn't take an argument n, .s, or .p.",
      )
      stop(msg)
    }
    # call user's function
    fun_args <- dots_to_fun_args(
      prefix = .betas_name, ...
    )
    betas_args_list <- fun_args[[paste0(".", .betas_name, "_args")]]
    optional_args_list <- c(
      fun_args$.optional_args,
      list(.p = .p, .s = .s, .betas_name = .betas_name)
    )
    if (identical(args_intersect, "n")) {
      # user's function takes arg n, but not .s or .p
      betas_args_list$n <- .s
    }
    .betas <- R.utils::doCall(
      .betas,
      args = optional_args_list,
      alwaysArgs = betas_args_list
    )
  }
  if (is.numeric(.betas)) {
    if (!length(.betas) %in% c(1, .s, .p)) {
      stop(sprintf("%s must have length 1, %s, or %s.", .betas_name, .s, .p))
    }
    if (length(.betas) == 1 && .p > 1) {
      .betas <- rep(.betas, length.out = min(.s, .p))
    }
    .betas <- c(.betas, rep(0, .p - length(.betas)))
  } else {
    msg <- sprintf(
      paste(
        "%s must be NULL, a function that returns a numeric vector,",
        "or a fixed numeric vector, but instead had class: %s."
      ),
      .betas_name, paste0(class(.betas), collapse = ", ")
    )
    stop(msg)
  }
  return(.betas)
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
  train_ids <- sample(seq(n), size = round(n * train_prop), replace = FALSE)
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
#' @param prefix Character vector of arbitrary prefixes to use in splitting
#'   entries of \code{...}. For example, if \code{.X_p} and \code{.U_p} are in
#'   \code{...}, then this vector should include \code{"X"} and \code{"U"}
#'   so that this function will look for arguments with name prefixed by
#'   \code{".X_"} and \code{".U_"}.
#' @param ... Named arguments to process.
#'
#' @return A list with ".optional_args" as well as entries named like
#'   \code{".X_args"}. For example, when \code{"X"} is one of the elements of
#'   the input character vector \code{prefix} and \code{.X_p = 1} and \code{.X_s
#'   = 2} are both passed via \code{...}, the \code{".X_args"} entry of the
#'   output list will be code{list(p = 1, s = 2)}. With the default value of
#'   \code{prefix}, the output list will the following have entries:
#'
#' \describe{
#' \item{.X_args}{Args to pass to function that generates {X}.}
#' \item{.U_args}{Args to pass to function that generates {U}.}
#' \item{.y_args}{Args to pass to function that generates {y}.}
#' \item{.err_args}{Args to pass to function that generates \code{err}.}
#' \item{.betas_args}{Args to pass to function that generates \code{betas}.}
#' \item{.betas_unobs_args}{Args to pass to function that generates
#'   \code{betas_unobs}.}
#' \item{.betas_corr_args}{Args to pass to function that generates
#'   \code{betas_corr}.}
#' \item{.betas_uncorr_args}{Args to pass to function that generates
#'   \code{betas_uncorr}.}
#' \item{.optional_args}{Args to pass optionally to all functions.}
#' }
#'
#' @keywords internal
dots_to_fun_args <- function(prefix = c("X", "U", "y", "err", "betas",
                                        "betas_unobs", "betas_corr",
                                        "betas_uncorr"), ...) {
  prefixes <- paste0(".", prefix, "_")
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
    for (arg_name in names(args_list)[startsWith(names(args_list), ".")]) {
      ## find prefix that leads to longest match at beginning of arg_name
      prefixes_filtered <- prefixes[sapply(prefixes, function(pre) {
        startsWith(arg_name, pre)
      })]
      prefix <- prefixes_filtered[which.max(nchar(prefixes_filtered))]
      if (length(prefix) > 0 && nchar(prefix) < nchar(arg_name)) {
        out_list_name <- paste0(prefix, "args")
        root_arg_name <- substr(arg_name, nchar(prefix) + 1, nchar(arg_name))
        out_list[[out_list_name]][[root_arg_name]] <- args_list[[arg_name]]
        args_list[[arg_name]] <- NULL
      }
    }
    if (length(args_list) > 0) {
      out_list[[".optional_args"]] <- args_list
    }
  }
  return(out_list)
}

#'
#' @keywords internal
do_call <- function(.fun, ..., args = NULL, always_args = NULL) {
  if ("..." %in% methods::formalArgs(.fun)) {
    R.utils::doCall(
      .fun, ..., args = args, alwaysArgs = always_args,
      .ignoreUnusedArgs = FALSE
    )
  } else {
    R.utils::doCall(.fun, ..., args = args, alwaysArgs = always_args)
  }
}
