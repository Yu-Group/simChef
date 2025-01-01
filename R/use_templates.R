#' Functions to create boilerplate code for specific types of experiments.
#'
#' @description These functions make suggestions for code when performing a few
#'   common types of experiments (i.e., for prediction, feature selection, and
#'   inference). They print out code to the console that could be considered
#'   minimal syntax.
#'
#' @param experiment_name Name of the experiment.
#' @param type Either "regression" or "classification" specifying the type of
#'   prediction problem.
#' @param support Logical. If `TRUE`, include code to evaluate the
#'   estimated feature support.
#' @param include_dgp_example Logical. If `TRUE`, include a completed
#'   DGP example, rather than a fill-in-the-blank template.
#' @param include_method_example Logical. If `TRUE`, include a completed
#'   Method example, rather than a fill-in-the-blank template.
#'
#' @returns Invisible `NULL` but code is printed to the console.
#'
#' @name library_templates
#' @rdname library_templates
#'
#' @examples
#' # prediction templates
#' use_prediction_template(type = "regression")
#' use_prediction_template(type = "classification")
#'
#' # prediction templates with example DGP and Method
#' use_prediction_template(type = "regression",
#'                         include_dgp_example = TRUE,
#'                         include_method_example = TRUE)
#' use_prediction_template(type = "classification",
#'                         include_dgp_example = TRUE,
#'                         include_method_example = TRUE)
#'
#' # feature selection template
#' use_feature_selection_template()
#' # feature selection template with example DGP and Method
#' use_feature_selection_template(include_dgp_example = TRUE,
#'                                include_method_example = TRUE)
#'
#' # inference template
#' use_inference_template()
#' # inference template with example DGP and Method
#' use_inference_template(include_dgp_example = TRUE,
#'                        include_method_example = TRUE)
#'
NULL

#' @rdname library_templates
#'
#' @export
use_prediction_template <- function(experiment_name = "Prediction Experiment",
                                    type = c("regression", "classification"),
                                    support = FALSE,
                                    include_dgp_example = FALSE,
                                    include_method_example = FALSE) {
  type <- match.arg(type)

  github_packages <- NULL
  # if (include_dgp_example) {
  #   github_packages <- "Yu-Group/dgpoix"
  # }
  cran_packages <- NULL
  if (include_method_example) {
    cran_packages <- c("dplyr", "purrr", "ranger")
  }
  req_packages <- use_library_template(
    cran_packages = cran_packages, github_packages = github_packages
  )

  if (include_dgp_example) {
    dgp_names <- use_dgp_template(ids = paste0(type, "-example"))
  } else {
    dgp_names <- use_dgp_template()
  }

  if (include_method_example) {
    method_names <- use_method_template(ids = "RF")
  } else {
    method_names <- use_method_template()
  }

  # set parameters for evaluators and visualizers
  nested_pred_cols <- "nested_pred_cols"
  true_pred_col <- "true_pred_col"
  est_pred_col <- "est_pred_col"
  if (type == "classification") {
    prob_pred_cols <- "prob_pred_cols"
  } else {
    prob_pred_cols <- NULL
  }
  if (support) {
    nested_feature_cols <- "nested_feature_cols"
    feature_col <- "feature_col"
    true_feature_col <- "true_feature_col"
    feature_imp_col <- "feature_imp_col"
    feature_sel_col <- "feature_sel_col"
  } else {
    nested_feature_cols <- NULL
    feature_col <- NULL
    true_feature_col <- NULL
    feature_imp_col <- NULL
    feature_sel_col <- NULL
  }
  if (include_method_example) {
    create_assign_str(
      nested_pred_cols,
      "c('y', 'predictions', 'prob_predictions')  # prediction results columns to be unnested"
    )
    create_assign_str(
      true_pred_col, "'y'  # true response column"
    )
    create_assign_str(
      est_pred_col, "'predictions'  # predicted response column"
    )
    create_assign_str(
      prob_pred_cols, "'1'  # predicted probability columns"
    )
    cat("\n")
    create_assign_str(
      nested_feature_cols,
      "'support_df'  # feature importance columns to be unnested"
    )
    create_assign_str(
      feature_col, "'feature'  # feature names column"
    )
    create_assign_str(
      true_feature_col, "'true_support'  # true feature support column"
    )
    create_assign_str(
      feature_imp_col, "'imp'  # feature importance column"
    )
    create_assign_str(
      feature_sel_col, "'selected'  # estimated feature support column"
    )
    cat("\n")
  } else {
    create_assign_str(nested_pred_cols,
                      use_variable_template(nested_pred_cols), TRUE)
    create_assign_str(true_pred_col,
                      use_variable_template(true_pred_col), TRUE)
    create_assign_str(est_pred_col,
                      use_variable_template(est_pred_col), TRUE)
    create_assign_str(prob_pred_cols,
                      use_variable_template(prob_pred_cols), TRUE)
    cat("\n")
    create_assign_str(nested_feature_cols,
                      use_variable_template(nested_feature_cols), TRUE)
    create_assign_str(feature_col,
                      use_variable_template(feature_col), TRUE)
    create_assign_str(true_feature_col,
                      use_variable_template(true_feature_col), TRUE)
    create_assign_str(feature_imp_col,
                      use_variable_template(feature_imp_col), TRUE)
    create_assign_str(feature_sel_col,
                      use_variable_template(feature_sel_col), TRUE)
    cat("\n")
  }

  eval_ids <- "pred_err"
  viz_ids <- "pred_err_plot"
  if (type == "classification") {
    viz_ids <- c(viz_ids, "roc_plot", "pr_plot")
  }
  if (support) {
    eval_ids <- c(eval_ids, "fi", "feature_sel")
    viz_ids <- c(viz_ids, "fi_plot", "feature_sel_plot",
                 "feature_roc_plot", "feature_pr_plot")
  }

  eval_names <- use_evaluator_template(
    ids = eval_ids,
    pred_nested_cols = nested_pred_cols, pred_truth_col = true_pred_col,
    pred_estimate_col = est_pred_col, pred_prob_cols = prob_pred_cols,
    feature_nested_cols = nested_feature_cols, feature_col = feature_col,
    feature_truth_col = true_feature_col, feature_imp_col = feature_imp_col,
    feature_sel_col = feature_sel_col
  )
  viz_names <- use_visualizer_template(
    ids = viz_ids,
    pred_nested_cols = nested_pred_cols, pred_truth_col = true_pred_col,
    pred_prob_cols = prob_pred_cols, feature_nested_cols = nested_feature_cols,
    feature_col = feature_col, feature_truth_col = true_feature_col,
    feature_imp_col = feature_imp_col
  )

  use_experiment_template(name = experiment_name,
                          dgp_names = dgp_names,
                          method_names = method_names,
                          eval_names = eval_names,
                          viz_names = viz_names)
  use_init_docs_template()
  use_run_template()
  use_render_docs_template()

  return(invisible(NULL))
}

#' @rdname library_templates
#'
#' @export
use_feature_selection_template <- function(experiment_name =
                                             "Feature Selection Experiment",
                                           include_dgp_example = FALSE,
                                           include_method_example = FALSE) {

  github_packages <- NULL
  # if (include_dgp_example) {
  #   github_packages <- "Yu-Group/dgpoix"
  # }
  cran_packages <- NULL
  if (include_method_example) {
    cran_packages <- c("dplyr", "purrr", "ranger")
  }
  req_packages <- use_library_template(
    cran_packages = cran_packages, github_packages = github_packages
  )

  if (include_dgp_example) {
    dgp_names <- use_dgp_template(ids = "regression-example")
  } else {
    dgp_names <- use_dgp_template()
  }

  if (include_method_example) {
    method_names <- use_method_template(ids = "RF")
  } else {
    method_names <- use_method_template()
  }

  # set parameters for evaluators and visualizers
  nested_feature_cols <- "nested_feature_cols"
  feature_col <- "feature_col"
  true_feature_col <- "true_feature_col"
  feature_imp_col <- "feature_imp_col"
  feature_sel_col <- "feature_sel_col"
  if (include_method_example) {
    create_assign_str(
      nested_feature_cols,
      "'support_df'  # feature importance columns to be unnested"
    )
    create_assign_str(
      feature_col, "'feature'  # feature names column"
    )
    create_assign_str(
      true_feature_col, "'true_support'  # true feature support column"
    )
    create_assign_str(
      feature_imp_col, "'imp'  # feature importance column"
    )
    create_assign_str(
      feature_sel_col, "'selected'  # estimated feature support column"
    )
    cat("\n")
  } else {
    create_assign_str(nested_feature_cols,
                      use_variable_template(nested_feature_cols), TRUE)
    create_assign_str(feature_col,
                      use_variable_template(feature_col), TRUE)
    create_assign_str(true_feature_col,
                      use_variable_template(true_feature_col), TRUE)
    create_assign_str(feature_imp_col,
                      use_variable_template(feature_imp_col), TRUE)
    create_assign_str(feature_sel_col,
                      use_variable_template(feature_sel_col), TRUE)
    cat("\n")
  }

  eval_ids <- c("fi", "feature_sel")
  viz_ids <- c("fi_plot", "feature_sel_plot",
               "feature_roc_curve", "feature_pr_curve")

  eval_names <- use_evaluator_template(
    ids = eval_ids,
    feature_nested_cols = nested_feature_cols, feature_col = feature_col,
    feature_truth_col = true_feature_col, feature_imp_col = feature_imp_col,
    feature_sel_col = feature_sel_col
  )
  viz_names <- use_visualizer_template(
    ids = viz_ids,
    feature_nested_cols = nested_feature_cols, feature_col = feature_col,
    feature_truth_col = true_feature_col, feature_imp_col = feature_imp_col
  )

  use_experiment_template(name = experiment_name,
                          dgp_names = dgp_names,
                          method_names = method_names,
                          eval_names = eval_names,
                          viz_names = viz_names)
  use_init_docs_template()
  use_run_template()
  use_render_docs_template()

  return(invisible(NULL))
}

#' @rdname library_templates
#'
#' @export
use_inference_template <- function(experiment_name = "Inference Experiment",
                                   include_dgp_example = FALSE,
                                   include_method_example = FALSE) {

  github_packages <- NULL
  # if (include_dgp_example) {
  #   github_packages <- "Yu-Group/dgpoix"
  # }
  cran_packages <- NULL
  if (include_method_example) {
    cran_packages <- "broom"
  }
  req_packages <- use_library_template(
    cran_packages = cran_packages, github_packages = github_packages
  )

  if (include_dgp_example) {
    dgp_names <- use_dgp_template(ids = "regression-example",
                                  data_split = FALSE)
  } else {
    dgp_names <- use_dgp_template()
  }

  if (include_method_example) {
    method_names <- use_method_template(ids = "OLS")
  } else {
    method_names <- use_method_template()
  }

  # set parameters for evaluators and visualizers
  nested_feature_cols <- "nested_feature_cols"
  feature_col <- "feature_col"
  true_feature_col <- "true_feature_col"
  pval_col <- "pval_col"
  if (include_method_example) {

    create_assign_str(
      nested_feature_cols,
      "'support_df'  # feature importance columns to be unnested"
    )
    create_assign_str(
      feature_col, "'feature'  # feature names column"
    )
    create_assign_str(
      true_feature_col, "'true_support'  # true feature support column"
    )
    create_assign_str(
      pval_col, "'pval'  # p-values column"
    )
    cat("\n")
  } else {
    create_assign_str(nested_feature_cols,
                      use_variable_template(nested_feature_cols), TRUE)
    create_assign_str(feature_col,
                      use_variable_template(feature_col), TRUE)
    create_assign_str(true_feature_col,
                      use_variable_template(true_feature_col), TRUE)
    create_assign_str(pval_col,
                      use_variable_template(pval_col), TRUE)
    cat("\n")
  }

  eval_ids <- c("inf_err", "fi_pval")
  viz_ids <- c("inf_err_plot", "inf_roc_plot", "inf_pr_plot",
               "reject_prob_plot")

  eval_names <- use_evaluator_template(
    ids = eval_ids,
    feature_nested_cols = nested_feature_cols, feature_col = feature_col,
    feature_truth_col = true_feature_col, feature_pval_col = pval_col
  )
  viz_names <- use_visualizer_template(
    ids = viz_ids,
    feature_nested_cols = nested_feature_cols,
    feature_col = feature_col, feature_truth_col = true_feature_col,
    feature_pval_col = pval_col
  )

  use_experiment_template(name = experiment_name,
                          dgp_names = dgp_names,
                          method_names = method_names,
                          eval_names = eval_names,
                          viz_names = viz_names)
  use_init_docs_template()
  use_run_template()
  use_render_docs_template()

  return(invisible(NULL))
}

#' Function to create boilerplate code for loading in packages
#'
#' @keywords internal
use_library_template <- function(cran_packages = NULL,
                                 github_packages = NULL) {
  library_str <- NULL
  if (!is.null(cran_packages)) {
    for (pkg in cran_packages) {
      library_str <- paste0(
        library_str,
        sprintf("if (!require('%s')) install.packages('%s')\n", pkg, pkg)
      )
    }
  }
  if (!is.null(github_packages)) {
    for (pkg in github_packages) {
      library_str <- paste0(
        library_str,
        sprintf("if (!require('%s')) devtools::install_github('%s')\n",
                basename(pkg), pkg)
      )
    }
  }

  if (!is.null(library_str)) {
    cat(library_str, "\n\n")
  }
  return("library")
}

#' Function to create boilerplate code for creating DGPs.
#'
#' @keywords internal
use_dgp_template <- function(ids = NULL, data_split = TRUE) {
  if (is.null(ids)) {
    dgp_str <- create_fun_str(
      name = "dgp",
      fun = "create_dgp",
      args = list(
        .dgp_fun = "stop('Add DGP function here.')",
        .name = "stop('Add name of DGP here.')",
        "stop('Add additional arguments (if necessary) to pass to DGP here.')"
      )
    )
  } else {
    ids <- match.arg(ids, choices = c("regression-example",
                                      "classification-example"))
    if (ids == "regression-example") {
      dgp_fun_name <- "gaussian_linear_dgp"
      dgp_name <- "'Example DGP (Uncorrelated Gaussian Linear DGP)'"
      if (data_split) {
        dgp_args <- list(
          n = 200, p = 10, beta = c(rep(1, 5), rep(0, 5)), err_sd = 1,
          data_split = TRUE, train_prop = 0.5, return_support = TRUE
        )
      } else {
        dgp_args <- list(
          n = 200, p = 10, beta = c(rep(1, 5), rep(0, 5)), err_sd = 1,
          data_split = FALSE, return_support = TRUE
        )
      }
      dgp <- function(n, p, beta = 1, err_sd = 1, data_split = TRUE,
                      train_prop = 0.5, return_support = TRUE) {

        X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
        beta_vec <- matrix(beta, ncol = 1, nrow = p)
        y <- X %*% beta_vec + rnorm(n = n, sd = err_sd)
        if (data_split) {
          train_ids <- sample(1:n, size = round(n * train_prop))
          Xtest <- X[-train_ids, , drop = FALSE]
          ytest <- y[-train_ids]
          X <- X[train_ids, , drop = FALSE]
          y <- y[train_ids]
        } else {
          Xtest <- NULL
          ytest <- NULL
        }
        if (return_support) {
          support <- which(beta_vec != 0)
          out <- list(X = X, y = y, Xtest = Xtest, ytest = ytest, support = support)
        } else {
          out <- list(X = X, y = y, Xtest = Xtest, ytest = ytest)
        }
        return(out)
      }

      # dgp_str <- create_fun_str(
      #   name = "dgp",
      #   fun = "create_dgp",
      #   args = list(
      #     .dgp_fun = "dgpoix::xy_dgp_constructor",
      #     .name = "'Example DGP (Uncorrelated Gaussian Linear DGP)'",
      #     X_fun = "dgpoix::generate_X_gaussian",
      #     y_fun = "dgpoix::generate_y_linear",
      #     err_fun = "rnorm",
      #     n = 200, p = 10, betas = "c(rep(1, 5), rep(0, 5))", .err_sd = 1,
      #     data_split = TRUE, train_prop = 0.5, return_support = TRUE
      #   )
      # )
    } else if (ids == "classification-example") {
      dgp_fun_name <- "gaussian_logistic_dgp"
      dgp_name <- "'Example DGP (Uncorrelated Gaussian Logistic DGP)'"
      if (data_split) {
        dgp_args <- list(
          n = 200, p = 10, beta = c(rep(1, 5), rep(0, 5)),
          data_split = TRUE, train_prop = 0.5, return_support = TRUE
        )
      } else {
        dgp_args <- list(
          n = 200, p = 10, beta = c(rep(1, 5), rep(0, 5)),
          data_split = FALSE, return_support = TRUE
        )
      }
      dgp <- function(n, p, beta = 1, data_split = TRUE,
                      train_prop = 0.5, return_support = TRUE) {

        X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
        beta_vec <- matrix(beta, ncol = 1, nrow = p)
        probs <- 1 / (1 + exp(-(X %*% beta_vec)))
        y <- as.factor(
          ifelse(stats::runif(n = n, min = 0, max = 1) > probs, "0", "1")
        )
        if (data_split) {
          train_ids <- sample(1:n, size = round(n * train_prop))
          Xtest <- X[-train_ids, , drop = FALSE]
          ytest <- y[-train_ids]
          X <- X[train_ids, , drop = FALSE]
          y <- y[train_ids]
        } else {
          Xtest <- NULL
          ytest <- NULL
        }
        if (return_support) {
          support <- which(beta_vec != 0)
          out <- list(X = X, y = y, Xtest = Xtest, ytest = ytest, support = support)
        } else {
          out <- list(X = X, y = y, Xtest = Xtest, ytest = ytest)
        }
        return(out)
      }

      # dgp_str <- create_fun_str(
      #   name = "dgp",
      #   fun = "create_dgp",
      #   args = list(
      #     .dgp_fun = "dgpoix::xy_dgp_constructor",
      #     .name = "'Example DGP (Uncorrelated Gaussian Logistic DGP)'",
      #     X_fun = "dgpoix::generate_X_gaussian",
      #     y_fun = "dgpoix::generate_y_logistic",
      #     n = 200, p = 10, betas = "c(rep(1, 5), rep(0, 5))",
      #     data_split = TRUE, train_prop = 0.5, return_support = TRUE
      #   )
      # )
    }

    cat(paste0(dgp_fun_name, " <- ", rlang::expr_text(dgp)), "\n\n")
    dgp_str <- create_fun_str(
      name = "dgp",
      fun = "create_dgp",
      args = c(list(.dgp_fun = dgp_fun_name, .name = dgp_name), dgp_args)
    )
  }

  cat(dgp_str, "\n\n")
  return("dgp")
}

#' Function to create boilerplate code for creating Methods.
#'
#' @keywords internal
use_method_template <- function(ids = NULL) {
  if (is.null(ids)) {
    method_str <- create_fun_str(
      name = "method",
      fun = "create_method",
      args = list(.method_fun = "stop('Add Method function here.')",
                  .name = "stop('Add name of Method here.')",
                  "stop('Add additional arguments (if necessary) to pass to Method here.')")
    )
  } else {
    ids <- match.arg(ids, choices = c("RF", "OLS"))
    if (ids == "RF") {
      method <- function(X, y, Xtest, ytest, support, ...) {

        data <- as.data.frame(X) |>
          cbind(.y = y)

        if (is.factor(y)) {
          mtry <- round(sqrt(ncol(X)))
        } else {
          mtry <- round(ncol(X) / 3)
        }

        fit <- ranger::ranger(data = data,
                              dependent.variable.name = ".y",
                              importance = "impurity",
                              mtry = mtry,
                              num.threads = 1,
                              ...)
        preds <- stats::predict(fit, as.data.frame(Xtest))$predictions
        if (is.factor(y)) {
          k <- nlevels(y)
          prob_preds <- stats::predict(fit, as.data.frame(Xtest),
                                       predict.all = TRUE,
                                       num.threads = 1)$predictions
          prob_preds <- purrr::list_rbind(purrr::map(
            1:nrow(prob_preds),
            function(i) {
              x <- factor(prob_preds[i, ], levels = 1:k)
              tibble::as_tibble_row(c(prop.table(table(x))))
            }
          )) |>
            stats::setNames(levels(y)) |>
            dplyr::select(-1)
        } else {
          prob_preds <- NULL
        }

        p <- ncol(X)
        if (is.null(colnames(X))) {
          features <- 1:p
        } else {
          features <- colnames(X)
        }
        out <- list(
          y = ytest,
          predictions = preds,
          prob_predictions = prob_preds,
          support_df = data.frame(
            feature = features,
            true_support = 1:p %in% support,
            imp = fit$variable.importance,
            selected = fit$variable.importance > mean(fit$variable.importance)
          )
        )
        return(out)
      }
    } else if (ids == "OLS") {
      method <- function(X, y, support, ...) {

        data <- as.data.frame(X) |>
          cbind(.y = y)

        if (is.factor(y)) {
          stop("OLS cannot be applied to a factor response.")
        }

        fit <- stats::lm(.y ~ ., data = data)

        p <- ncol(X)
        if (is.null(colnames(X))) {
          features <- 1:p
        } else {
          features <- colnames(X)
        }
        out <- list(
          support_df = data.frame(
            feature = features,
            true_support = 1:p %in% support,
            pval = broom::tidy(fit)$p.value[-1]  # ignore intercept
          )
        )
        return(out)
      }
    }
    cat(paste0(tolower(ids), "_method <- ", rlang::expr_text(method)), "\n\n")
    method_str <- create_fun_str(
      name = "method",
      fun = "create_method",
      args = list(.method_fun = paste0(tolower(ids), "_method"),
                  .name = paste0("'", ids, "'"))
    )
  }

  cat(method_str, "\n\n")
  return("method")
}

#' Function to create boilerplate code for creating Evaluators.
#'
#' @keywords internal
use_evaluator_template <- function(ids = NULL,
                                   pred_nested_cols, pred_truth_col,
                                   pred_estimate_col, pred_prob_cols,
                                   feature_nested_cols, feature_col,
                                   feature_truth_col, feature_imp_col,
                                   feature_sel_col, feature_pval_col) {
  if (is.null(ids)) {
    eval_str <- create_fun_str(
      name = "eval",
      fun = "create_evaluator",
      args = list(
        .eval_fun = "stop('Add Evaluator function here.')",
        .name = "stop('Add name of Evaluator here.')",
        "stop('Add additional arguments (if necessary) to pass to Evaluator here.')"
      )
    )
    ids <- "eval"
    cat(eval_str, "\n\n")
  } else {
    ids <- match.arg(ids, choices = c("pred_err", "fi", "feature_sel",
                                      "inf_err", "fi_pval"),
                     several.ok = TRUE)
    for (id in ids) {
      if (id == "pred_err") {
        eval_args <- list(
          .eval_fun = "summarize_pred_err", .name = "'Prediction Accuracy'",
          nested_cols = pred_nested_cols, truth_col = pred_truth_col,
          estimate_col = pred_estimate_col, prob_cols = pred_prob_cols
        )
      } else if (id == "fi") {
        eval_args <- list(
          .eval_fun = "summarize_feature_importance",
          .name = "'Feature Importances'",
          nested_cols = feature_nested_cols,
          feature_col = feature_col, imp_col = feature_imp_col
        )
      } else if (id == "feature_sel") {
        eval_args <- list(
          .eval_fun = "summarize_feature_selection_err",
          .name = "'Feature Selection Error'",
          nested_cols = feature_nested_cols, truth_col = feature_truth_col,
          estimate_col = feature_sel_col, imp_col = feature_imp_col
        )
      } else if (id == "inf_err") {
        eval_args <- list(
          .eval_fun = "summarize_testing_err",
          .name = "'Hypothesis Testing Error'",
          nested_cols = feature_nested_cols, truth_col = feature_truth_col,
          pval_col = feature_pval_col
        )
      } else if (id == "fi_pval") {
        eval_args <- list(
          .eval_fun = "summarize_feature_importance",
          .name = "'P-value Summary Statistics'",
          eval_id = "'pval'",
          nested_cols = feature_nested_cols,
          feature_col = feature_col, imp_col = feature_pval_col
        )
      }
      eval_str <- create_fun_str(name = id, fun = "create_evaluator",
                                 args = purrr::compact(eval_args))
      cat(eval_str, "\n\n")
    }
  }

  return(ids)
}

#' Function to create boilerplate code for creating Visualizers
#'
#' @keywords internal
use_visualizer_template <- function(ids = NULL,
                                    pred_nested_cols, pred_truth_col,
                                    pred_prob_cols, feature_nested_cols,
                                    feature_col, feature_truth_col,
                                    feature_imp_col, feature_pval_col) {
  if (is.null(ids)) {
    viz_str <- create_fun_str(
      name = "viz",
      fun = "create_visualizer",
      args = list(
        .viz_fun = "stop('Add Visualizer function here.')",
        .name = "stop('Add name of Visualizer here.')",
        "stop('Add additional arguments (if necessary) to pass to Visualizer here.')"
      )
    )
    ids <- "viz"
    cat(viz_str, "\n\n")
  } else {
    ids <- match.arg(ids,
                     choices = c("pred_err_plot", "roc_plot", "pr_plot",
                                 "fi_plot", "feature_sel_plot",
                                 "feature_roc_plot", "feature_pr_plot",
                                 "inf_err_plot", "inf_roc_plot", "inf_pr_plot",
                                 "reject_prob_plot"),
                     several.ok = TRUE)
    for (id in ids) {
      if (id == "pred_err_plot") {
        viz_args <- list(.viz_fun = "plot_pred_err",
                         .name = "'Prediction Accuracy Plot'",
                         eval_name = "'Prediction Accuracy'")
      } else if (id == "roc_plot") {
        viz_args <- list(.viz_fun = "plot_pred_curve",
                         .name = "'ROC Plot'", curve = "'ROC'",
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = pred_nested_cols,
                                truth_col = pred_truth_col,
                                prob_cols = pred_prob_cols)
                         ))
      } else if (id == "pr_plot") {
        viz_args <- list(.viz_fun = "plot_pred_curve",
                         .name = "'PR Plot'", curve = "'PR'",
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = pred_nested_cols,
                                truth_col = pred_truth_col,
                                prob_cols = pred_prob_cols)
                         ))
      } else if (id == "fi_plot") {
        viz_args <- list(.viz_fun = "plot_feature_importance",
                         .name = "'Feature Importances Plot'",
                         eval_name = "'Feature Importances'",
                         feature_col = feature_col)
      } else if (id == "feature_sel_plot") {
        viz_args <- list(.viz_fun = "plot_feature_selection_err",
                         .name = "'Feature Selection Error Plot'",
                         eval_name = "'Feature Selection Error'")
      } else if (id == "feature_roc_plot") {
        viz_args <- list(.viz_fun = "plot_feature_selection_curve",
                         .name = "'Feature Selection ROC Plot'",
                         curve = "'ROC'",
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = feature_nested_cols,
                                truth_col = feature_truth_col,
                                imp_col = feature_imp_col)
                         ))
      } else if (id == "feature_pr_plot") {
        viz_args <- list(.viz_fun = "plot_feature_selection_curve",
                         .name = "'Feature Selection PR Plot'",
                         curve = "'PR'",
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = feature_nested_cols,
                                truth_col = feature_truth_col,
                                imp_col = feature_imp_col)
                         ))
      } else if (id == "inf_err_plot") {
        viz_args <- list(.viz_fun = "plot_testing_err",
                         .name = "'Hypothesis Testing Error Plot'",
                         eval_name = "'Hypothesis Testing Error'")
      } else if (id == "inf_roc_plot") {
        viz_args <- list(.viz_fun = "plot_testing_curve",
                         .name = "'Feature ROC Plot'",
                         curve = "'ROC'",
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = feature_nested_cols,
                                truth_col = feature_truth_col,
                                pval_col = feature_pval_col)
                         ))
      } else if (id == "inf_pr_plot") {
        viz_args <- list(.viz_fun = "plot_testing_curve",
                         .name = "'Feature Selection PR Plot'",
                         curve = "'PR'",
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = feature_nested_cols,
                                truth_col = feature_truth_col,
                                pval_col = feature_pval_col)
                         ))
      } else if (id == "reject_prob_plot") {
        viz_args <- list(.viz_fun = "plot_reject_prob",
                         .name = "'Rejection Probability Curve'",
                         feature_col = feature_col,
                         eval_fun_options = create_list_args_str(
                           list(nested_cols = feature_nested_cols,
                                pval_col = feature_pval_col)
                         ))
      }
      viz_str <- create_fun_str(name = id, fun = "create_visualizer",
                                args = purrr::compact(viz_args))
      cat(viz_str, "\n\n")
    }
  }

  return(ids)
}

#' Function to create boilerplate code for creating Experiments.
#'
#' @keywords internal
use_experiment_template <- function(name = "Experiment",
                                    dgp_names = NULL,
                                    method_names = NULL,
                                    eval_names = NULL,
                                    viz_names = NULL) {

  experiment_str <- sprintf("experiment <- create_experiment(name = '%s')",
                            name)
  for (dgp_name in dgp_names) {
    experiment_str <- experiment_str |>
      pipe_value(rlang::call2("add_dgp", as.name(dgp_name)))
  }
  for (method_name in method_names) {
    experiment_str <- experiment_str |>
      pipe_value(rlang::call2("add_method", as.name(method_name)))
  }
  for (eval_name in eval_names) {
    experiment_str <- experiment_str |>
      pipe_value(rlang::call2("add_evaluator", as.name(eval_name)))
  }
  for (viz_name in viz_names) {
    experiment_str <- experiment_str |>
      pipe_value(rlang::call2("add_visualizer", as.name(viz_name)))
  }

  cat(experiment_str, "\n\n")
  return(invisible(NULL))
}

#' Function to create boilerplate code for running an Experiment.
#'
#' @keywords internal
use_run_template <- function() {
  run_str <- create_fun_str(
    name = "results",
    fun = "run_experiment",
    args = list(experiment = "experiment",
                n_reps = "stop('Add number of replicates here.')",
                save = TRUE)
  )

  cat(run_str, "\n\n")
  return(invisible(NULL))
}

#' Function to create boilerplate code for creating the documentation template.
#'
#' @keywords internal
use_init_docs_template <- function() {
  cat("init_docs(experiment)  #> fill out documentation before proceeding!\n\n")
}

#' Function to create boilerplate code for creating the Rmd report.
#'
#' @keywords internal
use_render_docs_template <- function() {
  cat("render_docs(experiment)\n\n")
}

#' Function to create boilerplate code for assigning descriptive errors to
#' variables.
#'
#' @keywords internal
use_variable_template <- function(id) {
  id <- match.arg(id,
                  choices = c("nested_pred_cols", "true_pred_col",
                              "est_pred_col", "prob_pred_cols",
                              "nested_feature_cols", "feature_col",
                              "true_feature_col", "feature_imp_col",
                              "feature_sel_col", "pval_col"))
  msg <- dplyr::case_when(
    id == "nested_pred_cols" ~
      "(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.",
    id == "true_pred_col" ~
      "Add name of column in `fit_results` with true responses here.",
    id == "est_pred_col" ~
      "Add name of column in `fit_results` with the predicted responses here.",
    id == "prob_pred_cols" ~
      "Add name of column(s) in `fit_results` with the predicted probabilities here.",
    id == "nested_feature_cols" ~
      "(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.",
    id == "feature_col" ~
      "Add name of column in `fit_results` containing the feature names here.",
    id == "true_feature_col" ~
      "Add name of column in `fit_results` containing the true feature support here.",
    id == "feature_imp_col" ~
      "Add name of column in `fit_results` containing the feature importances here.",
    id == "feature_sel_col" ~
      "(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.",
    id == "pval_col" ~
      "Add name of column in `fit_results` containing the p-values here."
  )
  return(msg)
}
