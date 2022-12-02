# use_*_template() works

    dgp <- create_dgp(
      dgp_fun = stop('Add DGP function here.'),
      name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      method_fun = stop('Add Method function here.'),
      name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_data <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    
    
    pred_err <- create_evaluator(
      eval_fun = summarize_pred_err,
      name = 'Prediction Accuracy',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      estimate_col = est_pred_col
    ) 
    
    pred_err_plot <- create_visualizer(
      viz_fun = plot_pred_err,
      name = 'Prediction Accuracy Plot',
      evaluator_name = 'Prediction Accuracy'
    ) 
    
    experiment <- create_experiment(name = 'Prediction Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(pred_err) %>% 
      add_visualizer(pred_err_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = stop('Add DGP function here.'),
      name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      method_fun = stop('Add Method function here.'),
      name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_data <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    prob_pred_cols <- stop('Add name of column(s) in `fit_results` with the predicted probabilities here.')
    
    
    pred_err <- create_evaluator(
      eval_fun = summarize_pred_err,
      name = 'Prediction Accuracy',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      estimate_col = est_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    pred_err_plot <- create_visualizer(
      viz_fun = plot_pred_err,
      name = 'Prediction Accuracy Plot',
      evaluator_name = 'Prediction Accuracy'
    ) 
    
    roc_plot <- create_visualizer(
      viz_fun = plot_pred_curve,
      name = 'ROC Plot',
      curve = 'ROC',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    pr_plot <- create_visualizer(
      viz_fun = plot_pred_curve,
      name = 'PR Plot',
      curve = 'PR',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    experiment <- create_experiment(name = 'Prediction Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(pred_err) %>% 
      add_visualizer(pred_err_plot) %>% 
      add_visualizer(roc_plot) %>% 
      add_visualizer(pr_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = stop('Add DGP function here.'),
      name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      method_fun = stop('Add Method function here.'),
      name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_data <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    
    nested_feature_data <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    feature_imp_col <- stop('Add name of column in `fit_results` containing the feature importances here.')
    feature_sel_col <- stop('(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.')
    
    pred_err <- create_evaluator(
      eval_fun = summarize_pred_err,
      name = 'Prediction Accuracy',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      estimate_col = est_pred_col
    ) 
    
    fi <- create_evaluator(
      eval_fun = summarize_feature_importance,
      name = 'Feature Importances',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      eval_fun = summarize_feature_selection_err,
      name = 'Feature Selection Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    pred_err_plot <- create_visualizer(
      viz_fun = plot_pred_err,
      name = 'Prediction Accuracy Plot',
      evaluator_name = 'Prediction Accuracy'
    ) 
    
    fi_plot <- create_visualizer(
      viz_fun = plot_feature_importance,
      name = 'Feature Importances Plot',
      evaluator_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      viz_fun = plot_feature_selection_err,
      name = 'Feature Selection Error Plot',
      evaluator_name = 'Feature Selection Error'
    ) 
    
    feature_roc_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection ROC Plot',
      curve = 'ROC',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_pr_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection PR Plot',
      curve = 'PR',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    experiment <- create_experiment(name = 'Prediction Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(pred_err) %>% 
      add_evaluator(fi) %>% 
      add_evaluator(feature_sel) %>% 
      add_visualizer(pred_err_plot) %>% 
      add_visualizer(fi_plot) %>% 
      add_visualizer(feature_sel_plot) %>% 
      add_visualizer(feature_roc_plot) %>% 
      add_visualizer(feature_pr_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = stop('Add DGP function here.'),
      name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      method_fun = stop('Add Method function here.'),
      name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_data <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    prob_pred_cols <- stop('Add name of column(s) in `fit_results` with the predicted probabilities here.')
    
    nested_feature_data <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    feature_imp_col <- stop('Add name of column in `fit_results` containing the feature importances here.')
    feature_sel_col <- stop('(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.')
    
    pred_err <- create_evaluator(
      eval_fun = summarize_pred_err,
      name = 'Prediction Accuracy',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      estimate_col = est_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    fi <- create_evaluator(
      eval_fun = summarize_feature_importance,
      name = 'Feature Importances',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      eval_fun = summarize_feature_selection_err,
      name = 'Feature Selection Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    pred_err_plot <- create_visualizer(
      viz_fun = plot_pred_err,
      name = 'Prediction Accuracy Plot',
      evaluator_name = 'Prediction Accuracy'
    ) 
    
    roc_plot <- create_visualizer(
      viz_fun = plot_pred_curve,
      name = 'ROC Plot',
      curve = 'ROC',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    pr_plot <- create_visualizer(
      viz_fun = plot_pred_curve,
      name = 'PR Plot',
      curve = 'PR',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    fi_plot <- create_visualizer(
      viz_fun = plot_feature_importance,
      name = 'Feature Importances Plot',
      evaluator_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      viz_fun = plot_feature_selection_err,
      name = 'Feature Selection Error Plot',
      evaluator_name = 'Feature Selection Error'
    ) 
    
    feature_roc_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection ROC Plot',
      curve = 'ROC',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_pr_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection PR Plot',
      curve = 'PR',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    experiment <- create_experiment(name = 'Prediction Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(pred_err) %>% 
      add_evaluator(fi) %>% 
      add_evaluator(feature_sel) %>% 
      add_visualizer(pred_err_plot) %>% 
      add_visualizer(roc_plot) %>% 
      add_visualizer(pr_plot) %>% 
      add_visualizer(fi_plot) %>% 
      add_visualizer(feature_sel_plot) %>% 
      add_visualizer(feature_roc_plot) %>% 
      add_visualizer(feature_pr_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = xy_dgp_constructor,
      name = 'Example DGP (Uncorrelated Gaussian Linear DGP)',
      X_fun = generate_X_gaussian,
      y_fun = generate_y_linear,
      err_fun = rnorm,
      n = 200,
      p = 10,
      betas = c(rep(1, 5), rep(0, 5)),
      .err_sd = 1,
      data_split = TRUE,
      train_prop = 0.5,
      return_support = TRUE
    ) 
    
    rf_method <-function (X, y, Xtest, ytest, support, ...) 
    {
        data <- as.data.frame(X) %>% cbind(.y = y)
        if (is.factor(y)) {
            mtry <- round(sqrt(ncol(X)))
        }
        else {
            mtry <- round(ncol(X)/3)
        }
        fit <- ranger::ranger(data = data, dependent.variable.name = ".y", 
            importance = "impurity", mtry = mtry, num.threads = 1, 
            ...)
        preds <- stats::predict(fit, as.data.frame(Xtest))$predictions
        if (is.factor(y)) {
            k <- nlevels(y)
            prob_preds <- stats::predict(fit, as.data.frame(Xtest), 
                predict.all = TRUE, num.threads = 1)$predictions
            prob_preds <- purrr::map_dfr(1:nrow(prob_preds), function(i) {
                x <- factor(prob_preds[i, ], levels = 1:k)
                c(prop.table(table(x)))
            }) %>% stats::setNames(levels(y)) %>% dplyr::select(-1)
        }
        else {
            prob_preds <- NULL
        }
        p <- ncol(X)
        if (is.null(colnames(X))) {
            features <- 1:p
        }
        else {
            features <- colnames(X)
        }
        out <- list(y = y, predictions = preds, prob_predictions = prob_preds, 
            support_df = data.frame(feature = features, true_support = 1:p %in% 
                support, imp = fit$variable.importance, selected = fit$variable.importance > 
                mean(fit$variable.importance)))
        return(out)
    } 
    
    method <- create_method(
      method_fun = rf_method,
      name = 'RF'
    ) 
    
    nested_pred_data <- c('y', 'predictions', 'prob_predictions')  # prediction results columns to be unnested
    true_pred_col <- 'y'  # true response column
    est_pred_col <- 'predictions'  # predicted response column
    
    nested_feature_data <- 'support_df'  # feature importance columns to be unnested
    feature_col <- 'feature'  # feature names column
    true_feature_col <- 'true_support'  # true feature support column
    feature_imp_col <- 'imp'  # feature importance column
    feature_sel_col <- 'selected'  # estimated feature support column
    
    pred_err <- create_evaluator(
      eval_fun = summarize_pred_err,
      name = 'Prediction Accuracy',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      estimate_col = est_pred_col
    ) 
    
    fi <- create_evaluator(
      eval_fun = summarize_feature_importance,
      name = 'Feature Importances',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      eval_fun = summarize_feature_selection_err,
      name = 'Feature Selection Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    pred_err_plot <- create_visualizer(
      viz_fun = plot_pred_err,
      name = 'Prediction Accuracy Plot',
      evaluator_name = 'Prediction Accuracy'
    ) 
    
    fi_plot <- create_visualizer(
      viz_fun = plot_feature_importance,
      name = 'Feature Importances Plot',
      evaluator_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      viz_fun = plot_feature_selection_err,
      name = 'Feature Selection Error Plot',
      evaluator_name = 'Feature Selection Error'
    ) 
    
    feature_roc_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection ROC Plot',
      curve = 'ROC',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_pr_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection PR Plot',
      curve = 'PR',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    experiment <- create_experiment(name = 'Prediction Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(pred_err) %>% 
      add_evaluator(fi) %>% 
      add_evaluator(feature_sel) %>% 
      add_visualizer(pred_err_plot) %>% 
      add_visualizer(fi_plot) %>% 
      add_visualizer(feature_sel_plot) %>% 
      add_visualizer(feature_roc_plot) %>% 
      add_visualizer(feature_pr_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = xy_dgp_constructor,
      name = 'Example DGP (Uncorrelated Gaussian Logistic DGP)',
      X_fun = generate_X_gaussian,
      y_fun = generate_y_logistic,
      n = 200,
      p = 10,
      betas = c(rep(1, 5), rep(0, 5)),
      data_split = TRUE,
      train_prop = 0.5,
      return_support = TRUE
    ) 
    
    rf_method <-function (X, y, Xtest, ytest, support, ...) 
    {
        data <- as.data.frame(X) %>% cbind(.y = y)
        if (is.factor(y)) {
            mtry <- round(sqrt(ncol(X)))
        }
        else {
            mtry <- round(ncol(X)/3)
        }
        fit <- ranger::ranger(data = data, dependent.variable.name = ".y", 
            importance = "impurity", mtry = mtry, num.threads = 1, 
            ...)
        preds <- stats::predict(fit, as.data.frame(Xtest))$predictions
        if (is.factor(y)) {
            k <- nlevels(y)
            prob_preds <- stats::predict(fit, as.data.frame(Xtest), 
                predict.all = TRUE, num.threads = 1)$predictions
            prob_preds <- purrr::map_dfr(1:nrow(prob_preds), function(i) {
                x <- factor(prob_preds[i, ], levels = 1:k)
                c(prop.table(table(x)))
            }) %>% stats::setNames(levels(y)) %>% dplyr::select(-1)
        }
        else {
            prob_preds <- NULL
        }
        p <- ncol(X)
        if (is.null(colnames(X))) {
            features <- 1:p
        }
        else {
            features <- colnames(X)
        }
        out <- list(y = y, predictions = preds, prob_predictions = prob_preds, 
            support_df = data.frame(feature = features, true_support = 1:p %in% 
                support, imp = fit$variable.importance, selected = fit$variable.importance > 
                mean(fit$variable.importance)))
        return(out)
    } 
    
    method <- create_method(
      method_fun = rf_method,
      name = 'RF'
    ) 
    
    nested_pred_data <- c('y', 'predictions', 'prob_predictions')  # prediction results columns to be unnested
    true_pred_col <- 'y'  # true response column
    est_pred_col <- 'predictions'  # predicted response column
    prob_pred_cols <- '1'  # predicted probability columns
    
    nested_feature_data <- 'support_df'  # feature importance columns to be unnested
    feature_col <- 'feature'  # feature names column
    true_feature_col <- 'true_support'  # true feature support column
    feature_imp_col <- 'imp'  # feature importance column
    feature_sel_col <- 'selected'  # estimated feature support column
    
    pred_err <- create_evaluator(
      eval_fun = summarize_pred_err,
      name = 'Prediction Accuracy',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      estimate_col = est_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    fi <- create_evaluator(
      eval_fun = summarize_feature_importance,
      name = 'Feature Importances',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      eval_fun = summarize_feature_selection_err,
      name = 'Feature Selection Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    pred_err_plot <- create_visualizer(
      viz_fun = plot_pred_err,
      name = 'Prediction Accuracy Plot',
      evaluator_name = 'Prediction Accuracy'
    ) 
    
    roc_plot <- create_visualizer(
      viz_fun = plot_pred_curve,
      name = 'ROC Plot',
      curve = 'ROC',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    pr_plot <- create_visualizer(
      viz_fun = plot_pred_curve,
      name = 'PR Plot',
      curve = 'PR',
      nested_data = nested_pred_data,
      truth_col = true_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    fi_plot <- create_visualizer(
      viz_fun = plot_feature_importance,
      name = 'Feature Importances Plot',
      evaluator_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      viz_fun = plot_feature_selection_err,
      name = 'Feature Selection Error Plot',
      evaluator_name = 'Feature Selection Error'
    ) 
    
    feature_roc_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection ROC Plot',
      curve = 'ROC',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_pr_plot <- create_visualizer(
      viz_fun = plot_feature_selection_curve,
      name = 'Feature Selection PR Plot',
      curve = 'PR',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      imp_col = feature_imp_col
    ) 
    
    experiment <- create_experiment(name = 'Prediction Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(pred_err) %>% 
      add_evaluator(fi) %>% 
      add_evaluator(feature_sel) %>% 
      add_visualizer(pred_err_plot) %>% 
      add_visualizer(roc_plot) %>% 
      add_visualizer(pr_plot) %>% 
      add_visualizer(fi_plot) %>% 
      add_visualizer(feature_sel_plot) %>% 
      add_visualizer(feature_roc_plot) %>% 
      add_visualizer(feature_pr_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = stop('Add DGP function here.'),
      name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      method_fun = stop('Add Method function here.'),
      name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_feature_data <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    feature_imp_col <- stop('Add name of column in `fit_results` containing the feature importances here.')
    feature_sel_col <- stop('(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.')
    
    fi <- create_evaluator(
      eval_fun = summarize_feature_importance,
      name = 'Feature Importances',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      eval_fun = summarize_feature_selection_err,
      name = 'Feature Selection Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    fi_plot <- create_visualizer(
      viz_fun = plot_feature_importance,
      name = 'Feature Importances Plot',
      evaluator_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      viz_fun = plot_feature_selection_err,
      name = 'Feature Selection Error Plot',
      evaluator_name = 'Feature Selection Error'
    ) 
    
    experiment <- create_experiment(name = 'Feature Selection Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(fi) %>% 
      add_evaluator(feature_sel) %>% 
      add_visualizer(fi_plot) %>% 
      add_visualizer(feature_sel_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = xy_dgp_constructor,
      name = 'Example DGP (Uncorrelated Gaussian Linear DGP)',
      X_fun = generate_X_gaussian,
      y_fun = generate_y_linear,
      err_fun = rnorm,
      n = 200,
      p = 10,
      betas = c(rep(1, 5), rep(0, 5)),
      .err_sd = 1,
      data_split = TRUE,
      train_prop = 0.5,
      return_support = TRUE
    ) 
    
    rf_method <-function (X, y, Xtest, ytest, support, ...) 
    {
        data <- as.data.frame(X) %>% cbind(.y = y)
        if (is.factor(y)) {
            mtry <- round(sqrt(ncol(X)))
        }
        else {
            mtry <- round(ncol(X)/3)
        }
        fit <- ranger::ranger(data = data, dependent.variable.name = ".y", 
            importance = "impurity", mtry = mtry, num.threads = 1, 
            ...)
        preds <- stats::predict(fit, as.data.frame(Xtest))$predictions
        if (is.factor(y)) {
            k <- nlevels(y)
            prob_preds <- stats::predict(fit, as.data.frame(Xtest), 
                predict.all = TRUE, num.threads = 1)$predictions
            prob_preds <- purrr::map_dfr(1:nrow(prob_preds), function(i) {
                x <- factor(prob_preds[i, ], levels = 1:k)
                c(prop.table(table(x)))
            }) %>% stats::setNames(levels(y)) %>% dplyr::select(-1)
        }
        else {
            prob_preds <- NULL
        }
        p <- ncol(X)
        if (is.null(colnames(X))) {
            features <- 1:p
        }
        else {
            features <- colnames(X)
        }
        out <- list(y = y, predictions = preds, prob_predictions = prob_preds, 
            support_df = data.frame(feature = features, true_support = 1:p %in% 
                support, imp = fit$variable.importance, selected = fit$variable.importance > 
                mean(fit$variable.importance)))
        return(out)
    } 
    
    method <- create_method(
      method_fun = rf_method,
      name = 'RF'
    ) 
    
    nested_feature_data <- 'support_df'  # feature importance columns to be unnested
    feature_col <- 'feature'  # feature names column
    true_feature_col <- 'true_support'  # true feature support column
    feature_imp_col <- 'imp'  # feature importance column
    feature_sel_col <- 'selected'  # estimated feature support column
    
    fi <- create_evaluator(
      eval_fun = summarize_feature_importance,
      name = 'Feature Importances',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      eval_fun = summarize_feature_selection_err,
      name = 'Feature Selection Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    fi_plot <- create_visualizer(
      viz_fun = plot_feature_importance,
      name = 'Feature Importances Plot',
      evaluator_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      viz_fun = plot_feature_selection_err,
      name = 'Feature Selection Error Plot',
      evaluator_name = 'Feature Selection Error'
    ) 
    
    experiment <- create_experiment(name = 'Feature Selection Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(fi) %>% 
      add_evaluator(feature_sel) %>% 
      add_visualizer(fi_plot) %>% 
      add_visualizer(feature_sel_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = stop('Add DGP function here.'),
      name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      method_fun = stop('Add Method function here.'),
      name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_feature_data <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    pval_col <- stop('Add name of column in `fit_results` containing the p-values here.')
    
    inf_err <- create_evaluator(
      eval_fun = summarize_testing_err,
      name = 'Hypothesis Testing Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    fi_pval <- create_evaluator(
      eval_fun = summarize_feature_importance,
      eval_id = 'pval',
      name = 'P-value Summary Statistics',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = pval_col
    ) 
    
    inf_err_plot <- create_visualizer(
      viz_fun = plot_testing_err,
      name = 'Hypothesis Testing Error Plot',
      evaluator_name = 'Hypothesis Testing Error'
    ) 
    
    inf_roc_plot <- create_visualizer(
      viz_fun = plot_testing_curve,
      name = 'Feature ROC Plot',
      curve = 'ROC',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    inf_pr_plot <- create_visualizer(
      viz_fun = plot_testing_curve,
      name = 'Feature Selection PR Plot',
      curve = 'PR',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    reject_prob_plot <- create_visualizer(
      viz_fun = plot_reject_prob,
      name = 'Rejection Probability Curve',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      pval_col = pval_col
    ) 
    
    experiment <- create_experiment(name = 'Inference Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(inf_err) %>% 
      add_evaluator(fi_pval) %>% 
      add_visualizer(inf_err_plot) %>% 
      add_visualizer(inf_roc_plot) %>% 
      add_visualizer(inf_pr_plot) %>% 
      add_visualizer(reject_prob_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

---

    dgp <- create_dgp(
      dgp_fun = xy_dgp_constructor,
      name = 'Example DGP (Uncorrelated Gaussian Linear DGP)',
      X_fun = generate_X_gaussian,
      y_fun = generate_y_linear,
      err_fun = rnorm,
      n = 200,
      p = 10,
      betas = c(rep(1, 5), rep(0, 5)),
      .err_sd = 1,
      data_split = TRUE,
      train_prop = 0.5,
      return_support = TRUE
    ) 
    
    ols_method <-function (X, y, Xtest, ytest, support, ...) 
    {
        data <- as.data.frame(X) %>% cbind(.y = y)
        if (is.factor(y)) {
            stop("OLS cannot be applied to a factor response.")
        }
        fit <- stats::lm(.y ~ ., data = data)
        p <- ncol(X)
        if (is.null(colnames(X))) {
            features <- 1:p
        }
        else {
            features <- colnames(X)
        }
        out <- list(support_df = data.frame(feature = features, true_support = 1:p %in% 
            support, pval = broom::tidy(fit)$p.value[-1]))
        return(out)
    } 
    
    method <- create_method(
      method_fun = ols_method,
      name = 'OLS'
    ) 
    
    nested_feature_data <- 'support_df'  # feature importance columns to be unnested
    feature_col <- 'feature'  # feature names column
    true_feature_col <- 'true_support'  # true feature support column
    pval_col <- 'pval'  # p-values column
    
    inf_err <- create_evaluator(
      eval_fun = summarize_testing_err,
      name = 'Hypothesis Testing Error',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    fi_pval <- create_evaluator(
      eval_fun = summarize_feature_importance,
      eval_id = 'pval',
      name = 'P-value Summary Statistics',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      imp_col = pval_col
    ) 
    
    inf_err_plot <- create_visualizer(
      viz_fun = plot_testing_err,
      name = 'Hypothesis Testing Error Plot',
      evaluator_name = 'Hypothesis Testing Error'
    ) 
    
    inf_roc_plot <- create_visualizer(
      viz_fun = plot_testing_curve,
      name = 'Feature ROC Plot',
      curve = 'ROC',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    inf_pr_plot <- create_visualizer(
      viz_fun = plot_testing_curve,
      name = 'Feature Selection PR Plot',
      curve = 'PR',
      nested_data = nested_feature_data,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    reject_prob_plot <- create_visualizer(
      viz_fun = plot_reject_prob,
      name = 'Rejection Probability Curve',
      nested_data = nested_feature_data,
      feature_col = feature_col,
      pval_col = pval_col
    ) 
    
    experiment <- create_experiment(name = 'Inference Experiment') %>% 
      add_dgp(dgp) %>% 
      add_method(method) %>% 
      add_evaluator(inf_err) %>% 
      add_evaluator(fi_pval) %>% 
      add_visualizer(inf_err_plot) %>% 
      add_visualizer(inf_roc_plot) %>% 
      add_visualizer(inf_pr_plot) %>% 
      add_visualizer(reject_prob_plot) 
    
    init_docs(experiment)  #> fill out documentation before proceeding!
    
    results <- run_experiment(
      experiment = experiment,
      n_reps = stop('Add number of replicates here.'),
      save = TRUE
    ) 
    
    render_docs(experiment)
    

