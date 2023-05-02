# use_*_template() works

    dgp <- create_dgp(
      .dgp_fun = stop('Add DGP function here.'),
      .name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      .method_fun = stop('Add Method function here.'),
      .name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_cols <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    
    
    pred_err <- create_evaluator(
      .eval_fun = summarize_pred_err,
      .name = 'Prediction Accuracy',
      nested_cols = nested_pred_cols,
      truth_col = true_pred_col,
      estimate_col = est_pred_col
    ) 
    
    pred_err_plot <- create_visualizer(
      .viz_fun = plot_pred_err,
      .name = 'Prediction Accuracy Plot',
      eval_name = 'Prediction Accuracy'
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
      .dgp_fun = stop('Add DGP function here.'),
      .name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      .method_fun = stop('Add Method function here.'),
      .name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_cols <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    prob_pred_cols <- stop('Add name of column(s) in `fit_results` with the predicted probabilities here.')
    
    
    pred_err <- create_evaluator(
      .eval_fun = summarize_pred_err,
      .name = 'Prediction Accuracy',
      nested_cols = nested_pred_cols,
      truth_col = true_pred_col,
      estimate_col = est_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    pred_err_plot <- create_visualizer(
      .viz_fun = plot_pred_err,
      .name = 'Prediction Accuracy Plot',
      eval_name = 'Prediction Accuracy'
    ) 
    
    roc_plot <- create_visualizer(
      .viz_fun = plot_pred_curve,
      .name = 'ROC Plot',
      curve = 'ROC',
      eval_fun_options = list(
        nested_cols = nested_pred_cols,
        truth_col = true_pred_col,
        prob_cols = prob_pred_cols
      )
    ) 
    
    pr_plot <- create_visualizer(
      .viz_fun = plot_pred_curve,
      .name = 'PR Plot',
      curve = 'PR',
      eval_fun_options = list(
        nested_cols = nested_pred_cols,
        truth_col = true_pred_col,
        prob_cols = prob_pred_cols
      )
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
      .dgp_fun = stop('Add DGP function here.'),
      .name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      .method_fun = stop('Add Method function here.'),
      .name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_cols <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    
    nested_feature_cols <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    feature_imp_col <- stop('Add name of column in `fit_results` containing the feature importances here.')
    feature_sel_col <- stop('(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.')
    
    pred_err <- create_evaluator(
      .eval_fun = summarize_pred_err,
      .name = 'Prediction Accuracy',
      nested_cols = nested_pred_cols,
      truth_col = true_pred_col,
      estimate_col = est_pred_col
    ) 
    
    fi <- create_evaluator(
      .eval_fun = summarize_feature_importance,
      .name = 'Feature Importances',
      nested_cols = nested_feature_cols,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      .eval_fun = summarize_feature_selection_err,
      .name = 'Feature Selection Error',
      nested_cols = nested_feature_cols,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    pred_err_plot <- create_visualizer(
      .viz_fun = plot_pred_err,
      .name = 'Prediction Accuracy Plot',
      eval_name = 'Prediction Accuracy'
    ) 
    
    fi_plot <- create_visualizer(
      .viz_fun = plot_feature_importance,
      .name = 'Feature Importances Plot',
      eval_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_err,
      .name = 'Feature Selection Error Plot',
      eval_name = 'Feature Selection Error'
    ) 
    
    feature_roc_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_curve,
      .name = 'Feature Selection ROC Plot',
      curve = 'ROC',
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        truth_col = true_feature_col,
        imp_col = feature_imp_col
      )
    ) 
    
    feature_pr_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_curve,
      .name = 'Feature Selection PR Plot',
      curve = 'PR',
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        truth_col = true_feature_col,
        imp_col = feature_imp_col
      )
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
      .dgp_fun = stop('Add DGP function here.'),
      .name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      .method_fun = stop('Add Method function here.'),
      .name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_pred_cols <- stop('(Optional) Add name of column in `fit_results` with prediction result columns to be unnested.')
    true_pred_col <- stop('Add name of column in `fit_results` with true responses here.')
    est_pred_col <- stop('Add name of column in `fit_results` with the predicted responses here.')
    prob_pred_cols <- stop('Add name of column(s) in `fit_results` with the predicted probabilities here.')
    
    nested_feature_cols <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    feature_imp_col <- stop('Add name of column in `fit_results` containing the feature importances here.')
    feature_sel_col <- stop('(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.')
    
    pred_err <- create_evaluator(
      .eval_fun = summarize_pred_err,
      .name = 'Prediction Accuracy',
      nested_cols = nested_pred_cols,
      truth_col = true_pred_col,
      estimate_col = est_pred_col,
      prob_cols = prob_pred_cols
    ) 
    
    fi <- create_evaluator(
      .eval_fun = summarize_feature_importance,
      .name = 'Feature Importances',
      nested_cols = nested_feature_cols,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      .eval_fun = summarize_feature_selection_err,
      .name = 'Feature Selection Error',
      nested_cols = nested_feature_cols,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    pred_err_plot <- create_visualizer(
      .viz_fun = plot_pred_err,
      .name = 'Prediction Accuracy Plot',
      eval_name = 'Prediction Accuracy'
    ) 
    
    roc_plot <- create_visualizer(
      .viz_fun = plot_pred_curve,
      .name = 'ROC Plot',
      curve = 'ROC',
      eval_fun_options = list(
        nested_cols = nested_pred_cols,
        truth_col = true_pred_col,
        prob_cols = prob_pred_cols
      )
    ) 
    
    pr_plot <- create_visualizer(
      .viz_fun = plot_pred_curve,
      .name = 'PR Plot',
      curve = 'PR',
      eval_fun_options = list(
        nested_cols = nested_pred_cols,
        truth_col = true_pred_col,
        prob_cols = prob_pred_cols
      )
    ) 
    
    fi_plot <- create_visualizer(
      .viz_fun = plot_feature_importance,
      .name = 'Feature Importances Plot',
      eval_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_err,
      .name = 'Feature Selection Error Plot',
      eval_name = 'Feature Selection Error'
    ) 
    
    feature_roc_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_curve,
      .name = 'Feature Selection ROC Plot',
      curve = 'ROC',
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        truth_col = true_feature_col,
        imp_col = feature_imp_col
      )
    ) 
    
    feature_pr_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_curve,
      .name = 'Feature Selection PR Plot',
      curve = 'PR',
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        truth_col = true_feature_col,
        imp_col = feature_imp_col
      )
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
      .dgp_fun = stop('Add DGP function here.'),
      .name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      .method_fun = stop('Add Method function here.'),
      .name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_feature_cols <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    feature_imp_col <- stop('Add name of column in `fit_results` containing the feature importances here.')
    feature_sel_col <- stop('(Optional) Add name of column in `fit_results` containing the (estimated) selected features here.')
    
    fi <- create_evaluator(
      .eval_fun = summarize_feature_importance,
      .name = 'Feature Importances',
      nested_cols = nested_feature_cols,
      feature_col = feature_col,
      imp_col = feature_imp_col
    ) 
    
    feature_sel <- create_evaluator(
      .eval_fun = summarize_feature_selection_err,
      .name = 'Feature Selection Error',
      nested_cols = nested_feature_cols,
      truth_col = true_feature_col,
      estimate_col = feature_sel_col,
      imp_col = feature_imp_col
    ) 
    
    fi_plot <- create_visualizer(
      .viz_fun = plot_feature_importance,
      .name = 'Feature Importances Plot',
      eval_name = 'Feature Importances',
      feature_col = feature_col
    ) 
    
    feature_sel_plot <- create_visualizer(
      .viz_fun = plot_feature_selection_err,
      .name = 'Feature Selection Error Plot',
      eval_name = 'Feature Selection Error'
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
      .dgp_fun = stop('Add DGP function here.'),
      .name = stop('Add name of DGP here.'),
      stop('Add additional arguments (if necessary) to pass to DGP here.')
    ) 
    
    method <- create_method(
      .method_fun = stop('Add Method function here.'),
      .name = stop('Add name of Method here.'),
      stop('Add additional arguments (if necessary) to pass to Method here.')
    ) 
    
    nested_feature_cols <- stop('(Optional) Add name of column in `fit_results` with feature importance columns to be unnested here.')
    feature_col <- stop('Add name of column in `fit_results` containing the feature names here.')
    true_feature_col <- stop('Add name of column in `fit_results` containing the true feature support here.')
    pval_col <- stop('Add name of column in `fit_results` containing the p-values here.')
    
    inf_err <- create_evaluator(
      .eval_fun = summarize_testing_err,
      .name = 'Hypothesis Testing Error',
      nested_cols = nested_feature_cols,
      truth_col = true_feature_col,
      pval_col = pval_col
    ) 
    
    fi_pval <- create_evaluator(
      .eval_fun = summarize_feature_importance,
      .name = 'P-value Summary Statistics',
      eval_id = 'pval',
      nested_cols = nested_feature_cols,
      feature_col = feature_col,
      imp_col = pval_col
    ) 
    
    inf_err_plot <- create_visualizer(
      .viz_fun = plot_testing_err,
      .name = 'Hypothesis Testing Error Plot',
      eval_name = 'Hypothesis Testing Error'
    ) 
    
    inf_roc_plot <- create_visualizer(
      .viz_fun = plot_testing_curve,
      .name = 'Feature ROC Plot',
      curve = 'ROC',
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        truth_col = true_feature_col,
        pval_col = pval_col
      )
    ) 
    
    inf_pr_plot <- create_visualizer(
      .viz_fun = plot_testing_curve,
      .name = 'Feature Selection PR Plot',
      curve = 'PR',
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        truth_col = true_feature_col,
        pval_col = pval_col
      )
    ) 
    
    reject_prob_plot <- create_visualizer(
      .viz_fun = plot_reject_prob,
      .name = 'Rejection Probability Curve',
      feature_col = feature_col,
      eval_fun_options = list(
        nested_cols = nested_feature_cols,
        pval_col = pval_col
      )
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
    

