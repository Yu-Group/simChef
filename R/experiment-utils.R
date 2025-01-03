#' @keywords internal
get_new_dgp_params <- function(method_params, new_fit_params) {
  # dummies to fix R CMD check note on no visible binding for global variable
  .dgp <- NULL
  .method <- NULL

  # get new dgp parameter combinations given method parameter set
  dgp_params_list <- new_fit_params |>
    dplyr::filter(sapply(.method, identical, method_params)) |>
    dplyr::pull(.dgp)
  return(dgp_params_list)
}


#' @keywords internal
get_new_method_params <- function(dgp_params, new_fit_params) {
  # dummies to fix R CMD check note on no visible binding for global variable
  .dgp <- NULL
  .method <- NULL

  # get new method parameter combinations given dgp parameter set
  method_params_list <- new_fit_params |>
    dplyr::filter(sapply(.dgp, identical, dgp_params)) |>
    dplyr::pull(.method)
  return(method_params_list)
}


#' Get the size of an object, including environments.
#'
#' @param obj The object to measure. Default is the calling environment.
#'
#' @keywords internal
obj_size <- function(obj = rlang::caller_env()) {

  bytes <- "can't determine size because package `lobstr` not installed!"

  # if lobstr is installed, use it to compute the size of `obj`
  if (rlang::is_installed("lobstr")) {
    bytes <- rlang::as_bytes(as.integer(lobstr::obj_size(obj)))
  }

  return(bytes)
}


#' @keywords internal
maybe_progressr <- function(...) {

  if (rlang::is_installed("progressr")) {
    return(progressr::progressor(...))

  } else {
    # return dummy function
    return(function(...) invisible(NULL))
  }
}


#' @keywords internal
maybe_add_debug_data <- function(tbl, debug = FALSE) {
  if (debug) {
    tbl$.pid <- Sys.getpid()
    tbl$.gc <- list(gc())
  }
  invisible(tbl)
}


#' Distribute simulation computation by replicates.
#'
#' @keywords internal
compute_rep <- function(reps,
                        future.globals,
                        future.packages,
                        future.seed,
                        ...) {

  debug <- isTRUE(getOption("simChef.debug", FALSE))

  if (debug) {

    inform(c("future::plan():", utils::capture.output(future::plan())))

    inform("parallel_strategy: reps")

    closure_size <- obj_size()
    inform(c("simulation loop closure size before:",
             as.character(closure_size)))

    inform(c("gc():", utils::capture.output(gc())))

    inform("simulation loop starting...")

  }

  # check whether future will error whenever reference values are exported, a
  # false positive in the case of `data.table`; more info at the following url:
  # https://future.futureverse.org/articles/future-4-non-exportable-objects.html
  err_on_ref <- identical(getOption("future.globals.onReference"), "error")

  if (err_on_ref) {
    # use an environment to share error state, which won't be shared across
    # workers when workers are tied to processes but will stop the worker from
    # continuing to process reps when it first encounters an error
    error_state <- rlang::new_environment()
    error_state[["error"]] <- FALSE

  } else {
    # use a shared reference via `data.table` to share error state so that all
    # workers know when one first encounters an error
    error_state <- data.table::data.table(error = FALSE)
  }

  # progress updates
  total_reps <- length(reps) * length(dgp_params_list) * length(method_params_list)
  p <- maybe_progressr(steps = total_reps,
                       envir = parent.frame())

  vary_param_names <- purrr::map(
    c(dgp_params_list, method_params_list),
    ~ names(.x)
  ) |>
    purrr::reduce(c) |>
    unique()

  results <- future.apply::future_lapply(as.character(reps), function(rep) {

    # make a local binding to error_state
    error_state <- error_state

    ## # debugging
    ## print(
    ##   sprintf(
    ##     "[pid %s] in error state: %s",
    ##     Sys.getpid(), error_state[["error"]]
    ##   )
    ## )

    future_env <- rlang::current_env()
    # withr::defer(clean_up_worker_env("future", env = future_env))
    withr::defer({
      tryCatch(
        warning = identity,
        rm(dgp_res,
           error_state,
           envir = future_env)
      )
      rm(future_env)
      gc()
    })

    save_file <- file.path(
      save_dir, "fit_results", sprintf("fit_result%s.rds", rep)
    )
    if (use_cached && file.exists(save_file) && !save_in_bulk) {
      cached_results <- readRDS(save_file)
    } else {
      cached_results <- NULL
    }

    dgp_res <- purrr::list_rbind(purrr::map(
      dgp_params_list,
      function(dgp_params) {

        dgp_env <- rlang::current_env()
        # withr::defer(clean_up_worker_env("dgp", env = dgp_env))
        withr::defer({
          tryCatch(
            warning = identity,
            rm(method_res,
               data_list,
               dgp_params,
               dgp_name,
               envir = dgp_env)
          )
          rm(dgp_env)
          gc()
        })

        if (error_state[["error"]]) {
          return(NULL)
        }

        if (!is.null(new_fit_params)) {
          # TODO: do this outside of loop
          method_params_list <- get_new_method_params(
            dgp_params, new_fit_params
          )
        }

        dgp_name <- dgp_params$.dgp_name
        dgp_params$.dgp_name <- NULL

        # seed <- list(.Random.seed)

        data_list <- do_call_wrapper(
          dgp_name,
          dgp_list[[dgp_name]]$generate,
          dgp_params,
          verbose,
          # hard-coded dgp fun call for error messages
          call = rlang::call2(paste0(dgp_name, "$dgp_fun(...)"))
        )

        if ("error" %in% class(data_list)) {

          if (err_on_ref) {
            # env
            error_state[["error"]] <- TRUE
          } else {
            # data.table
            data.table::set(error_state, j = "error", value = TRUE)
          }

          return(
            list(.rep = rep,
                 .dgp = dgp_list[[dgp_name]],
                 .dgp_name = dgp_name,
                 .dgp_params = dgp_params,
                 .method = NULL,
                 .method_name = NULL,
                 .method_params = NULL,
                 .method_output = NULL,
                 .err = data_list) |>
              list_to_tibble_row() |>
              maybe_add_debug_data(TRUE)
          )
        }

        method_res <- purrr::list_rbind(purrr::map(
          method_params_list,
          function(method_params) {

            method_env <- rlang::current_env()
            # withr::defer(
            #   clean_up_worker_env("method", env = method_env),
            #   envir = method_env
            # )
            withr::defer({
              tryCatch(
                warning = identity,
                rm(method_params,
                   method_name,
                   param_df,
                   result,
                   envir = method_env)
              )
              rm(method_env)
              gc()
            })

            method_name <- method_params$.method_name

            param_df <- fix_duplicate_param_names(
              dgp_params = c(.dgp_name = dgp_name, dgp_params),
              method_params = method_params,
              duplicate_param_names = duplicate_param_names
            ) |>
              list_to_tibble_row()

            # param_df$.seed <- seed

            method_params$.method_name <- NULL
            method_params$data_list <- data_list
            method_params$.simplify <- FALSE

            if (use_cached && file.exists(save_file) && !save_in_bulk) {
              is_cached <- compare_tibble_rows(
                simplify_tibble(param_df),
                cached_results |>
                  dplyr::select(tidyselect::all_of(colnames(param_df))) |>
                  simplify_tibble(),
                op = "contained_in"
              ) &&
                compare_tibble_rows(
                  simplify_tibble(param_df),
                  cached_fit_params |>
                    dplyr::select(tidyselect::all_of(colnames(param_df))),
                  op = "contained_in"
                )
              if (is_cached) {
                # if (verbose >= 1) {
                #   inform(sprintf("Found cached results for rep=%s for", rep))
                #   inform(str(simplify_tibble(param_df)))
                # }
                return(NULL)
              }
            }

            fit_start_time <- Sys.time()
            result <- do_call_wrapper(
              method_name,
              method_list[[method_name]]$fit,
              method_params,
              verbose,
              # hard-coded method fun call for error messages
              call = rlang::call2(paste0(method_name, "$method_fun(...)"))
            )
            fit_time <- difftime(Sys.time(), fit_start_time, units = "mins")

            if ("error" %in% class(result)) {

              if (err_on_ref) {
                # env
                error_state[["error"]] <- TRUE
              } else {
                # data.table
                data.table::set(error_state, j = "error", value = TRUE)
              }

              method_params$data_list <- NULL

              return(
                list(.rep = rep,
                     .dgp = dgp_list[[dgp_name]],
                     .dgp_name = dgp_name,
                     .dgp_params = dgp_params,
                     .method = method_list[[method_name]],
                     .method_name = method_name,
                     .method_params = method_params,
                     .method_output = NULL,
                     .err = result) |>
                  list_to_tibble_row() |>
                  maybe_add_debug_data(TRUE)
              )
            }

            names_check <- tryCatch(
              check_results_names(
                c(names(result), names(param_df)),
                method_name
              ),
              error = identity
            )

            if ("error" %in% class(names_check)) {

              if (err_on_ref) {
                # env
                error_state[["error"]] <- TRUE
              } else {
                # data.table
                data.table::set(error_state, j = "error", value = TRUE)
              }

              method_params$data_list <- NULL

              return(
                list(.rep = rep,
                     .dgp = dgp_list[[dgp_name]],
                     .dgp_name = dgp_name,
                     .dgp_params = dgp_params,
                     .method = method_list[[method_name]],
                     .method_name = method_name,
                     .method_params = method_params,
                     .method_output = result,
                     .err = names_check) |>
                  list_to_tibble_row() |>
                  maybe_add_debug_data(TRUE)
              )
            }

            result <- result |>
              tibble::add_column(param_df, .before = 1) |>
              tibble::add_column(.rep = rep, .before = 1)

            if (record_time) {
              result$.time_taken <- fit_time
            }

            p("of total reps")

            return(result |> maybe_add_debug_data(debug))

          }
        )) # method_res <- purrr::list_rbind(purrr::map(

        return(method_res)

      }
    )) # dgp_res <- purrr::list_rbind(purrr::map(

    dgp_res <- dgp_res |>
      simplify_tibble(c(vary_param_names, duplicate_param_names))

    if (use_cached && file.exists(save_file) && !save_in_bulk) {
      dgp_res <- get_matching_rows(
        id = cached_fit_params, x = cached_results
      ) |>
        dplyr::bind_rows(dgp_res)
    }

    if (save_per_rep) {
      if (".err" %in% colnames(dgp_res)) {
        saveRDS(
          dgp_res,
          stringr::str_replace(save_file, "\\.rds$", "_error.rds")
        )
      } else {
        saveRDS(dgp_res, save_file)
      }
      dgp_res <- dgp_res |>
        dplyr::select(tidyselect::any_of(unique(c(
          ".rep", vary_param_names, duplicate_param_names,
          ".dgp", ".dgp_name", ".dgp_params",
          ".method", ".method_name", ".method_params",
          ".method_output", ".err", ".pid", ".gc"
        ))))
    }

    return(dgp_res)

  },
  future.globals = future.globals,
  future.packages = future.packages,
  future.seed = future.seed,
  ...) # results <- future.apply::future_replicate(

  results <- dplyr::bind_rows(results)

  if (debug) {

    inform("simulation loop complete")

    # save the future plan in the results attributes
    attr(results, "simChef.debug") <- list(
      future_plan = future::plan()
    )

    if (!is.null(closure_size)) {

      # change in closure size after the loop
      closure_delta <- obj_size() - closure_size
      closure_size <- closure_size + closure_delta

      inform(c("simulation loop closure size after:",
               as.character(closure_size)))

      # add to result's debug attributes
      attr(results, "simChef.debug")[["closure_size"]] <- closure_size
      attr(results, "simChef.debug")[["closure_size_delta"]] <- closure_delta
    }

    inform(c("gc:", utils::capture.output(gc())))
  }

  return(results)

}


#' Distribute simulation computation by DGPs.
#'
#' @keywords internal
compute_dgp <- function(reps,
                        future.globals,
                        future.packages,
                        future.seed,
                        ...) {
  abort("`compute_dgp` not implemented")
}


#' Distribute simulation computation by Methods.
#'
#' @keywords internal
compute_method <- function(reps,
                           future.globals,
                           future.packages,
                           future.seed,
                           ...) {
  abort("`compute_method` not implemented")
}


#' Doubly nested distributed simulation computation nested by DGPs and reps.
#'
#' @keywords internal
compute_dgp_rep <- function(reps,
                            future.globals,
                            future.packages,
                            future.seed,
                            ...) {
  abort("`compute_dgp_rep` not implemented")
}


#' Doubly nested distributed simulation computation nested by Methods and reps.
#'
#' @keywords internal
compute_method_rep <- function(reps,
                               future.globals,
                               future.packages,
                               future.seed,
                               ...) {
  abort("`compute_method_rep` not implemented")
}


#' Doubly nested distributed simulation computation nested by DGPs and Methods.
#'
#' @keywords internal
compute_dgp_method <- function(reps,
                               future.globals,
                               future.packages,
                               future.seed,
                               ...) {
  abort("`compute_dgp_method` not implemented")
}


#' Triply nested distributed simulation computation nested by DGPs, Methods, and
#' reps.
#'
#' @keywords internal
compute_dgp_method_reps <- function(reps,
                                    future.globals,
                                    future.packages,
                                    future.seed,
                                    ...) {
  abort("`compute_dgp_method_reps` not implemented")
}
