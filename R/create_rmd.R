#' Create documentation template for the R Markdown results report.
#'
#' @name create_doc_template
#' @description Create documentation template (a series of .md files) to
#'   fill out for the R Markdown results report. If the \code{experiment} is
#'   provided, the documentation files can be found in the \code{Experiment}'s 
#'   results directory (see \code{Experiment$get_save_dir()}) under docs/. 
#'   Otherwise, the documentation files can be found in the specified 
#'   \code{save_dir} directory under docs/.
#'
#' @param experiment An \code{Experiment} object. If provided,
#'   \code{experiment$get_save_dir()} is used to find previously saved results.
#'   If not provided, then \code{save_dir} is used instead.
#' @param save_dir An optional directory in which to find saved results. Not
#'   used if \code{experiment} provided.
#'
#' @returns The original \code{Experiment} object if provided. Otherwise, 
#'   returns \code{NULL}.
#'
#' @export
create_doc_template <- function(experiment, save_dir) {
  if (missing(experiment) && missing(save_dir)) {
    stop("Must provide argument for one of experiment or save_dir.")
  }
  if (!missing(experiment)) {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of simChef::%s",
                         as.character(substitute(experiment)), "Experiment")
      stop(err_msg, call.=FALSE)
    }
    save_dir <- experiment$get_save_dir()
  }
  
  if (!dir.exists(file.path(save_dir, "docs"))) {
    dir.create(file.path(save_dir, "docs"), recursive = TRUE)
  }
  
  if (!file.exists(file.path(save_dir, "docs", "objectives.md"))) {
    fname <- file.path(save_dir, "docs", "objectives.md")
    utils::write.csv(NULL, file = fname, quote = F)
  }
  
  descendants <- purrr::map(
    list.dirs(save_dir), function(d) {
      if (file.exists(file.path(d, "experiment.rds"))) {
        return(readRDS(file.path(d, "experiment.rds")))
      } else {
        return(NULL)
      }
    }
  )
  descendants[sapply(descendants, is.null)] <- NULL
  
  fields <- c("dgp", "method", "evaluator", "visualizer")
  for (field in fields) {
    obj_names <- purrr::map(descendants,
                            ~names(.x[[paste0("get_", field, "s")]]())) %>%
      purrr::reduce(c) %>%
      unique()
    for (obj_name in obj_names) {
      fname <- file.path(save_dir, "docs", paste0(field, "s"),
                         paste0(obj_name, ".md"))
      if (!file.exists(fname)) {
        if (!dir.exists(dirname(fname))) {
          dir.create(dirname(fname), recursive = TRUE)
        }
        utils::write.csv(NULL, file = fname, quote = F)
      }
    }
  }
  
  if (!missing(experiment)) {
    return(invisible(experiment))
  }
}

#' Create an R Markdown file summarizing the results of an \code{Experiment} or
#'   set of \code{Experiments}.
#'
#' @name create_rmd
#' @description Knits an R Markdown file summarizing the results of an
#'   \code{Experiment} or set of \code{Experiments}. Outputs an R 
#'   Markdown-generated html file. If \code{experiment} is provided, the results
#'   are saved in the \code{Experiment}'s root results directory (see
#'   \code{Experiment$get_save_dir()}). Otherwise, the root results directory is
#'   taken to be that specified by \code{save_dir}. Note that 
#'   \code{create_rmd()} will process and include results from all 
#'   \code{Experiments} found *under* the root directory.
#'
#' @inheritParams create_doc_template
#' @param open If \code{TRUE}, open the R Markdown-generated html file in a
#'   web browser.
#' @param title Character string. Title of the report. By default, this will be
#'   the name of the \code{experiment} if \code{experiment} is provided.
#' @param author Character string of author names to display in knitted R
#'   Markdown document.
#' @param verbose Level of verboseness (0, 1, 2) when knitting R Markdown.
#'   Default is 2.
#' @param quiet Default is \code{TRUE}. See [rmarkdown::render()] for 
#'   details.
#' @param pretty Logical. Specifies whether or not to use pretty R Markdown
#'   results template or more barebones R Markdown results template. Default
#'   \code{TRUE} uses the pretty template. Set to \code{FALSE} to start from
#'   the barebones template, which can be helpful when using your own custom
#'   R Markdown theme.
#' @param ... Additional arguments to pass to [rmarkdown::render()]. Useful
#'   for applying a custom R Markdown output theme.
#'
#' @returns The original \code{Experiment} object if provided. Otherwise, 
#'   returns \code{NULL}.
#'
#' @export
create_rmd <- function(experiment, save_dir, open = TRUE, title = NULL,
                       author = "", verbose = 2, quiet = TRUE, pretty = TRUE, 
                       ...) {
  if (missing(experiment) && missing(save_dir)) {
    stop("Must provide argument for one of experiment or save_dir.")
  }
  if (!missing(experiment)) {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of simChef::%s",
                         as.character(substitute(experiment)), "Experiment")
      stop(err_msg, call.=FALSE)
    }
    save_dir <- experiment$get_save_dir()
    if (is.null(title)) {
      title <- experiment$name
    }
  } else {
    if (is.null(title)) {
      title <- "Results"
    }
  }
  
  create_doc_template(save_dir = save_dir)
  
  if (pretty) {
    input_fname <- system.file("rmd", "results_pretty.Rmd",
                               package = utils::packageName())
  } else {
    input_fname <- system.file("rmd", "results.Rmd",
                               package = utils::packageName())
  }
  output_fname <- file.path(save_dir, paste0(title, ".html"))
  params_list <- list(sim_name = title, sim_path = save_dir,
                      author = author, verbose = verbose)
  rmarkdown::render(input = input_fname, params = params_list,
                    output_file = output_fname, quiet = quiet, ...)
  output_fname <- stringr::str_replace_all(output_fname, " ", "\\\\ ")
  if (open) {
    system(paste("open", output_fname))
  }
  
  if (!missing(experiment)) {
    return(invisible(experiment))
  }
}