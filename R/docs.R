#' Initialize documentation template for the R Markdown results report.
#'
#' @name init_docs
#' @description Create documentation template (a series of .md files) to
#'   fill out for the R Markdown results report. If the \code{experiment} is
#'   provided, the documentation files can be found in the \code{Experiment}'s
#'   results directory (see \code{Experiment$get_save_dir()}) under docs/.
#'   Otherwise, the documentation files can be found in the specified
#'   \code{save_dir} directory under docs/. The documentation files generated
#'   include objectives.md and .md files corresponding to \code{DGPs},
#'   \code{Methods}, \code{Evaluators}, and \code{Visualizers} in the
#'   \code{Experiment}.
#'
#' @param experiment An \code{Experiment} object. If provided, documentation is
#'   created for all previously saved \code{Experiments} that are found in the
#'   directory given by \code{experiment$get_save_dir()}. If no
#'   \code{Experiments} have been previously saved under this directory, then
#'   the current \code{experiment} is saved to disk and used to create the
#'   documentation template.
#' @param save_dir An optional directory in which to find previously saved
#'   \code{Experiment} objects. Documentation is created for these found
#'   \code{Experiments}. Not used if \code{experiment} is provided.
#'
#' @returns The original \code{Experiment} object if provided. Otherwise,
#'   returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' # create documentation template from an experiment (of class `Experiment`)
#' init_docs(experiment)
#'
#' # or alternatively, create documentation template from a specific directory
#' init_docs(save_dir = experiment$get_save_dir())}
#'
#' @export
init_docs <- function(experiment, save_dir) {
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
  if (identical(descendants, list()) && !missing(experiment)) {
    saveRDS(experiment, file.path(save_dir, "experiment.rds"))
    descendants <- list(experiment)
  }
  if (!identical(descendants, list())) {
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
  }

  if (!missing(experiment)) {
    return(invisible(experiment))
  }
}

#' Initialize documentation template for the R Markdown results report.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `create_doc_template()` was renamed to `init_docs()` to create a more
#' consistent API.
#'
#' @keywords internal
#' @export
create_doc_template <- function(experiment, save_dir) {
  lifecycle::deprecate_warn("0.1.0", "create_doc_template()", "init_docs()")
  init_docs(experiment, save_dir)
}

#' Render an R Markdown file summarizing the results of an \code{Experiment} or
#'   set of \code{Experiments}.
#'
#' @name render_docs
#' @description Knits and/or writes an R Markdown file summarizing the results of
#'   an \code{Experiment} or set of \code{Experiments}. This document may
#'   contain (1) the code corresponding to the \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, \code{Visualizers}, and \code{vary_across} parameters
#'   from the \code{Experiment}, (2) the results of the \code{Evaluators}
#'   (typically tables), and (3) the results of the \code{Visualizers}
#'   (typically figures). Note that \code{render_docs()} will process and
#'   include results from all \code{Experiments} found *under* the root
#'   directory. This root directory is determined via
#'   \code{experiment$get_save_dir()} if \code{experiment} is provided and
#'   \code{save_dir} otherwise.
#'
#' @inheritParams init_docs
#' @inheritParams rmarkdown::render
#' @param write_rmd Logical indicating whether or not to write out the raw
#'   R Markdown file used to generate the results. If \code{TRUE}, both the
#'   raw R Markdown file and the rendered R Markdown output are saved to disk.
#'   If \code{FALSE}, only the rendered R Markdown output is saved. Default is
#'   \code{FALSE}.
#' @param output_file The name of the output file. If using \code{NULL}, then
#'   the output filename will be based on the \code{experiment}'s root results
#'   directory and the name of the \code{experiment}. Note that the filename
#'   may include the path to the output file.
#' @param output_format The R Markdown output format to convert to. Must be an
#'   object of class `rmarkdown_output_format` (e.g.,
#'   [rmarkdown::html_document()], [rmarkdown::pdf_document()],
#'   [rmarkdown::md_document]).
#' @param output_ext File extension for the rendered R Markdown output document.
#'   Only used if  \code{open = TRUE} to open the rendered document. Default is
#'   "auto", which automatically detects the file extension based on
#'   `output_format`.
#' @param title Character string. Title of the report. By default, this will be
#'   the name of the \code{experiment} if \code{experiment} is provided.
#' @param author Character string of author names to display in knitted R
#'   Markdown document.
#' @param show_code Logical indicating whether or not to show code portions in
#'   the output document.
#' @param show_eval Logical indicating whether or not to show the results of
#'   the \code{Evaluators} in the output document.
#' @param show_viz Logical indicating whether or not to show the results of
#'   the \code{Visualizers} in the output document.
#' @param eval_order Vector of \code{Evaluator} names in their desired order for
#'   display. By default, the report will display the \code{Evaluator} results
#'   in the order that they were computed.
#' @param viz_order Vector of \code{Visualizer} names in their desired order for
#'   display. By default, the report will display the \code{Visualizer} results
#'   in the order that they were computed.
#' @param use_icons Logical indicating whether or not to use fontawesome icons.
#' @param open If \code{TRUE}, open the rendered (and raw, if applicable)
#'   R Markdown-generated file using the system's default application.
#' @param verbose Level of verboseness (0, 1, 2) when knitting R Markdown.
#'   Default is 2.
#' @param ... Additional arguments to pass to [rmarkdown::render()]. Useful
#'   for applying a custom R Markdown output theme.
#'
#' @returns The original \code{Experiment} object if provided. Otherwise,
#'   returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' # create basic Rmd from an experiment (of class `Experiment`)
#' render_docs(experiment)
#'
#' # or alternatively, create basic Rmd from a specific directory
#' render_docs(save_dir = experiment$get_save_dir())}
#'
#' @export
render_docs <- function(experiment, save_dir, write_rmd = FALSE,
                        output_file = NULL, output_format = vthemes::vmodern(),
                        output_ext = "auto", title = NULL, author = "",
                        show_code = TRUE, show_eval = TRUE, show_viz = TRUE,
                        eval_order = NULL, viz_order = NULL, use_icons = TRUE,
                        open = TRUE, quiet = TRUE, verbose = 2, ...) {

  if (missing(experiment) && missing(save_dir)) {
    stop("Must provide argument for one of experiment or save_dir.")
  }
  if (!missing(experiment)) {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of simChef::%s",
                         as.character(substitute(experiment)), "Experiment")
      stop(err_msg, call. = FALSE)
    }
    save_dir <- experiment$get_save_dir()
    if (is.null(title)) {
      title <- experiment$name
    }
  } else {
    save_dir <- R.utils::getAbsolutePath(save_dir)
    if (is.null(title)) {
      title <- "Results"
    }
  }

  init_docs(save_dir = save_dir)

  input_file <- system.file(
    "rmd", "results.Rmd", package = utils::packageName()
  )
  if (is.null(output_file)) {
    output_file <- file.path(save_dir, title)
  }

  output_format_type <- rlang::call_name(rlang::enexpr(output_format))
  use_vmodern <- output_format_type == "vmodern"
  if (use_vmodern) {
    if (!rlang::is_installed("htmltools")) {
      stop("Need `htmltools` package in order to run `render_docs(output_format = vthemes::vmodern(), ...)`. Please install via `install.packages('htmltools')`.")
    }
  }

  if (write_rmd) {
    if (!rlang::is_installed("ymlthis")) {
      stop("Need `ymlthis` package in order to run `render_docs(write_rmd = TRUE, ...)`. Please install via `install.packages('ymlthis').`")
    }
    yml_header <- ymlthis::yml() %>%
      ymlthis::yml_title(title) %>%
      ymlthis::yml_author(author) %>%
      ymlthis::yml_output({{output_format}}) %>%
      ymlthis::yml_params(
        sim_path = ymlthis::shiny_text(
          label = "Path to Simulation Experiment Folder:",
          value = save_dir
        )
      )
    template_path <- system.file(
      "rmd", "results_header_template.Rmd", package = utils::packageName()
    )
    ymlthis::use_rmarkdown(
      .yml = yml_header, open_doc = FALSE,
      path = sprintf("%s.Rmd", output_file), template = template_path
    )
  }

  if (use_icons) {
    if (!rlang::is_installed("fontawesome")) {
      use_icons <- FALSE
      warning("fontawesome package is not installed. Setting use_icons to FALSE. Install package via `install.packages('fontawesome')` to use use_icons = TRUE.")
    } else if (identical(output_format_type, "pdf_document")) {
      stop("Cannot output pdf_document when use_icons = TRUE. To output a pdf_document, please set use_icons = FALSE.")
    }
  }
  if (identical(output_ext, "auto")) {
    output_ext <- get_rmd_output_extension(output_format)
  }
  if (write_rmd) {
    output_format <- rmarkdown::html_document()
    output_file_temp <- file.path(
      dirname(output_file), sprintf("%s_temp", title)
    )
    quiet_temp <- TRUE
  } else {
    output_file_temp <- output_file
    quiet_temp <- quiet
  }

  params_list <- list(
    sim_name = title, sim_path = save_dir, author = author,
    write_filename = sprintf("%s.Rmd", output_file),
    show_code = show_code, show_eval = show_eval, show_viz = show_viz,
    eval_order = eval_order, viz_order = viz_order, use_icons = use_icons,
    use_vmodern = use_vmodern, write = write_rmd, verbose = verbose
  )

  rmarkdown::render(
    input = input_file, params = params_list, output_file = output_file_temp,
    output_format = output_format, quiet = quiet_temp, ...
  )

  if (write_rmd) {
    file.remove(sprintf("%s.html", output_file_temp))
    rmarkdown::render(
      input = sprintf("%s.Rmd", output_file), output_file = output_file,
      quiet = quiet, ...
    )
  }

  if (open) {
    if (write_rmd) {
      if (rlang::is_installed("rstudioapi")) {
        if (rstudioapi::isAvailable()) {
          rstudioapi::navigateToFile(sprintf("%s.Rmd", output_file), line = 2)
        }
      }
    }
    if (!is.null(output_ext)) {
      output_file <- stringr::str_replace_all(output_file, " ", "\\\\ ")
      system(paste("open", sprintf("%s.%s", output_file, output_ext)))
    } else {
      warning("Cannot automatically detect output file extension. Please specify `output_ext` directly in function call in order to open file.")
    }
  }

  if (!missing(experiment)) {
    return(invisible(experiment))
  }
}

#' Render an R Markdown file summarizing the results of an \code{Experiment} or
#' set of \code{Experiments}.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `create_rmd()` was renamed to `render_docs()` to create a more consistent
#' API.
#'
#' @inheritParams render_docs
#'
#' @export
create_rmd <- function(experiment, save_dir, write_rmd = FALSE,
                       output_file = NULL, output_format = vthemes::vmodern(),
                       output_ext = "auto", title = NULL, author = "",
                       show_code = TRUE, show_eval = TRUE, show_viz = TRUE,
                       eval_order = NULL, viz_order = NULL, use_icons = TRUE,
                       open = TRUE, quiet = TRUE, verbose = 2, ...) {
  lifecycle::deprecate_warn("0.1.0", "create_rmd()", "render_docs()")
  render_docs(experiment, save_dir, write_rmd, output_file, output_format,
              output_ext, title, author, show_code, show_eval, show_viz,
              eval_order, viz_order, use_icons, open, quiet, verbose, ...)
}

#' Get R Markdown output file extension
#'
#' @description Infer file extension of R Markdown output format
#'
#' @param output_format An rmarkdown output format object (e.g.,
#'   `rmarkdown::html_document()`)
#'
#' @return File extension string (e.g., "html", "pdf", "docx")
#'
#' @keywords internal
get_rmd_output_extension <- function(output_format) {
  if (!is.null(output_format$pandoc$ext)) {
    return(stringr::str_remove(output_format$pandoc$ext, "^\\."))
  } else if (!is.null(output_format$pandoc$to)) {
    if (output_format$pandoc$to == "latex") {
      return("pdf")
    } else {
      return(output_format$pandoc$to)
    }
  } else {
    return(NULL)
  }
}
