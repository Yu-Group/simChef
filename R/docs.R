#' Initialize documentation template for the R Markdown results report.
#'
#' @name init_docs
#' @description Create documentation template (a series of .md files) to
#'   fill out for the R Markdown results report. If the `experiment` is
#'   provided, the documentation files can be found in the `Experiment`'s
#'   results directory (see `Experiment$get_save_dir()`) under docs/.
#'   Otherwise, the documentation files can be found in the specified
#'   `save_dir` directory under docs/. The documentation files generated
#'   include objectives.md and .md files corresponding to `DGPs`,
#'   `Methods`, `Evaluators`, and `Visualizers` in the
#'   `Experiment`.
#'
#' @param experiment An `Experiment` object. If provided, documentation is
#'   created for all previously saved `Experiments` that are found in the
#'   directory given by `experiment$get_save_dir()`. If no
#'   `Experiments` have been previously saved under this directory, then
#'   the current `experiment` is saved to disk and used to create the
#'   documentation template.
#' @param save_dir An optional directory in which to find previously saved
#'   `Experiment` objects. Documentation is created for these found
#'   `Experiments`. Not used if `experiment` is provided.
#'
#' @returns The original `Experiment` object if provided. Otherwise,
#'   returns `NULL`.
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
    abort("Must provide argument for one of experiment or save_dir.")
  }
  if (!missing(experiment)) {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of simChef::%s",
                         as.character(substitute(experiment)), "Experiment")
      abort(err_msg, call. = FALSE)
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
                              ~names(.x[[paste0("get_", field, "s")]]())) |>
        purrr::reduce(c) |>
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
#' @inheritParams init_docs
#'
#' @examples
#' \dontrun{
#' # create documentation template from an experiment (of class `Experiment`)
#' create_doc_template(experiment)
#'
#' # or alternatively, create documentation template from a specific directory
#' create_doc_template(save_dir = experiment$get_save_dir())}
#'
#' @keywords internal
#' @export
create_doc_template <- function(experiment, save_dir) {
  lifecycle::deprecate_warn("0.1.0", "create_doc_template()", "init_docs()")
  init_docs(experiment, save_dir)
}

#' Render an R Markdown file summarizing the results of an `Experiment` or
#'   set of `Experiments`.
#'
#' @name render_docs
#' @description Knits and/or writes an R Markdown file summarizing the results of
#'   an `Experiment` or set of `Experiments`. This document may
#'   contain (1) the code corresponding to the `DGPs`, `Methods`,
#'   `Evaluators`, `Visualizers`, and `vary_across` parameters
#'   from the `Experiment`, (2) the results of the `Evaluators`
#'   (typically tables), and (3) the results of the `Visualizers`
#'   (typically figures). Note that `render_docs()` will process and
#'   include results from all `Experiments` found *under* the root
#'   directory. This root directory is determined via
#'   `experiment$get_save_dir()` if `experiment` is provided and
#'   `save_dir` otherwise.
#'
#' @inheritParams init_docs
#' @inheritParams rmarkdown::render
#' @param write_rmd Logical indicating whether or not to write out the raw
#'   R Markdown file used to generate the results. If `TRUE`, both the
#'   raw R Markdown file and the rendered R Markdown output are saved to disk.
#'   If `FALSE`, only the rendered R Markdown output is saved. Default is
#'   `FALSE`.
#' @param output_file The name of the output file. If using `NULL`, then
#'   the output filename will be based on the `experiment`'s root results
#'   directory and the name of the `experiment`. Note that the filename
#'   may include the path to the output file.
#' @param output_format The R Markdown output format to convert to. Must be an
#'   object of class `rmarkdown_output_format` (e.g.,
#'   [rmarkdown::html_document()], [rmarkdown::pdf_document()],
#'   [rmarkdown::md_document]).
#' @param title Character string. Title of the report. By default, this will be
#'   the name of the `experiment` if `experiment` is provided.
#' @param author Character string of author names to display in knitted R
#'   Markdown document.
#' @param show_code Logical indicating whether or not to show code portions in
#'   the output document.
#' @param show_eval Logical indicating whether or not to show the results of
#'   the `Evaluators` in the output document.
#' @param show_viz Logical indicating whether or not to show the results of
#'   the `Visualizers` in the output document.
#' @param eval_order Vector of `Evaluator` names in their desired order for
#'   display. By default, the report will display the `Evaluator` results
#'   in the order that they were computed.
#' @param viz_order Vector of `Visualizer` names in their desired order for
#'   display. By default, the report will display the `Visualizer` results
#'   in the order that they were computed.
#' @param eval_cache File extension of the cached evaluator results to read in.
#'   Typically `".rds"` or `"none"`, but can be any file extension where
#'   evaluator results are stored as `eval_results.ext` and can be read in
#'   using `data.table::fread(eval_results.ext)`. If `"none"`, evaluator results
#'   are computed using the experiment via [evaluate_experiment()].
#' @param viz_cache File extension of the cached visualizer results to read in.
#'   Typically `".rds"` or `"none"`, but can be any (image) file extension
#'   (e.g., `"png"`, `"jpg"`) where the visualizer results have been previously
#'   stored as separate `{visualizer_name}.ext` images (typically using
#'   [export_visualizers()]). If `"none"`, visualizer results are computed using
#'   the experiment via [visualize_experiment()].
#' @param viz_interactive Logical, indicating whether or not to display
#'   plot visualizers interactively in the R Markdown document using
#'   [plotly::ggplotly()]. Default is `FALSE`. Can also specify a character
#'   vector of `Visualizer` names to display interactively. Note that only
#'   the visualizers that can be coerced into an interactive plot using
#'   [plotly::ggplotly()] will be displayed interactively.
#' @param use_icons Logical indicating whether or not to use fontawesome icons.
#' @param verbose Level of verboseness (0, 1, 2) when knitting R Markdown.
#'   Default is 2.
#' @param ... Additional arguments to pass to [rmarkdown::render()]. Useful
#'   for applying a custom R Markdown output theme.
#'
#' @returns The original `Experiment` object if provided. Otherwise,
#'   returns `NULL`.
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
                        title = NULL, author = "",
                        show_code = TRUE, show_eval = TRUE, show_viz = TRUE,
                        eval_order = NULL, viz_order = NULL,
                        eval_cache = ".rds", viz_cache = ".rds",
                        viz_interactive = FALSE,
                        use_icons = TRUE,
                        quiet = TRUE, verbose = 2, ...) {

  if (missing(experiment) && missing(save_dir)) {
    abort("Must provide argument for one of experiment or save_dir.")
  }
  if (!missing(experiment)) {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of simChef::%s",
                         as.character(substitute(experiment)), "Experiment")
      abort(err_msg, call. = FALSE)
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
  if (viz_interactive) {
    rlang::check_installed("plotly", reason = "to use viz_interactive = TRUE")
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

  if (write_rmd) {
    rlang::check_installed("ymlthis",
                           reason = "to run `render_docs(write_rmd = TRUE, ...)`")
    yml_header <- ymlthis::yml() |>
      ymlthis::yml_title(title) |>
      ymlthis::yml_author(author) |>
      ymlthis::yml_output({{output_format}}) |>
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
      warn("fontawesome package is not installed. Setting use_icons to FALSE. Install package via `install.packages('fontawesome')` to use use_icons = TRUE.")
    } else if (identical(output_format_type, "pdf_document")) {
      abort("Cannot output pdf_document when use_icons = TRUE. To output a pdf_document, please set use_icons = FALSE.")
    }
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
    eval_order = eval_order, viz_order = viz_order,
    eval_cache = eval_cache, viz_cache = viz_cache,
    viz_interactive = viz_interactive, use_icons = use_icons,
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

  if (write_rmd) {
    if (rlang::is_installed("rstudioapi")) {
      if (rstudioapi::isAvailable()) {
        rstudioapi::navigateToFile(sprintf("%s.Rmd", output_file), line = 2)
      }
    }
  }

  if (verbose > 1) {
    inform(sprintf("Rendered document can be found at %s", output_file))
  }

  if (!missing(experiment)) {
    return(invisible(experiment))
  }
}

#' Render an R Markdown file summarizing the results of an `Experiment` or
#' set of `Experiments`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `create_rmd()` was renamed to `render_docs()` to create a more consistent
#' API.
#'
#' @inheritParams render_docs
#'
#' @examples
#' \dontrun{
#' # create basic Rmd from an experiment (of class `Experiment`)
#' create_rmd(experiment)
#'
#' # or alternatively, create basic Rmd from a specific directory
#' create_rmd(save_dir = experiment$get_save_dir())}
#'
#' @export
create_rmd <- function(experiment, save_dir, write_rmd = FALSE,
                       output_file = NULL, output_format = vthemes::vmodern(),
                       title = NULL, author = "",
                       show_code = TRUE, show_eval = TRUE, show_viz = TRUE,
                       eval_order = NULL, viz_order = NULL, use_icons = TRUE,
                       quiet = TRUE, verbose = 2, ...) {
  lifecycle::deprecate_warn("0.1.0", "create_rmd()", "render_docs()")
  render_docs(experiment, save_dir, write_rmd, output_file, output_format,
              title, author, show_code, show_eval, show_viz,
              eval_order, viz_order, use_icons, quiet, verbose, ...)
}
