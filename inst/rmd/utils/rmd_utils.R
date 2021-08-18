#' Splits up R chunks to enable plotting multiple plots of different
#' sizes and captions within a single written R code chunk in Rmarkdown
#'
#' @param g plot
#' @param i chunk id (should be unique for each plot)
#' @param fig_height height of figure
#' @param fig_width width of figure
#' @param caption figure caption; should be surrounded by two sets of quotes, 
#' e.g., "'This is a valid caption.'"
#' @param add_class vector of css classes to add to object
#' @param other_args other arguments to pass to R code chunk header; should be
#' surrounded by two sets of quotes, e.g., "results='asis'"
#'
subchunkify <- function(g, i, fig_height = 12, fig_width = 10, caption = "''",
                        add_class = NULL, other_args = "") {
  
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  
  if (!identical(other_args, "")) {
    if (!startsWith(other_args, ",")) {
      other_args <- paste0(", ", other_args)
    }
  }
  
  sub_chunk <- paste0("
  `","``{r subchunk_", i, 
                      ", fig.height=", fig_height, ", fig.width=", fig_width, 
                      ", fig.cap=", caption,", echo=FALSE", other_args, "}",
                      "\n(", g_deparsed, ")()",
                      "\n`","``
  ")
  
  if (!is.null(add_class)) {
    cat(sprintf("<div class='%s'>", paste(add_class)))
  }
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
  if (!is.null(add_class)) {
    cat("</div>")
  }
}

#' Reads in markdown file to Rmd 
#' (credit: https://stackoverflow.com/questions/56328581/how-to-read-markdown-code-from-a-file-from-an-r-markdown-document)
#'
#' @param filename path to markdown file to read into Rmd
#'
pasteMd <- function(filename){

  breakFun <- function(x){
    # function to replace empty lines with newline and 
    # put bullet points on new line
    if (nchar(x) == 0) {
      return("\n\n") # double newline to give same space as in the .md-file
    } else if (startsWith(x, "- ")) {
      return(paste0("\n", x))
    } else {
      return(x)
    }
  }

  storelines <- readLines(filename)

  cat(paste0(lapply(storelines, FUN = function(x) breakFun(x)), collapse = ""))

}
