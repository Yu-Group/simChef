#' Read Markdown file into R Markdown.
#' 
#' @description Read contents of Markdown file into R Markdown.
#'
#' @param filename filepath of markdown file to read into R Markdown
#'
#' @references Adapted from \url{https://stackoverflow.com/questions/56328581/how-to-read-markdown-code-from-a-file-from-an-r-markdown-document}.
#' @keywords internal
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
