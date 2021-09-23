get_args <- function(user_args, default_args) {
  arg_list <- list()
  for (arg_name in unique(c(names(user_args), names(default_args)))) {
    if (arg_name %in% names(user_args)) {
      arg_list[[arg_name]] <- user_args[[arg_name]]
    } else {
      arg_list[[arg_name]] <- default_args[[arg_name]]
    }
  }
  return(arg_list)
}