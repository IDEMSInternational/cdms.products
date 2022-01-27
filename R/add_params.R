add_params <- function(fn_exp, add_params = list()) {
  for (i in seq_along(add_params)) {
    fn_exp <- paste0(fn_exp, ", ", names(add_params)[i], " = ", add_params[[i]])
  }
  fn_exp
}

null_to_string <- function(x) ifelse(is.null(x), "NULL", x)