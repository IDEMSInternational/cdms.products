elements_wider <- function(data, name, value) {
  tidyr::pivot_wider(data, names_from = tidyselect::all_of(name), 
                     values_from = tidyselect::all_of(value))
}
