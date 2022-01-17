assert_column_names <- function(data, columns) {
  if (!all(columns %in% names(data))) {
    if (length(columns) > 1) stop("Not all columns: ", paste(columns, collapse = ", "), " found in data")
    else stop("Not all columns: ", "'", paste(columns, collapse = ", "), "'", " found in data")
  }
}