assert_column_names <- function(data, columns) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(columns)
  if (!all(columns %in% names(data))) {
    if (length(columns) > 1) stop("Not all columns: ", paste(columns, collapse = ", "), " found in data")
    else stop("Not all columns: ", "'", paste(columns, collapse = ", "), "'", " found in data")
  }
}