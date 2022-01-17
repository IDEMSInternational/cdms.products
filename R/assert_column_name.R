assert_column_name <- function(data, column) {
  if (!column %in% names(data)) stop("Column: ", "'", column, "'", " not found in data")
}