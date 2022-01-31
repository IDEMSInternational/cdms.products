# make_factor is intended to be somewhat equivalent to forcats::as_factor() or base::as.factor().
# It provides default behaviour for converting to factor depending on the data type, similar to forcats::as_factor().
# For "character" and "numeric" types make_factor is consistent with forcats::as_factor() in terms of the order of the factor levels.
# It differs from forcats::as_factor() in two main ways:
# 1. It includes an ordered parameter to allow for creating ordered factors, including converting a factor to an ordered factor (and vice versa).
# 2. It works for any data types (e.g. Dates) whereas forcats::as_factor() is limited to "factor", "character", "logical", "numeric".
#    For any other data types, levels are given in order of appearance (the same as for "character").
#    Note that this should be used cautiously for other data types and the default behaviour may not be the most sensible.
# If anything other than this default behaviour is required, use factor().
make_factor <- function(x, ordered = is.ordered(x)) {
  if (is.factor(x)) {
    if (ordered != is.ordered(x)) {
      if (ordered) class(x) <- c("ordered", class(x))
      else class(x) <- class(x)[class(x) != "ordered"]
    }
    x
  } else if (is.numeric(x)) {
    factor(x, ordered = ordered)
  } else if (is.logical(x)) {
    factor(x, levels = c("FALSE", "TRUE"), ordered = ordered)
  } else if (is.character(x)) {
    factor(x, levels = unique(x), ordered = ordered)
  } else {
    factor(x, levels = as.character(unique(x)), ordered = ordered)
  }
}
