#' Title
#'
#' @param x TODO
#' @param n TODO
#' @param period TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
fourier_series <- function(x, n, period) {
  p2 <- "2 * pi"
  h <-  seq_len(n)
  paste0("sin(", x, " * ", h, " * ", p2, " / ", period, ")", " + ", 
         "cos(", x, " * ", h, " * ", p2, " / ", period, ")", 
         collapse = " + ")
}
