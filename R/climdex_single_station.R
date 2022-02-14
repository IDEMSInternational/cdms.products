# factored out code for a multiple indices for a single station.
# Called by climdex().
# Not intended to be used externally.
#' Title
#'
#' @param ci TODO
#' @param freq TODO
#' @param indices TODO
#' @param year TODO
#' @param month TODO
#' @param spells.can.span.years TODO
#' @param gsl.mode TODO
#' @param threshold TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
climdex_single_station <- function(ci, indices, year, month, freq = "annual", 
                                   spells.can.span.years = FALSE, 
                                   gsl.mode = c("GSL", "GSL_first", 
                                                "GSL_max", "GSL_sum"),
                                   threshold = 1) {
  stopifnot(freq %in% c("annual", "monthly"))
  if (freq == "monthly" && missing(month)) stop("month is required for freq = 'monthly'.")
  if (missing(indices)) stop("No indices specified.")
  for (i in seq_along(indices)) {
    vals <- switch(indices[i],
                   "fd" = climdex.pcic::climdex.fd(ci),
                   "su" = climdex.pcic::climdex.su(ci),
                   "id" = climdex.pcic::climdex.id(ci),
                   "tr" = climdex.pcic::climdex.tr(ci),
                   "wsdi" = climdex.pcic::climdex.wsdi(ci, spells.can.span.years = spells.can.span.years),
                   "csdi" = climdex.pcic::climdex.csdi(ci, spells.can.span.years = spells.can.span.years),
                   "gsl" = climdex.pcic::climdex.gsl(ci, gsl.mode = gsl.mode),
                   "txx" = climdex.pcic::climdex.txx(ci, freq = freq),
                   "txn" = climdex.pcic::climdex.txn(ci, freq = freq),
                   "tnx" = climdex.pcic::climdex.tnx(ci, freq = freq),
                   "tnn" = climdex.pcic::climdex.tnn(ci, freq = freq),
                   "tn10p" = climdex.pcic::climdex.tn10p(ci, freq = freq),
                   "tx10p" = climdex.pcic::climdex.tx10p(ci, freq = freq),
                   "tn90p" = climdex.pcic::climdex.tn90p(ci, freq = freq),
                   "tx90p" = climdex.pcic::climdex.tx90p(ci, freq = freq),
                   "dtr" = climdex.pcic::climdex.dtr(ci, freq = freq),
                   "rx1day" = climdex.pcic::climdex.rx1day(ci, freq = freq),
                   "rx5day" = climdex.pcic::climdex.rx5day(ci, freq = freq),
                   "sdii" = climdex.pcic::climdex.sdii(ci),
                   "r10mm" = climdex.pcic::climdex.r10mm(ci),
                   "r20mm" = climdex.pcic::climdex.r20mm(ci),
                   "rnnmm" = climdex.pcic::climdex.rnnmm(ci, threshold = threshold),
                   "cdd" = climdex.pcic::climdex.cdd(ci, spells.can.span.years = spells.can.span.years),
                   "cwd" = climdex.pcic::climdex.cwd(ci, spells.can.span.years = spells.can.span.years),
                   "r95ptot" = climdex.pcic::climdex.r95ptot(ci),
                   "r99ptot" = climdex.pcic::climdex.r99ptot(ci),
                   "prcptot" = climdex.pcic::climdex.prcptot(ci),
                   stop("index name ", indices[i], " not recognised.")
    )
    if (i == 1) {
      if (freq == "annual") {
        df_ind <- data.frame(names(vals), unname(vals))
        names(df_ind) <- c(year, indices[i])
      } else {
        df_ind <- data.frame(stringr::str_split_fixed(string = names(vals), n = 2, pattern = "-"), vals, row.names = NULL)
        names(df_ind) <- c(year, month, indices[i])
        df_ind[[month]] <- as.numeric(df_ind[[month]])
      }
    }
    else {
      df_ind[[indices[i]]] <- unname(vals)
    }
    if (indices[[i]] == "rnnmm") names(df_ind)[ncol(df_ind)] <- paste(indices[[i]], threshold, sep = "_")
  }
  return(df_ind)
}
