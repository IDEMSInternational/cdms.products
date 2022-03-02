#' Export CLIMAT messages file(s) from daily data
#'
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station_id TODO
#' @param year \code{character(1)} The name of the year column in \code{data}.
#' @param month \code{character(1)} The name of the month column in \code{data}. 
#' @param mean_pressure_station TODO
#' @param mean_pressure_reduced TODO
#' @param mean_temp TODO
#' @param mean_max_temp TODO
#' @param mean_min_temp TODO
#' @param mean_vapour_pressure TODO
#' @param total_precip TODO
#' @param total_sunshine TODO
#' @param folder TODO
#' @param total_snow_depth Daily total snow depth in cm column name
#' @param max_ws Daily maximum wind speed in m/s column name
#' @param min_h_vis Daily minimum horizontal visibility in m column name
#'
#' @export
#'
#' @examples # TODO
export_climat_messages <- function(data, date_time, station_id, year = NULL, 
                                   month = NULL, 
                                   mean_pressure_station = NULL, 
                                   mean_pressure_reduced = NULL, 
                                   mean_temp = NULL, 
                                   mean_max_temp = NULL, 
                                   mean_min_temp = NULL, 
                                   mean_vapour_pressure = NULL, 
                                   total_precip = NULL, 
                                   total_sunshine = NULL, 
                                   total_snow_depth = NULL,
                                   max_ws = NULL,
                                   min_h_vis = NULL,
                                   folder = getwd()) {
  
  checkmate::assert_data_frame(data)
  checkmate::assert_string(date_time)
  checkmate::assert(checkmate::check_date(data[[date_time]]), 
                    checkmate::check_posixct(data[[date_time]]))
  
  checkmate::assert_string(mean_pressure_station, null.ok = TRUE)
  checkmate::assert_string(mean_pressure_reduced, null.ok = TRUE)
  checkmate::assert_string(mean_temp, null.ok = TRUE)
  checkmate::assert_string(mean_max_temp, null.ok = TRUE)
  checkmate::assert_string(mean_min_temp, null.ok = TRUE)
  checkmate::assert_string(mean_vapour_pressure, null.ok = TRUE)
  checkmate::assert_string(total_precip, null.ok = TRUE)
  checkmate::assert_string(total_sunshine, null.ok = TRUE)
  
  mean_cols <- c(mean_pressure_station, mean_pressure_reduced, mean_temp,
                 mean_max_temp, mean_min_temp, mean_vapour_pressure)
  sum_cols <- c(total_precip, total_sunshine)
  all_cols <- c(mean_cols, sum_cols)
  if (length(all_cols) == 0) stop("No elements specified.", 
                                  "At least one element must be specified.")
  
  checkmate::assert_string(station_id)
  assert_column_names(data, station_id)
  if (any(nchar(data[[station_id]]) != 5)) {
    stop("Values in station_id column must be five digits/characters")
  }
  checkmate::assert_string(year, null.ok = TRUE)
  if (is.null(year)) {
    year <- "year"
    data[[year]] <- lubridate::year(data[[date_time]])
  } else {
    assert_column_names(data, year)
    if (any(nchar(data[[year]]) != 4)) {
      stop("Values in year column must be four digits/characters")
    }
  }
  checkmate::assert_string(month, null.ok = TRUE)
  if (is.null(month)) {
    year <- "month"
    data[[month]] <- lubridate::month(data[[date_time]])
  } else assert_column_names(data, month)
  
  #TODO This is copied from prepare_geoclim_month and should be separate function
  unique_months <- unique(data[[month]])
  if (all(as.character(unique_months) %in% as.character(1:12))) {
    data[[month]] <- factor(data[[month]], 
                            levels = 1:12, 
                            labels = month_name_english)
  } else if (all(unique_months %in% month_abb_english)) {
    data[[month]] <- factor(data[[month]], levels = month_abb_english)
  } else if (all(unique_months %in% month_name_english)) {
    data[[month]] <- factor(data[[month]], levels = month_name_english)
  } else if (all(unique_months %in% month.abb)) {
    data[[month]] <- factor(data[[month]], levels = month.abb)
  } else if (all(unique_months %in% month.name)) {
    data[[month]] <- factor(data[[month]], levels = month.name)
  } else {
    if (!is.factor(data[[month]]) || nlevels(data[[month]]) != 12) {
      stop("Values in month column are not recognised. ",
           "Values must be full or abbreviated month names ", 
           "or numbers 1 to 12 or a factor with 12 levels.")
    }
  }
  month_levels <- levels(data[[month]])
  unused_months <- setdiff(levels(data[[month]]), unique(data[[month]]))
  year_levels <- unique(data[[year]])
  station_levels <- unique(data[[station_id]])
  figs <- function(x, n, na = "/") {
    ifelse(is.na(x), strrep(na, n), sprintf(paste0("%0", n, "d"), x))
  }
  monthly_data <- list()
  data_grp <- data %>%
    dplyr::group_by(.data[[station_id]], .data[[year]], .data[[month]])
  
  data_monthly_na <- data_grp %>%
    dplyr::summarise(dplyr::across(tidyselect::all_of(all_cols),
                                   list(na = ~sum(is.na(.x))),
                                   .names = "{.fn}_{.col}"))
  monthly_data[["monthly_na"]] <- data_monthly_na
  na_names <- paste0("na_", all_cols)
  names(na_names) <- all_cols
  
  if (length(mean_cols) > 0) {
    data_monthly_means <- data_grp %>%
      dplyr::summarise(dplyr::across(tidyselect::all_of(mean_cols),
                                     list(mean = mean),
                                     na.rm = TRUE, .names = "{.fn}_{.col}"))
    monthly_data[["monthly_means"]] <- data_monthly_means
    mean_names <- paste0("mean_", mean_cols)
    names(mean_names) <- mean_cols
  }
  if (!is.null(mean_temp)) {
    data_monthly_sd <- data_grp %>%
      dplyr::summarise(
        sd_mean_temp = stats::sd(.data[[mean_temp]], na.rm = TRUE))
    monthly_data[["monthly_sd"]] <- data_monthly_sd
    sd_mean_temp_col <- "sd_mean_temp"
  }
  if (!is.null(mean_max_temp)) {
    data_monthly_tmax <- data_grp %>%
      dplyr::summarise(
        t25 = figs(sum(.data[[mean_max_temp]] >= 25), 2),
        t30 = figs(sum(.data[[mean_max_temp]] >= 30), 2),
        t35 = figs(sum(.data[[mean_max_temp]] >= 35), 2),
        t40 = figs(sum(.data[[mean_max_temp]] >= 40), 2),
        tx0 = figs(sum(.data[[mean_max_temp]] < 0), 2)
        )
    monthly_data[["monthly_tmax"]] <- data_monthly_tmax
  }
  if (!is.null(mean_min_temp)) {
    data_monthly_tmin <- data_grp %>%
      dplyr::summarise(
        tn0 = figs(sum(.data[[mean_min_temp]] < 0), 2)
        )
    monthly_data[["monthly_tmin"]] <- data_monthly_tmin
  }
  if (!is.null(total_precip)) {
    data_monthly_precip <- data_grp %>%
      dplyr::summarise(sum_total_precip = sum(.data[[total_precip]]),
                       r01 = figs(sum(.data[[total_precip]] >= 1), 2),
                       r05 = figs(sum(.data[[total_precip]] >= 5), 2),
                       r10 = figs(sum(.data[[total_precip]] >= 10), 2),
                       r50 = figs(sum(.data[[total_precip]] >= 50), 2),
                       r100 = figs(sum(.data[[total_precip]] >= 100), 2),
                       r150 = figs(sum(.data[[total_precip]] >= 150), 2)
      )
    monthly_data[["monthly_precip"]] <- data_monthly_precip
    sum_precip_col <- "sum_total_precip"
  }
  if (!is.null(total_sunshine)) {
    data_monthly_sunshine <- data_grp %>%
      dplyr::summarise(sum_total_sunshine = sum(.data[[total_sunshine]]))
    monthly_data[["monthly_sunshine"]] <- data_monthly_sunshine
    sum_sunshine_col <- "sum_total_sunshine"
  }
  if (!is.null(total_snow_depth)) {
    data_monthly_snow <- data_grp %>%
      dplyr::summarise(s00 = figs(sum(.data[[total_snow_depth]] > 0), 2),
                       s01 = figs(sum(.data[[total_snow_depth]] > 1), 2),
                       s10 = figs(sum(.data[[total_snow_depth]] > 10), 2),
                       s50 = figs(sum(.data[[total_snow_depth]] > 50), 2)
      )
    monthly_data[["monthly_snow"]] <- data_monthly_snow
  }
  if (!is.null(max_ws)) {
    data_monthly_ws <- data_grp %>%
      dplyr::summarise(f10 = figs(sum(.data[[max_ws]] > 10), 2),
                       f20 = figs(sum(.data[[max_ws]] > 20), 2),
                       f30 = figs(sum(.data[[max_ws]] > 30), 2)
      )
    monthly_data[["monthly_ws"]] <- data_monthly_ws
  }
  if (!is.null(min_h_vis)) {
    data_monthly_vis <- data_grp %>%
      dplyr::summarise(v1 = figs(sum(.data[[min_h_vis]] < 50), 2),
                       v2 = figs(sum(.data[[min_h_vis]] < 100), 2),
                       v3 = figs(sum(.data[[min_h_vis]] < 1000), 2)
      )
    monthly_data[["monthly_vis"]] <- data_monthly_vis
  }
  data_monthly <- 
    Reduce(function(x, y) dplyr::left_join(x, y, 
                                           by = c(station_id, year, month)),
           monthly_data
    )
  
  if (!is.null(mean_pressure_station)) {
    mean_col <- mean_names[[mean_pressure_station]]
    na_col <- na_names[[mean_pressure_station]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        mean_mean_pressure_station = round(.data[[mean_col]], 1),
        mean_mean_pressure_station = 
          ifelse(.data$mean_mean_pressure_station > 1000,
                 .data$mean_mean_pressure_station - 1000,
                 .data$mean_mean_pressure_station),
        mean_mean_pressure_station = 
          sprintf("%04d", .data$mean_mean_pressure_station * 10),
        na_mean_pressure_station = sprintf("%02d", .data[[na_col]])
      )
  }
  if (!is.null(mean_pressure_reduced)) {
    mean_col <- mean_names[[mean_pressure_reduced]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        mean_mean_pressure_reduced = round(.data[[mean_col]], 1),
        mean_mean_pressure_reduced = 
          ifelse(.data$mean_mean_pressure_reduced > 1000,
                 .data$mean_mean_pressure_reduced - 1000,
                 .data$mean_mean_pressure_reduced),
        mean_mean_pressure_reduced = 
          sprintf("%04d", .data$mean_mean_pressure_reduced * 10)
      )
  }
  if (!is.null(mean_temp)) {
    mean_col <- mean_names[[mean_temp]]
    na_col <- na_names[[mean_temp]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        mean_mean_temp = round(.data[[mean_col]], 1),
        mean_mean_temp_i <- ifelse(.data$mean_mean_temp >= 0, 0, 1),
        mean_mean_temp = sprintf("%03d", .data$mean_mean_temp * 10),
        sd_mean_temp = round(.data[[sd_mean_temp_col]], 1),
        sd_mean_temp = sprintf("%03d", .data$sd_mean_temp * 10),
        na_mean_temp = sprintf("%02d", .data[[na_col]])
      )
  }
  if (!is.null(mean_max_temp) && !is.null(mean_min_temp)) {
    mean_tmax_col <- mean_names[[mean_max_temp]]
    na_tmax_col <- na_names[[mean_max_temp]]
    mean_tmin_col <- mean_names[[mean_min_temp]]
    na_tmin_col <- na_names[[mean_min_temp]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        mean_mean_max_temp = round(.data[[mean_tmax_col]], 1),
        mean_mean_max_temp_i = ifelse(.data$mean_mean_max_temp >= 0, 0, 1),
        mean_mean_max_temp = sprintf("%03d", .data$mean_mean_max_temp * 10),
        na_mean_max_temp = ifelse(.data[[na_tmax_col]] > 9, 
                                  9, .data[[na_tmax_col]]),
        mean_mean_min_temp = round(.data[[mean_tmin_col]], 1),
        mean_mean_min_temp_i = ifelse(.data$mean_mean_min_temp >= 0, 0, 1),
        mean_mean_min_temp = sprintf("%03d", .data$mean_mean_min_temp * 10),
        na_mean_min_temp = ifelse(.data[[na_tmin_col]] > 9, 
                                  9, .data[[na_tmin_col]])
      )
  }
  if (!is.null(mean_vapour_pressure)) {
    mean_vapour_col <- mean_names[[mean_max_temp]]
    na_vapour_col <- na_names[[mean_max_temp]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        mean_mean_vapour_pressure = round(.data[[mean_vapour_col]], 1),
        mean_mean_vapour_pressure = 
          sprintf("%03d", .data$mean_mean_vapour_pressure * 10),
        na_mean_vapour_pressure = 
          sprintf("%02d", .data[[na_vapour_col]])
      )
  }
  if (!is.null(total_precip)) {
    na_precip_col <- na_names[[total_precip]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        sum_total_precip = 
          ifelse(.data[[sum_precip_col]] > 0 & .data[[sum_precip_col]] < 1,
                 -1, .data[[sum_precip_col]]),
        sum_total_precip = round(.data$sum_total_precip),
        sum_total_precip = ifelse(.data$sum_total_precip >= 8899,
                 8899, .data$sum_total_precip),
        sum_total_precip = ifelse(.data[[sum_precip_col]] == -1, 
                                  9999, .data[[sum_precip_col]]),
        sum_total_precip = sprintf("%04d", .data$sum_total_precip),
        na_total_precip = sprintf("%02d", .data[[na_precip_col]])
      )
  }
  if (!is.null(total_sunshine)) {
    na_sunshine_col <- na_names[[total_sunshine]]
    data_monthly <- data_monthly %>%
      dplyr::mutate(
        sum_total_sunshine = round(.data[[sum_sunshine_col]]),
        na_total_sunshine = sprintf("%02d", .data[[na_sunshine_col]])
      )
  }
  
  for(i in seq_len(nrow(data_monthly))) {
    s <- data_monthly[[station_id]][i]
    y <- data_monthly[[year]][i]
    jjj <- substr(y, 2, 4)
    m <- data_monthly[[month]][i]
    mm <- which(m == month_levels)
    mm <- sprintf("%02d", mm)
    lines <- c()
    
    # Section 0
    sec0 <- paste0("CLIMAT", " ", mm, jjj, " ", s)
    lines <- append(lines, sec0)
    
    # Section 1
    sec1 <- "111"
    grp_8 <- "8"
    grp_9 <- "9"
    if (!is.null(mean_pressure_station) && 
        !is.na(data_monthly$mean_mean_pressure_station[i])) {
      sec1 <- append(sec1,
                     paste0(1, data_monthly$mean_mean_pressure_station[i]))
      grp_8 <- paste0(grp_8, data_monthly$na_mean_pressure_station[i])
    } else grp_8 <- paste0(grp_8, "00")
    if (!is.null(mean_pressure_reduced) && 
        !is.na(data_monthly$mean_mean_pressure_reduced[i])) {
      sec1 <- append(sec1,
                      paste0(2, data_monthly$mean_mean_pressure_reduced[i]))
    }
    if (!is.null(mean_temp) && 
        !is.na(data_monthly$mean_mean_temp[i])) {
      sec1 <- append(sec1,
                      paste0(3,
                             data_monthly$mean_mean_temp_i[i],
                             data_monthly$mean_mean_temp[i],
                             data_monthly$sd_mean_temp[i]))
      grp_8 <- paste0(grp_8, data_monthly$na_mean_temp[i])
    } else grp_8 <- paste0(grp_8, "00")
    if (!is.null(mean_max_temp) &&
        !is.null(mean_min_temp) &&
        !is.na(data_monthly$mean_mean_max_temp[i]) &&
        !is.na(data_monthly$mean_mean_min_temp[i])) {
      sec1 <- append(sec1,
                      paste0(4,
                             data_monthly$mean_mean_max_temp_i[i],
                             data_monthly$mean_mean_max_temp[i],
                             data_monthly$mean_mean_min_temp_i[i],
                             data_monthly$mean_mean_min_temp[i]))
      grp_8 <- paste0(grp_8, data_monthly$na_mean_max_temp[i])
      grp_8 <- paste0(grp_8, data_monthly$na_mean_min_temp[i])
    } else grp_8 <- paste0(grp_8, "00")
    if (!is.null(mean_vapour_pressure) && 
        !is.na(data_monthly$mean_mean_vapour_pressure[i])) {
      sec1 <- append(sec1,
                      paste0(5,
                             data_monthly$mean_mean_vapour_pressure[i]))
      grp_9 <- paste0(grp_9, data_monthly$na_mean_vapour_pressure[i])
    } else grp_9 <- paste0(grp_9, "00")
    if (!is.null(total_precip) && 
        !is.na(data_monthly$sum_total_precip[i])) {
      sec1 <- append(sec1,
                      paste0(6,
                             data_monthly$sum_total_precip[i],
                             # TODO Implement relative position in 30 year climatology
                             "/",
                             data_monthly$r01[i]))
      grp_9 <- paste0(grp_9, data_monthly$na_total_precip[i])
    } else grp_9 <- paste0(grp_9, "00")
    if (!is.null(total_sunshine) && 
        !is.na(data_monthly$sum_total_sunshine[i])) {
      sec1 <- append(sec1,
                      paste0(7,
                             data_monthly$sum_total_sunshine[i],
                             # TODO Implement relative position in 30 year climatology
                             "///"))
      grp_9 <- paste0(grp_9, data_monthly$na_total_sunshine[i])
    } else grp_9 <- paste0(grp_9, "00")
    grp_9 <- paste0(grp_9, "=")
    sec1 <- append(sec1, grp_8)
    sec1 <- append(sec1, grp_9)
    lines <- append(lines, paste(sec1, collapse = " "))
    
    # Section 3
    sec3 <- "333"
    if (!is.null(mean_max_temp) && !is.null(mean_min_temp)) {
      if (data_monthly$t25[i] != "00" || data_monthly$t30 != "00") {
        sec3 <- append(sec3, 
                       paste0("0", data_monthly$t25[i], data_monthly$t30[i]))
      }
      if (data_monthly$t35[i] != "00" || data_monthly$t40 != "00") {
        sec3 <- append(sec3, 
                       paste0("1", data_monthly$t35[i], data_monthly$t40[i]))
      }
      if (data_monthly$tn0[i] != "00" || data_monthly$tx0 != "00") {
        sec3 <- append(sec3, 
                       paste0("2", data_monthly$t35[i], data_monthly$t40[i]))
      }
    }
    if (!is.null(total_precip)) {
      if (data_monthly$r01[i] != "00" || data_monthly$r05 != "00") {
        sec3 <- append(sec3, 
                       paste0("3", data_monthly$r01[i], data_monthly$r05[i]))
      }
      if (data_monthly$r10[i] != "00" || data_monthly$r50 != "00") {
        sec3 <- append(sec3, 
                       paste0("4", data_monthly$r10[i], data_monthly$r50[i]))
      }
      if (data_monthly$r100[i] != "00" || data_monthly$r150 != "00") {
        sec3 <- append(sec3, 
                       paste0("5", data_monthly$r100[i], data_monthly$r150[i]))
      }
    }
    if (!is.null(total_snow_depth)) {
      if (data_monthly$s00[i] != "00" || data_monthly$s01 != "00") {
        sec3 <- append(sec3, 
                       paste0("6", data_monthly$s00[i], data_monthly$s01[i]))
      }
      if (data_monthly$s10[i] != "00" || data_monthly$s50 != "00") {
        sec3 <- append(sec3, 
                       paste0("7", data_monthly$s10[i], data_monthly$s50[i]))
      }
    }
    if (!is.null(max_ws)) {
      if (data_monthly$f10[i] != "00" || data_monthly$f20 != "00" || data_monthly$f30 != "00") {
        sec3 <- append(sec3, 
                       paste0("8", data_monthly$f10[i], data_monthly$f20[i], data_monthly$f30[i]))
      }
    }
    if (!is.null(min_h_vis)) {
      if (data_monthly$v1[i] != "00" || data_monthly$v2 != "00" || data_monthly$v3 != "00") {
        sec3 <- append(sec3, 
                       paste0("9", data_monthly$v1[i], data_monthly$v2[i], data_monthly$v3[i]))
      }
    }
    lines <- append(lines, paste(sec3, collapse = " "))
    
    # TODO Add optional sections 2 & 4 to function.
    file_path <- paste0(folder, "/", "climat", s, y, mm, ".a")
    writeLines(lines, file_path)
    message("CLIMAT Message saved at: '", file_path, "'")
  }
}