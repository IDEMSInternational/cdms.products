#' Export CLIMAT messages file(s) from daily data
#'
#' @param data TODO
#' @param date_time TODO
#' @param station_id TODO
#' @param year TODO
#' @param month TODO
#' @param mean_pressure_station TODO
#' @param mean_pressure_reduced TODO
#' @param mean_temp TODO
#' @param mean_max_temp TODO
#' @param mean_min_temp TODO
#' @param mean_vapour_pressure TODO
#' @param total_precip TODO
#' @param total_sunshine TODO
#' @param folder TODO
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
  if (!is.null(total_precip)) {
    data_monthly_precip <- data_grp %>%
      dplyr::summarise(sum_total_precip = sum(.data[[total_precip]]),
                       n1_total_precip = sum(.data[[total_precip]] >= 1))
    monthly_data[["monthly_precip"]] <- data_monthly_precip
    sum_precip_col <- "sum_total_precip"
    n1_precip_col <- "n1_total_precip"
  }
  if (!is.null(total_sunshine)) {
    data_monthly_sunshine <- data_grp %>%
      dplyr::summarise(sum_total_sunshine = sum(.data[[total_sunshine]]))
    monthly_data[["monthly_sunshine"]] <- data_monthly_sunshine
    sum_sunshine_col <- "sum_total_sunshine"
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
        n1_total_precip = sprintf("%02d", .data[[n1_precip_col]]),
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
    sec0 <- paste0("CLIMAT", " ", mm, jjj, " ", s)
    lines <- append(lines, sec0)
    sec1 <- c()
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
                             data_monthly$n1_total_precip[i]))
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
    # TODO Add optional sections 2, 3 & 4 to function.
    file_path <- paste0(folder, "/", "climat", s, y, mm, ".a")
    writeLines(lines, file_path)
    message("CLIMAT Message saved at: '", file_path, "'")
  }
}