#' Prepare daily data in CLIMAT messages format 
#'
#' @param data 
#' @param date_time 
#' @param station_id 
#' @param year 
#' @param month 
#' @param mean_pressure_station 
#' @param mean_pressure_reduced 
#' @param mean_temp 
#' @param mean_max_temp 
#' @param mean_min_temp 
#' @param mean_vapour_pressure 
#' @param total_precip 
#' @param total_sunshine 
#'
#' @return
#' @export
#'
#' @examples
prepare_climat_messages <- function(data, date_time, station_id, year = NULL, month = NULL,
                                    mean_pressure_station = NULL, mean_pressure_reduced = NULL, mean_temp = NULL,
                                    mean_max_temp = NULL, mean_min_temp = NULL, mean_vapour_pressure = NULL, 
                                    total_precip = NULL, total_sunshine = NULL) {
  
  checkmate::assert_data_frame(data)
  checkmate::assert_string(date_time)
  checkmate::assert(checkmate::check_date(data[[date_time]]), 
                    checkmate::check_posixct(data[[date_time]]))
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
  
  mean_cols <- c(mean_pressure_station, mean_pressure_reduced, mean_temp,
                 mean_max_temp, mean_min_temp, mean_vapour_pressure)
  monthly_data <- list()
  data_grp <- data %>%
    group_by(.data[[station_id]], .data[[year]], .data[[month]])
  if (length(mean_cols) != 0) {
    data_monthly_means <- data_grp %>%
      summarise(across(tidyselect::all_of(mean_cols), 
                       list(mean = mean, na = ~sum(is.na(.x))), 
                       na.rm = TRUE, .names = "{.fn}_{.col}"))
    monthly_data[["monthly_means"]] <- data_monthly_means
  }
  if (!is.null(mean_temp)) {
    data_monthly_sd <- data_grp %>%
      summarise(sd_mean_temp = sd(.data[[mean_temp]], na.rm = TRUE))
    monthly_data[["monthly_sd"]] <- data_monthly_sd
  }
  if (!is.null(total_precip)) {
    data_monthly_precip <- data_grp %>%
      summarise(sum_total_precip = sum(.data[[total_precip]]),
                n1_total_precip = sum(.data[[total_precip]] >= 1))
    monthly_data[["monthly_precip"]] <- data_monthly_precip
  }
  if (!is.null(total_sunshine)) {
    data_monthly_sunshine <- data_grp %>%
      summarise(sum_total_sunshine = sum(.data[[total_sunshine]]))
    monthly_data[["monthly_sunshine"]] <- data_monthly_sunshine
  }
  data_monthly <- 
    Reduce(function(x, y) dplyr::left_join(x, y, 
                                           by = c(station_id, year, month)),
           monthly_data
    )
  
  if (!is.null(mean_pressure_station)) {
    data_monthly <- data_monthly %>%
      mutate(
        mean_mean_pressure_station = 
          round(.data$mean_mean_pressure_station, 1),
        mean_mean_pressure_station = 
          ifelse(.data$mean_mean_pressure_station > 1000,
                 .data$mean_mean_pressure_station - 1000,
                 .data$mean_mean_pressure_station),
        mean_mean_pressure_station = 
          sprintf("%04d", .data$mean_mean_pressure_station * 10)
      )
  }
  if (!is.null(mean_pressure_reduced)) {
    data_monthly <- data_monthly %>%
      mutate(
        mean_mean_pressure_reduced = 
          round(.data$mean_mean_pressure_reduced, 1),
        mean_mean_pressure_reduced = 
          ifelse(.data$mean_mean_pressure_reduced > 1000,
                 .data$mean_mean_pressure_reduced - 1000,
                 .data$mean_mean_pressure_reduced),
        mean_mean_pressure_reduced = 
          sprintf("%04d", .data$mean_mean_pressure_reduced * 10)
      )
  }
  if (!is.null(mean_temp)) {
    data_monthly <- data_monthly %>%
      mutate(
        mean_mean_temp = round(.data$mean_mean_temp, 1),
        mean_mean_temp_i <- ifelse(mean_mean_temp >= 0, 0, 1),
        mean_mean_temp = sprintf("%03d", .data$mean_mean_temp * 10),
        sd_mean_temp = round(.data$sd_mean_temp, 1),
        sd_mean_temp = sprintf("%03d", .data$sd_mean_temp * 10),
      )
  }

  for(i in seq_len(nrow(data_monthly))) {
    s <- data_monthly[[station_id]][i]
    y <- data_monthly[[year]][i]
    jjj <- substr(y, 2, 4)
    m <- data_monthly[[month]][i]
    mm <- which(m == month_levels)
    mm <- sprintf("%02d", k)
    lines <- c()
    lines <- append(lines, paste0("CLIMAT", " ", mm, jjj, " ", s))
    if (!is.null(mean_pressure_station) && 
        !is.na(data_monthly$mean_mean_pressure_station[i])) {
      lines <- append(lines,
                      paste0(" ",
                             1,
                             data_monthly$mean_mean_pressure_station[i]))
    }
    if (!is.null(mean_pressure_reduced) && 
        !is.na(data_monthly$mean_mean_pressure_reduced[i])) {
      lines <- append(lines,
                      paste0(" ",
                             2,
                             data_monthly$mean_mean_pressure_reduced[i]))
    }
    if (!is.null(mean_temp) && 
        !is.na(data_monthly$mean_mean_temp[i])) {
      lines <- append(lines,
                      paste0(" ",
                             3,
                             data_monthly$mean_mean_temp_i[i],
                             data_monthly$mean_mean_temp[i],
                             data_monthly$sd_mean_temp[i]))
    }
  }
}