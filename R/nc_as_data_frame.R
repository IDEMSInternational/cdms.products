#' Title
#'
#' @param nc TODO
#' @param vars TODO
#' @param keep_raw_time TODO
#' @param include_metadata TODO
#' @param boundary TODO
#' @param lon_points TODO
#' @param lat_points TODO
#' @param id_points TODO
#' @param show_requested_points TODO
#' @param great_circle_dist TODO
#'
#' @return
#' @export
#'
#' @examples # TODO
nc_as_data_frame <- function(nc, vars, keep_raw_time = TRUE, include_metadata = TRUE, boundary = NULL, lon_points = NULL, lat_points = NULL, id_points = NULL, show_requested_points = TRUE, great_circle_dist = TRUE) {
  if(missing(vars)) vars <- ncdf4.helpers::nc.get.variable.list(nc)
  if(sum(is.null(lon_points), is.null(lat_points)) == 1) stop("You must specificy both lon_points and lat_points")
  has_points <- (sum(is.null(lon_points), is.null(lat_points)) == 0)
  if(has_points && length(lon_points) != length(lat_points)) stop("lon_points and lat_points have unequal lengths.")
  if(has_points && !is.null(id_points) && length(id_points) != length(lat_points)) stop("id_points (if specified) must have the same length as lon_points and lat_points.")
  dim_names <- ncdf4.helpers::nc.get.dim.names(nc, vars[1])
  dim_values <- list()
  requested_points_added <- FALSE
  for(dim_name in dim_names) {
    #why no wrapper for this in ncdf4.helper?
    #(as.numeric ensures vectors no not have array class)
    dim_values[[dim_name]] <- as.numeric(nc$dim[[dim_name]]$vals)
    #This is not recommended but appears in tutorials
    #ncdf4::ncvar_get(nc, dim_name)
  }
  dim_axes <- ncdf4.helpers::nc.get.dim.axes(nc, vars[1])
  if(!is.null(boundary)) {
    if(!all(names(boundary) %in% dim_names)) stop("boundary contains dimensions not associated with", vars[1])
    if(anyNA(dim_axes)) {
      warning("Cannot subset data when some dimension axes cannot be identified.")
      start <- NA
      count <- NA
    }
    else {
      start <- c()
      count <- c()
      for(dim in c("X", "Y", "Z", "T", "S")) {
        if(dim %in% dim_axes) {
          dim_var <- names(dim_axes)[which(dim_axes == dim)]
          curr_dim_values <- dim_values[[dim_var]]
          if(dim_var %in% names(boundary) && !(has_points && dim %in% c("X", "Y"))) {
            if(dim == "T") {
              ind <- integer(0)
              try({
                print(dim_var)
                units <- ncdf4::ncatt_get(nc, dim_var, "units")
                if(units$hasatt && units$value == "julian_day") {
                  # RDotNet interprets Date class as numeric so character needed to preserve date
                  time_vals <- as.Date(curr_dim_values, origin = structure(-2440588, class = "Date"))
                }
                else {
                  pcict_time <- ncdf4.helpers::nc.get.time.series(nc, time.dim.name = dim_var)
                  posixct_time <- PCICt::as.POSIXct.PCICt(pcict_time)
                  time_vals <- as.Date(posixct_time)
                }
                ind <- which(time_vals >= boundary[[dim_var]][[1]] & time_vals <= boundary[[dim_var]][[2]])
              })
            }
            else ind <- which(curr_dim_values >= boundary[[dim_var]][1] & curr_dim_values <= boundary[[dim_var]][2])
            # TODO This is temporary solution for when there is only one value for a dimension and there are rounding difference
            if(length(ind) == 0 && length(curr_dim_values) == 1 && round(curr_dim_values, 3) == round(boundary[[dim_var]][1], 3) && round(curr_dim_values, 3) == round(boundary[[dim_var]][2], 3)) ind <- 1
            if(length(ind) == 0) {
              stop("No values within the range specified for ", dim_var, ".")
            }
            else {
              start <- c(start, min(ind))
              count <- c(count, length(ind))
              dim_values[[dim_var]] <- dim_values[[dim_var]][ind]
            }
          }
          else {
            start <- c(start, 1)
            count <- c(count, length(curr_dim_values))
          }
        }
      }
      if(length(start) == 0) {
        start <- rep(1, length(dim_axes))
        count <- rep(-1, length(dim_axes))
      }
    }
  }
  else {
    start <- rep(1, length(dim_axes))
    count <- rep(-1, length(dim_axes))
  }
  start_list <- list()
  count_list <- list()
  dim_values_list <- list()
  if(has_points) {
    dim_axes <- ncdf4.helpers::nc.get.dim.axes(nc, vars[1])
    x_var <- names(dim_axes)[which(dim_axes == "X")]
    y_var <- names(dim_axes)[which(dim_axes == "Y")]
    if(length(x_var) == 0 || length(y_var) == 0) stop("Cannot select points because dimensions are not labelled correctly in the nc file. Modify the nc file or remove the points to import all data.")
    xs <- dim_values[[x_var]]
    ys <- dim_values[[y_var]]
    for(i in seq_along(lon_points)) {
      curr_start <- start
      curr_count <- count
      curr_dim_values <- dim_values
      xy_possible <- expand.grid(xs, ys)
      point_ind <- which.min(sp::spDistsN1(pts = as.matrix(xy_possible), pt = c(lon_points[i], lat_points[i]), longlat = great_circle_dist))
      x_ind <- which(xs == xy_possible[point_ind, 1])[1]
      curr_start[1] <- x_ind
      curr_count[1]  <- 1
      curr_dim_values[[x_var]] <- curr_dim_values[[x_var]][x_ind]
      y_ind <- which(ys == xy_possible[point_ind, 2])[1]
      curr_start[2] <- y_ind
      curr_count[2]  <- 1
      curr_dim_values[[y_var]] <- curr_dim_values[[y_var]][y_ind]
      if(show_requested_points) {
        curr_dim_values[[paste0(x_var, "_point")]] <- lon_points[i]
        curr_dim_values[[paste0(y_var, "_point")]] <- lat_points[i]
        if(!is.null(id_points)) curr_dim_values[["station"]] <- id_points[i]
        requested_points_added <- TRUE
      }
      
      start_list[[i]] <- curr_start
      count_list[[i]] <- curr_count
      dim_values_list[[i]] <- curr_dim_values
    }
  }
  else {
    start_list[[1]] <- start
    count_list[[1]] <- count
    dim_values_list[[1]] <- dim_values
  }
  
  dim_axes <- ncdf4.helpers::nc.get.dim.axes(nc)
  time_dims <- names(dim_axes[which(dim_axes == "T" & names(dim_axes) %in% dim_names)])
  var_data_list <- list()
  for(i in seq_along(start_list)) {
    curr_dim_values <- dim_values_list[[i]]
    curr_var_data <- expand.grid(curr_dim_values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    for(j in seq_along(curr_var_data)) {
      attr(curr_var_data[[j]], "dim") <- NULL
    }
    names(curr_var_data) <- names(curr_dim_values)
    included_vars <- dim_names
    for(var in vars) {
      curr_dim_names <- ncdf4.helpers::nc.get.dim.names(nc, var)
      if(!setequal(curr_dim_names, dim_names)) {
        warning("The dimensions of", var, "do not match the other variables.", var, "will be dropped.")
      }
      else {
        included_vars <- c(included_vars, var)
        curr_var_data[[var]] <- as.vector(ncdf4::ncvar_get(nc, var, start = start_list[[i]], count = count_list[[i]]))
      }
    }
    if(length(time_dims) == 1) {
      time_var <- time_dims
      raw_time_full <- nc$dim[[time_var]]$vals
      raw_time <- curr_dim_values[[time_var]]
      attr(raw_time, "dim") <- NULL
      df_names <- time_var
      time_df <- data.frame(raw_time)
      names(time_df) <- time_var
      try({
        # need to subset this if time var has been subsetted
        time_ind <- which(raw_time_full %in% raw_time)
        units <- ncdf4::ncatt_get(nc, time_var, "units")
        if(units$hasatt && units$value == "julian_day") {
          time_df[["date"]] <- as.Date(raw_time, origin = structure(-2440588, class = "Date"))
        }
        else {
          pcict_time <- ncdf4.helpers::nc.get.time.series(nc, time.dim.name = time_var)
          pcict_time <- pcict_time[time_ind]
          posixct_time <- PCICt::as.POSIXct.PCICt(pcict_time)
          time_df[["date"]] <- as.Date(posixct_time)
          time_df[["datetime"]] <- posixct_time
        }
      })
      if(ncol(time_df) > 1) curr_var_data <- dplyr::full_join(curr_var_data, time_df, by = time_var)
      if(!keep_raw_time) {
        var_data[[time_var]] <- NULL
        included_vars <- included_vars[-which(included_vars == time_var)]
      }
    }
    var_data_list[[i]] <- curr_var_data
  }
  # # Following conventions in http://www.unidata.ucar.edu/software/netcdf/docs/attribute_conventions.html
  # if(replace_missing) {
  #   for(inc_var in included_vars) {
  #     
  #     numeric_var <- is.numeric(var_data[[inc_var]])
  #     integer_var <- is.integer(var_data[[inc_var]])
  #     valid_range <- ncdf4::ncatt_get(nc, var, "valid_range")
  #     valid_min <- ncdf4::ncatt_get(nc, var, "valid_min")
  #     valid_max <- ncdf4::ncatt_get(nc, var, "valid_max")
  #     fill_value <- ncdf4::ncatt_get(nc, var, "_FillValue")
  #     missing_value <- ncdf4::ncatt_get(nc, var, "missing_value")
  # 
  #     if(numeric_var && valid_range[[1]]) {
  #       var_data[[inc_var]][var_data[[inc_var]] < valid_range[[2]][1] | var_data[[inc_var]] > valid_range[[2]][2]] <- NA
  #     }
  #     else if(numeric_var && (valid_min[[1]] || valid_max[[1]])) {
  #       if(valid_min[[1]]) {
  #         var_data[[inc_var]][var_data[[inc_var]] < valid_min[[2]]] <- NA
  #       }
  #       if(valid_max[[2]]) {
  #         var_data[[inc_var]][var_data[[inc_var]] > valid_max[[2]]] <- NA
  #       }
  #     }
  #     else if(fill_value[[1]]) {
  #       val <- fill_value[[2]]
  #       if(numeric_var) {
  #         # Not sure this is safe if 'integer' types from file do not import as
  #         # 'integer' types in R.
  #         if(integer_var) width <- 1
  #         else width <- 2 * .Machine$double.xmin
  #         
  #         if(val > 0) var_data[[inc_var]][var_data[[inc_var]] > val + width] <- NA
  #         else var_data[[inc_var]][var_data[[inc_var]] < val - width] <- NA
  #       }
  #       else {
  #         # Should we do this? Non numeric not mentioned in convention
  #         var_data[[inc_var]][var_data[[inc_var]] %in% val] <- NA
  #       }
  #     }
  #     if(missing_value[[1]]) var_data[[inc_var]][var_data[[inc_var]] %in% missing_value[[2]]] <- NA
  #   }
  # }
  if(length(var_data_list) > 1) {
    var_data <- dplyr::bind_rows(var_data_list)
  }
  else if(length(var_data_list) == 1) var_data <- var_data_list[[1]]
  else var_data_list <- data.frame()
  
  if(include_metadata) {
    for(col_name in included_vars) {
      col_attr <- ncdf4::ncatt_get(nc, col_name)
      for(i in seq_along(col_attr)) {
        attr(var_data[[col_name]], names(col_attr)[i]) <- col_attr[[i]]
      }
    }
    global_attr <- ncdf4::ncatt_get(nc, 0)
    for(i in seq_along(global_attr)) {
      attr(var_data, names(global_attr)[i]) <- global_attr[[i]]
    }
  }
  return(var_data)
}