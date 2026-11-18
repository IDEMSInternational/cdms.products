# Copyright 2022 IDEMS International Community Interest Company
#
# This file is part of cdms.products.
#
# cdms.products is free software: you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# cdms.products is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along with
# cdms.products. If not, see <https://www.gnu.org/licenses/>.

library(here)
library(lubridate)

daily_niger <- read.csv(here("data-raw", "daily_niger_raw.csv"))
names(daily_niger)[1] <- "station_name"
names(daily_niger)[4] <- "month"
names(daily_niger)[6] <- "day"
daily_niger$rain <- NULL
names(daily_niger)[9] <- "rain"

daily_niger$date <- as.Date(daily_niger$date)
daily_niger$month <- lubridate::month(daily_niger$date, label = TRUE)
# Don't want month as ordered factor as ordered is handled differently
# in model functions
class(daily_niger$month) <- "factor"

usethis::use_data(daily_niger, overwrite = TRUE)
