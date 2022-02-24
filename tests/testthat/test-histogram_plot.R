library(cdms.products)
library(dplyr)
library(ggplot2)
library(tidyr)

niger50 <- daily_niger %>%
   dplyr::filter(year %in% 1950:1952)
 
agades <- niger50 %>%
  dplyr::filter(station_name == "Agades")
