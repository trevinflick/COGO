library(tidyverse)
library(janitor)
library(data.table)
library(beepr)

# Download data -----------------------------------------------------------

download_cogo <- function(year_month){
  temp <- tempfile()
  download.file(paste0('https://s3.amazonaws.com/cogo-sys-data/', year_month, '-cogo-tripdata.zip'), temp)
  cogo <- try(read_csv(unz(temp, paste0(year_month, '-cogo-tripdata.csv'))))
  if (inherits(cogo, "try-error")) cogo <- try(read_csv(unz(temp, paste0(year_month, '-cogo-tripdata'))))
  unlink(temp)
  return(cogo)
}

# Get list between two dates ----------------------------------------------

date_seq <- function(t1, t2) { 
  format(seq(as.Date(paste0(t1,"01"), "%Y%m%d"), 
             as.Date(paste0(t2,"01"), "%Y%m%d"), by="month"), 
         "%Y%m") 
}

date_list <- date_seq("201802", "202010")

# purrr -------------------------------------------------------------------

df <- lapply(date_list, safely(download_cogo))

df2 <- map(df, "result")

# clean up the first downloaded tibble

column_names <- c("start_time", "end_time", "from_station_id", "from_station_name",
                  "from_station_lat", "from_station_lon", "to_station_id", "to_station_name",
                  "to_station_lat", "to_station_lon", "bikeid", "usertype", "gender", "birthyear")

colnames(df2[[1]]) <- column_names

df2[[1]] <- df2[[1]] %>% 
  unite(from_station_location, c(from_station_lat, from_station_lon), sep = ",", remove = TRUE) %>%
  unite(to_station_location, c(to_station_lat, to_station_lon), sep = ",", remove = TRUE)

# create trip IDs for first data set
df2[[1]]$trip_id <- c(1:nrow(df2[[1]]))
df2[[1]]$tripduration <- NA

# separate the old and new format
old_format <- df2[1:26]
new_format <- df2[27:33]

#### clean the old format to match new format
for (i in seq_along(old_format)) {
  old_format[[i]]$rideable_type <- "docked_bike"
} 

old_format[[1]]$start_time <- as.POSIXct(old_format[[1]]$start_time, format="%m/%d/%Y %H:%M:%S")
old_format[[1]]$end_time <- as.POSIXct(old_format[[1]]$end_time, format="%m/%d/%Y %H:%M:%S")

old_format[[1]] <- old_format[[1]] %>% 
  mutate(gender = case_when(
  gender == 1 ~ "Male",
  gender == 2 ~ "Female"
  )
)

#### clean the new format to match old format
# delete column
new_format[[2]]$is_equity <- NULL

# concat start/stop lat and lng into one column
for (i in seq_along(new_format)) {
  new_format[[i]] <- new_format[[i]] %>%
    unite(from_station_location, c(start_lat, start_lng), sep = ",", remove = TRUE) %>%
    unite(to_station_location, c(end_lat, end_lng), sep = ",", remove = TRUE)
}

# add missing columns to match old format
for (i in seq_along(new_format)) {
  new_format[[i]]$bikeid <- NA
  new_format[[i]]$gender <- NA
  new_format[[i]]$birthyear <- NA
  new_format[[i]]$tripduration <- NA
}

new_format_column_names <- c("trip_id", "rideable_type", "start_time", "end_time", "from_station_name",
                             "from_station_id", "to_station_name", "to_station_id", 
                             "from_station_location", "to_station_location", "usertype",
                             "bikeid", "gender", "birthyear", "tripduration")

for (i in seq_along(new_format)) {
  colnames(new_format[[i]]) <- new_format_column_names
}

# make old and new format columns match before merging
for (i in seq_along(old_format)) {
  old_format[[i]] <- old_format[[i]] %>% select(order(colnames(.)))
}

for (i in seq_along(new_format)) {
  new_format[[i]] <- new_format[[i]] %>% select(order(colnames(.)))
}

cogo_old <- bind_rows(old_format)
cogo_new <- bind_rows(new_format)

cogo_old$trip_id <- as.character(cogo_old$trip_id)

cogo <- bind_rows(cogo_old, cogo_new)

cogo <- cogo %>% 
  mutate(usertype = case_when(
    usertype == "casual" ~ "Non-Subscriber",
    usertype == "Customer" ~ "Non-Subscriber",
    usertype == "Dependent" ~ "Subscriber",
    usertype == "member" ~ "Subscriber",
    TRUE ~ "Subscriber"
  )
)

cogo$rideable_type <- if_else(cogo$rideable_type == "classic_bike", "docked_bike", cogo$rideable_type)

cogo$start_time_round <- format(round(cogo$start_time, units="hours"), format="%Y-%m-%d %H:%M:%S")
cogo$end_time_round <- format(round(cogo$end_time, units="hours"), format="%Y-%m-%d %H:%M:%S")

cogo$start_time_round <- strptime(cogo$start_time_round, "%Y-%m-%d %H:%M:%S", tz="EST")
cogo$end_time_round <- strptime(cogo$end_time_round, "%Y-%m-%d %H:%M:%S", tz="EST")

cogo$start_wday <- wday(cogo$start_time, label = TRUE)
cogo$end_wday <- wday(cogo$end_time, label = TRUE)

write.csv(cogo, "cogo.csv")

beep(8)

