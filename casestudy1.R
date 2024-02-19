library(tidyverse)
library(geosphere)

glimpse(X202212_divvy_tripdata)

#appending data
data_processed <- rbind(
  X202212_divvy_tripdata,
  X202301_divvy_tripdata,
  X202302_divvy_tripdata,
  X202303_divvy_tripdata,
  X202304_divvy_tripdata,
  X202305_divvy_tripdata,
  X202306_divvy_tripdata,
  X202307_divvy_tripdata,
  X202308_divvy_tripdata,
  X202309_divvy_tripdata,
  X202310_divvy_tripdata,
  X202311_divvy_tripdata
)

#check data
glimpse(data_processed)
colSums(is.na(data_processed))

#cleaning
data_processed["start_station_name"][is.na(data_processed["start_station_name"])] <- "Not registered"
data_processed["start_station_id"][is.na(data_processed["start_station_id"])] <- "NR"
data_processed["end_station_name"][is.na(data_processed["end_station_name"])] <- "Not Registered"
data_processed["end_station_id"][is.na(data_processed["end_station_id"])] <- "NR"
data_processed["end_lat"][is.na(data_processed["end_lat"])] <- mean(data_processed$end_lat, na.rm = TRUE)
data_processed["end_lng"][is.na(data_processed["end_lng"])] <- mean(data_processed$end_lng, na.rm = TRUE)

colSums(is.na(data_processed))

#calculation mean of all numeric columns
colMeans(data_processed[sapply(data_processed, is.numeric)])

#processing
data_processed <- data_processed %>%
  mutate(ride_time_s = as.numeric(ended_at) - as.numeric(started_at)) %>%
  mutate(day_evening = case_when(
    hour(started_at) < 12 ~ "day",
    hour(started_at) >= 12 ~ "evening"
  )) %>%
  mutate(date = date(started_at)) %>% 
  mutate(month_abb = month.abb[month(date)]) %>% 
  mutate("distance(m)" = distGeo(matrix(c(start_lng, start_lat), ncol = 2),
                            matrix(c(end_lng, end_lat), ncol = 2)))


#summarizing
data_processed %>%
  group_by(member_casual) %>%
  summarise(
    mean(ride_time_s),
    median(ride_time_s),
    mean(`distance(m)`),
    count_classic = sum(rideable_type == "classic_bike"),
    count_docked = sum(rideable_type == "docked_bike"),
    count_electric = sum(rideable_type == "electric_bike"),
    count_day = sum(day_evening == "day"),
    count_evening = sum(day_evening == "evening")
  )

#data viz
ggplot(data_processed, aes(x = member_casual)) +
  geom_bar(fill="lightblue") +
  labs(title = "Number of rides between casual and member users", x = "Type of user", y = "Rides")

ggplot(data_processed, aes(x = hour(started_at), colour = member_casual)) +
  geom_freqpoly(bins = 24, lwd = 1) +
  labs(title = "Time of use of the service between casual and members users", x = "Time of start of use", y = "Frequency of use", colour = "Type of user")

ggplot(data_processed, aes(x = date, fill = member_casual)) +
  geom_histogram(bins = 12) +
  labs(title = "Number of members vs casual users through a year", x = "Time", y = "Number of members") +
  guides(fill = guide_legend(title= "Type of user"))
