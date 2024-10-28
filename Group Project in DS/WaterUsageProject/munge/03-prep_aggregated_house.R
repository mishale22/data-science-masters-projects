# Statistical approach for aggregated Whole House water usage
aggregatedWholeHouse$datetime <- as.POSIXct(aggregatedWholeHouse$unix, origin = "1970-01-01", tz = "UTC")
aggregatedWholeHouse$time_diff <- c(NA, as.integer(diff(aggregatedWholeHouse$datetime)))

aggregatedWholeHouse_time_diff_table <- table(aggregatedWholeHouse$time_diff)
aggregatedWholeHouse_time_diff_df <- as.data.frame(aggregatedWholeHouse_time_diff_table)
aggregatedWholeHouse_time_diff_df$Proportion <- aggregatedWholeHouse_time_diff_df$Freq / sum(aggregatedWholeHouse_time_diff_df$Freq)

aggregatedWholeHouse_unusual <- aggregatedWholeHouse %>% 
  filter(time_diff > 5)

aggregatedWholeHouse_part_1_cleaned <- aggregatedWholeHouse %>% 
  filter(datetime <= as.POSIXct("2019-10-28 23:53:06", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

aggregatedWholeHouse_features <- aggregatedWholeHouse_part_1_cleaned %>%
  mutate(
    year = year(datetime),
    quarter = quarter(datetime),
    month = month(datetime),
    week = week(datetime),
    day_of_week = wday(datetime, label = TRUE),
    hour = hour(datetime)
  ) %>%
  select(datetime, year, quarter, month, week, day_of_week, hour, flow)

aggregatedWholeHouse_features_hourly_avg_data <- aggregatedWholeHouse_features %>%
  group_by(hour) %>%
  summarise(avg_flow = mean(flow, na.rm = TRUE))

aggregatedWholeHouse_features_hourly_data <- aggregatedWholeHouse_features %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_sum = sum(flow, na.rm = TRUE))

aggregatedWholeHouse_features_hourly_data$diff_hourly_sum <- c(NA, diff(aggregatedWholeHouse_features_hourly_data$hourly_sum))

aggregatedWholeHouse_features_daily_data <- aggregatedWholeHouse_features %>%
  mutate(day = floor_date(datetime, "day")) %>%
  group_by(day) %>%
  summarise(daily_sum = sum(flow, na.rm = TRUE))

aggregatedWholeHouse_features_daily_data$diff_daily_sum <- c(NA, diff(aggregatedWholeHouse_features_daily_data$daily_sum))

aggregatedWholeHouse_features_weekly_data <- aggregatedWholeHouse_features %>%
  mutate(week = floor_date(datetime, "week")) %>% 
  group_by(week) %>%
  summarise(weekly_sum = sum(flow, na.rm = TRUE))

aggregatedWholeHouse_features_weekly_data$diff_weekly_sum <- c(NA, diff(aggregatedWholeHouse_features_weekly_data$weekly_sum))
aggregatedWholeHouse_features_weekly_data$diff_weekly_sum_2 <- c(NA, diff(aggregatedWholeHouse_features_weekly_data$diff_weekly_sum))

aggregatedWholeHouse_features_weekly_avg_data <- aggregatedWholeHouse_features %>%
  group_by(day_of_week) %>%
  summarise(avg_flow = mean(flow, na.rm = TRUE))

aggregatedWholeHouse_features_monthly_data <- aggregatedWholeHouse_features %>%
  mutate(month = floor_date(datetime, "month")) %>%
  group_by(month) %>%
  summarise(monthly_sum = sum(flow, na.rm = TRUE))

aggregatedWholeHouse_features_monthly_data$diff_monthly_sum <- c(NA, diff(aggregatedWholeHouse_features_monthly_data$monthly_sum))

aggregatedWholeHouse_features_monthly_avg_data <- aggregatedWholeHouse_features %>%
  group_by(month) %>%
  summarise(avg_flow = mean(flow, na.rm = TRUE))

aggregatedWholeHouse_features_quarterly_data <- aggregatedWholeHouse_features %>%
  mutate(quarter = floor_date(datetime, "quarter")) %>%
  group_by(quarter) %>%
  summarise(quarterly_sum = sum(flow, na.rm = TRUE))

aggregatedWholeHouse_features_quarterly_data$diff_quarterly_sum <- c(NA, diff(aggregatedWholeHouse_features_quarterly_data$quarterly_sum))

aggregatedWholeHouse_features_quarterly_avg_data <- aggregatedWholeHouse_features %>%
  group_by(quarter) %>%
  summarise(avg_flow = mean(flow, na.rm = TRUE))

aggregatedWholeHouse_features_hourly_ts <- ts(data = aggregatedWholeHouse_features_hourly_data$hourly_sum, start = c(2019, (32+13)/365), frequency = 24*365)
aggregatedWholeHouse_features_daily_ts <- ts(aggregatedWholeHouse_features_daily_data$daily_sum, start = c(2019, 44/365), frequency = 365)
aggregatedWholeHouse_features_weekly_ts <- ts(aggregatedWholeHouse_features_weekly_data$weekly_sum, start = c(2019, 6/52), frequency = 52)


