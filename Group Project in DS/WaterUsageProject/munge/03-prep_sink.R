# Clean Data -------------------------------------------------------------

# find missing values
sink = na.omit(sink) # none

# remove outliers (1970 data point)
sink = sink[sink$Time > 1.5e+09, ]

# remove duplicates

# unix timestamps to datetime
sink$Time = as.POSIXct(sink$Time, origin = "1970-01-01")
sink = sink[order(sink$Time), ]

# plot data
plot2 = ggplot(data = sink, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Flow Time Series")

# missing 8 months of data
# could view separately

# Separate Data -----------------------------------------------------------

# split the data
sink_pre = sink[sink$Time < '2020-01-01', ]
sink_post = sink[sink$Time >= '2020-01-01', ]

# plot split data
sink_pre_plot = ggplot(data = sink_pre, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Sink Water Flow Pre-2020")
sink_post_plot = ggplot(data = sink_post, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Sink Water Flow Post-2020")

# Missing Values Pre 2020 -------------------------------------------------

sink_pre = sink_pre |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_pre = sink_pre  |> 
  filter(Time_Diff_Sec == 2 | Time_Diff_Sec == 3)

missing_dates_pre = missing_dates_pre |> 
  mutate(Avg_Flow = (lag(Flow) + Flow) / 2)

new_missing_dates_pre = lapply(2:nrow(missing_dates_pre), function(i) {
  seq(missing_dates_pre$Time[i - 1] + 1, 
      by = "1 sec", 
      length.out = missing_dates_pre$Time_Diff_Sec[i]) %>%
    as.POSIXct() %>%
    setNames("Time") %>%
    tibble(Flow = missing_dates_pre$Avg_Flow[i])
})

new_missing_dates_pre_df = bind_rows(new_missing_dates_pre) |> rename(Time = .)

sink_pre = select(sink_pre, Time, Flow)

sink_pre = bind_rows(sink_pre, new_missing_dates_pre_df) |> 
  arrange(Time)

sink_pre = distinct(sink_pre, Time, .keep_all = TRUE)

# Missing Values Post 2020 ------------------------------------------------

sink_post = sink_post |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_post = sink_post  |> 
  filter(Time_Diff_Sec == 2 | Time_Diff_Sec == 3)

missing_dates_post = missing_dates_post |> 
  mutate(Avg_Flow = (lag(Flow) + Flow) / 2)

new_missing_dates_post = lapply(2:nrow(missing_dates_post), function(i) {
  seq(missing_dates_post$Time[i - 1] + 1, 
      by = "1 sec", 
      length.out = missing_dates_post$Time_Diff_Sec[i]) %>%
    as.POSIXct() %>%
    setNames("Time") %>%
    tibble(Flow = missing_dates_post$Avg_Flow[i])
})

new_missing_dates_post = bind_rows(new_missing_dates_post) |> rename(Time = .)

sink_post = select(sink_post, Time, Flow)

sink_post = bind_rows(sink_post, new_missing_dates_post) |> 
  arrange(Time)

sink_post = distinct(sink_post, Time, .keep_all = TRUE)

# Add Time Features -------------------------------------------------------

sink_pre = sink_pre %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

sink_post = sink_post %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

# Pre Summed Flow DataFrames ----------------------------------------------------

sink_pre_monthly_flow = sink_pre %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

sink_pre_weekly_flow = sink_pre %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

sink_pre_daily_flow = sink_pre %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

sink_pre_hourly_flow = sink_pre %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Day of the week x Hour
sink_pre_week_x_hour = sink_pre |> 
  summarise(Flow = sum(Flow), count = n(), .by = c(Week, Hour))

# Month x Hour
sink_pre_month_x_hour = sink_pre |> 
  summarise(Flow = sum(Flow), count = n(), avg_flow = mean(Flow), standard_deviation = sd(Flow), .by = c(Month, Hour))

# Post Summed Flow DataFrames --------------------------------------------

sink_post_monthly_flow = sink_post %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

sink_post_weekly_flow = sink_post %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

sink_post_daily_flow = sink_post %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

sink_post_hourly_flow = sink_post %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))


# Pre Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
sink_pre$Datetime <- as.POSIXct(sink_pre$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
sink_pre$Week <- week(sink_pre$Datetime)
sink_pre$Year <- year(sink_pre$Datetime)
sink_pre$Hour <- hour(sink_pre$Datetime)
sink_pre$Weekday <- wday(sink_pre$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_pre_sink <- sink_pre %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPreSink = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_pre_sink <- weekly_total_flow_pre_sink %>%
  top_n(1, WeeklyTotalFlowPreSink) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_pre_sink <- highest_total_week_pre_sink$Year
highest_flow_week_pre_sink <- highest_total_week_pre_sink$Week

# Filter the original data for the highest flow week
highest_flow_week_data_pre_sink <- sink_pre %>%
  filter(Year == highest_flow_year_pre_sink & Week == highest_flow_week_pre_sink)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_pre_sink <- highest_flow_week_data_pre_sink %>%
  mutate(WeekdayGroupPreSink = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPreSink, Hour) %>%
  summarise(TotalFlowPreSink = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_pre_sink$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_pre_sink$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")


# Post Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
sink_post$Datetime <- as.POSIXct(sink_post$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
sink_post$Week <- week(sink_post$Datetime)
sink_post$Year <- year(sink_post$Datetime)
sink_post$Hour <- hour(sink_post$Datetime)
sink_post$Weekday <- wday(sink_post$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_post_sink <- sink_post %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPostSink = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_post_sink <- weekly_total_flow_post_sink %>%
  top_n(1, WeeklyTotalFlowPostSink) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_post_sink <- highest_total_week_post_sink$Year
highest_flow_week_post_sink <- highest_total_week_post_sink$Week

# Filter the original data for the highest flow week
highest_flow_week_data_post_sink <- sink_post %>%
  filter(Year == highest_flow_year_post_sink & Week == highest_flow_week_post_sink)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_post_sink <- highest_flow_week_data_post_sink %>%
  mutate(WeekdayGroupPostSink = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPostSink, Hour) %>%
  summarise(TotalFlowPostSink = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_post_sink$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_post_sink$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")






# Data Collection Frequency Histogram --------------------------------------------

# entry frequency
data_freq = ggplot(sink_pre, aes(x = Time)) +
  geom_histogram(bins = 100, fill = "grey", color = "black") +
  labs(x = "Time", y = "Frequency", title = "Data Collection Frequency")

# For Cumulative post 2020 --------------------------------------------------------------------------------

# Filtering the original dataset for week number
sink_post_cum <- sink_post %>%
  filter(Week == 31)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, sink_post_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
sink_post_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# For Cumulative pre 2020 --------------------------------------------------------------------------------
# Filtering the original dataset for week number
sink_pre_cum <- sink_pre %>%
  filter(Week == 24)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, sink_pre_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
sink_pre_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# Final bubble map prep-----------------------------

# Month and Week of day vs total flow pre 2020

sink_pre_WH <- sink_pre %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

sink_pre_WH$Day <- factor(sink_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
sink_pre_WH$Hour <- factor(sink_pre_WH$Hour, levels = 0:23)

# Month and Week of day vs total flow post 2020

sink_post_WH <- sink_post %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

sink_pre_WH$Day <- factor(sink_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
sink_pre_WH$Hour <- factor(sink_pre_WH$Hour, levels = 0:23)

# Final heatmap prep-------------------------------------

# Month and Week of day vs total flow post 2020

sink_post_WM <- sink_post %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

sink_post_WM$Month <- factor(sink_post_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
sink_post_WM$Day <- factor(sink_post_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Month and Week of day vs total flow pre 2020

sink_pre_WM <- sink_pre %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

sink_pre_WM$Month <- factor(sink_pre_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
sink_pre_WM$Day <- factor(sink_pre_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
