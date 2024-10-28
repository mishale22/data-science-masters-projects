# Clean Data -------------------------------------------------------------

# find missing values
washing = na.omit(washing) # none

# remove duplicates

# unix timestamps to datetime
washing$Time = as.POSIXct(washing$Time, origin = "1970-01-01")
washing = washing[order(washing$Time), ]

# Separate Data -----------------------------------------------------------

# split the data
washing_pre = washing[washing$Time < '2020-01-01', ]
washing_post = washing[washing$Time >= '2020-01-01', ]

# plot split data
washing_pre_plot = ggplot(data = washing_pre, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Washing Water Flow Pre-2020")
washing_post_plot = ggplot(data = washing_post, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Washing Water Flow Post-2020")

# Missing Values Pre 2020 -------------------------------------------------

washing_pre = washing_pre |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_pre = washing_pre  |> 
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

washing_pre = select(washing_pre, Time, Flow)

washing_pre = bind_rows(washing_pre, new_missing_dates_pre_df) |> 
  arrange(Time)

washing_pre = distinct(washing_pre, Time, .keep_all = TRUE)

# Missing Values Post 2020 ------------------------------------------------

washing_post = washing_post |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_post = washing_post  |> 
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

washing_post = select(washing_post, Time, Flow)

washing_post = bind_rows(washing_post, new_missing_dates_post) |> 
  arrange(Time)

washing_post = distinct(washing_post, Time, .keep_all = TRUE)

# Add Time Features -------------------------------------------------------

washing_pre = washing_pre %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

washing_post = washing_post %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

# Pre Summed Flow DataFrames ----------------------------------------------------

washing_pre_monthly_flow = washing_pre %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

washing_pre_weekly_flow = washing_pre %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

washing_pre_daily_flow = washing_pre %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

washing_pre_hourly_flow = washing_pre %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Post Summed Flow DataFrames --------------------------------------------

washing_post_monthly_flow = washing_post %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

washing_post_weekly_flow = washing_post %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

washing_post_daily_flow = washing_post %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

washing_post_hourly_flow = washing_post %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Pre Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
washing_pre$Datetime <- as.POSIXct(washing_pre$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
washing_pre$Week <- week(washing_pre$Datetime)
washing_pre$Year <- year(washing_pre$Datetime)
washing_pre$Hour <- hour(washing_pre$Datetime)
washing_pre$Weekday <- wday(washing_pre$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_pre_washing <- washing_pre %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPreWashing = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_pre_washing <- weekly_total_flow_pre_washing %>%
  top_n(1, WeeklyTotalFlowPreWashing) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_pre_washing <- highest_total_week_pre_washing$Year
highest_flow_week_pre_washing <- highest_total_week_pre_washing$Week

# Filter the original data for the highest flow week
highest_flow_week_data_pre_washing <- washing_pre %>%
  filter(Year == highest_flow_year_pre_washing & Week == highest_flow_week_pre_washing)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_pre_washing <- highest_flow_week_data_pre_washing %>%
  mutate(WeekdayGroupPreWashing = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPreWashing, Hour) %>%
  summarise(TotalFlowPreWashing = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_pre_washing$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_pre_washing$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")


# Post Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
washing_post$Datetime <- as.POSIXct(washing_post$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
washing_post$Week <- week(washing_post$Datetime)
washing_post$Year <- year(washing_post$Datetime)
washing_post$Hour <- hour(washing_post$Datetime)
washing_post$Weekday <- wday(washing_post$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_post_washing <- washing_post %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPostWashing = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_post_washing <- weekly_total_flow_post_washing %>%
  top_n(1, WeeklyTotalFlowPostWashing) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_post_washing <- highest_total_week_post_washing$Year
highest_flow_week_post_washing <- highest_total_week_post_washing$Week

# Filter the original data for the highest flow week
highest_flow_week_data_post_washing <- washing_post %>%
  filter(Year == highest_flow_year_post_washing & Week == highest_flow_week_post_washing)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_post_washing <- highest_flow_week_data_post_washing %>%
  mutate(WeekdayGroupPostWashing = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPostWashing, Hour) %>%
  summarise(TotalFlowPostWashing = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_post_washing$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_post_washing$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")

# For Cumulative post 2020 --------------------------------------------------------------------------------

# Filtering the original dataset for week number
washing_post_cum <- washing_post %>%
  filter(Week == 40)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, washing_post_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
washing_post_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# For Cumulative pre 2020 --------------------------------------------------------------------------------
# Filtering the original dataset for week number
washing_pre_cum <- washing_pre %>%
  filter(Week == 43)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, washing_pre_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
washing_pre_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# Final bubble map prep-----------------------------

# Month and Week of day vs total flow pre 2020

washing_pre_WH <- washing_pre %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

washing_pre_WH$Day <- factor(washing_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
washing_pre_WH$Hour <- factor(washing_pre_WH$Hour, levels = 0:23)

# Month and Week of day vs total flow post 2020

washing_post_WH <- washing_post %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

washing_pre_WH$Day <- factor(washing_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
washing_pre_WH$Hour <- factor(washing_pre_WH$Hour, levels = 0:23)

# Final heatmap prep-------------------------------------

# Month and Week of day vs total flow post 2020

washing_post_WM <- washing_post %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

washing_post_WM$Month <- factor(washing_post_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
washing_post_WM$Day <- factor(washing_post_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Month and Week of day vs total flow pre 2020

washing_pre_WM <- washing_pre %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

washing_pre_WM$Month <- factor(washing_pre_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
washing_pre_WM$Day <- factor(washing_pre_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))