# Clean Data -------------------------------------------------------------

# find missing values
shower = na.omit(shower) # none

# remove duplicates

# unix timestamps to datetime
shower$Time = as.POSIXct(shower$Time, origin = "1970-01-01")
shower = shower[order(shower$Time), ]

# Separate Data -----------------------------------------------------------

# split the data
shower_pre = shower[shower$Time < '2020-01-01', ]
shower_post = shower[shower$Time >= '2020-01-01', ]

# plot split data
shower_pre_plot = ggplot(data = shower_pre, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Shower Water Flow Pre-2020")
shower_post_plot = ggplot(data = shower_post, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Shower Water Flow Post-2020")

# Missing Values Pre 2020 -------------------------------------------------

shower_pre = shower_pre |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_pre = shower_pre  |> 
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

shower_pre = select(shower_pre, Time, Flow)

shower_pre = bind_rows(shower_pre, new_missing_dates_pre_df) |> 
  arrange(Time)

shower_pre = distinct(shower_pre, Time, .keep_all = TRUE)

# Missing Values Post 2020 ------------------------------------------------

shower_post = shower_post |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_post = shower_post  |> 
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

shower_post = select(shower_post, Time, Flow)

shower_post = bind_rows(shower_post, new_missing_dates_post) |> 
  arrange(Time)

shower_post = distinct(shower_post, Time, .keep_all = TRUE)

# Add Time Features -------------------------------------------------------

shower_pre = shower_pre %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

shower_post = shower_post %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

# Pre Summed Flow DataFrames ----------------------------------------------------

shower_pre_monthly_flow = shower_pre %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

shower_pre_weekly_flow = shower_pre %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

shower_pre_daily_flow = shower_pre %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

shower_pre_hourly_flow = shower_pre %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Post Summed Flow DataFrames --------------------------------------------

shower_post_monthly_flow = shower_post %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

shower_post_weekly_flow = shower_post %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

shower_post_daily_flow = shower_post %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

shower_post_hourly_flow = shower_post %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Pre Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
shower_pre$Datetime <- as.POSIXct(shower_pre$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
shower_pre$Week <- week(shower_pre$Datetime)
shower_pre$Year <- year(shower_pre$Datetime)
shower_pre$Hour <- hour(shower_pre$Datetime)
shower_pre$Weekday <- wday(shower_pre$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_pre_shower <- shower_pre %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPreShower = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_pre_shower <- weekly_total_flow_pre_shower %>%
  top_n(1, WeeklyTotalFlowPreShower) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_pre_shower <- highest_total_week_pre_shower$Year
highest_flow_week_pre_shower <- highest_total_week_pre_shower$Week

# Filter the original data for the highest flow week
highest_flow_week_data_pre_shower <- shower_pre %>%
  filter(Year == highest_flow_year_pre_shower & Week == highest_flow_week_pre_shower)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_pre_shower <- highest_flow_week_data_pre_shower %>%
  mutate(WeekdayGroupPreShower = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPreShower, Hour) %>%
  summarise(TotalFlowPreShower = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_pre_shower$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_pre_shower$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")


# Post Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
shower_post$Datetime <- as.POSIXct(shower_post$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
shower_post$Week <- week(shower_post$Datetime)
shower_post$Year <- year(shower_post$Datetime)
shower_post$Hour <- hour(shower_post$Datetime)
shower_post$Weekday <- wday(shower_post$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_post_shower <- shower_post %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPostShower = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_post_shower <- weekly_total_flow_post_shower %>%
  top_n(1, WeeklyTotalFlowPostShower) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_post_shower <- highest_total_week_post_shower$Year
highest_flow_week_post_shower <- highest_total_week_post_shower$Week

# Filter the original data for the highest flow week
highest_flow_week_data_post_shower <- shower_post %>%
  filter(Year == highest_flow_year_post_shower & Week == highest_flow_week_post_shower)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_post_shower <- highest_flow_week_data_post_shower %>%
  mutate(WeekdayGroupPostShower = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPostShower, Hour) %>%
  summarise(TotalFlowPostShower = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_post_shower$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_post_shower$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")

# For Cumulative post 2020 --------------------------------------------------------------------------------

# Filtering the original dataset for week number
shower_post_cum <- shower_post %>%
  filter(Week == 32)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, shower_post_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
shower_post_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# For Cumulative pre 2020 --------------------------------------------------------------------------------
# Filtering the original dataset for week number
shower_pre_cum <- shower_pre %>%
  filter(Week == 41)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, shower_pre_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
shower_pre_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# Final bubble map prep-----------------------------

# Month and Week of day vs total flow pre 2020

shower_pre_WH <- shower_pre %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

shower_pre_WH$Day <- factor(shower_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
shower_pre_WH$Hour <- factor(shower_pre_WH$Hour, levels = 0:23)

# Month and Week of day vs total flow post 2020

shower_post_WH <- shower_post %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

shower_pre_WH$Day <- factor(shower_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
shower_pre_WH$Hour <- factor(shower_pre_WH$Hour, levels = 0:23)

# Final heatmap prep-------------------------------------

# Month and Week of day vs total flow post 2020

shower_post_WM <- shower_post %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

shower_post_WM$Month <- factor(shower_post_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
shower_post_WM$Day <- factor(shower_post_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Month and Week of day vs total flow pre 2020

shower_pre_WM <- shower_pre %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

shower_pre_WM$Month <- factor(shower_pre_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
shower_pre_WM$Day <- factor(shower_pre_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
