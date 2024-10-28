# Clean Data -------------------------------------------------------------

# find missing values
kitchen_faucet = na.omit(kitchen_faucet) # none

# remove duplicates

# unix timestamps to datetime
kitchen_faucet$Time = as.POSIXct(kitchen_faucet$Time, origin = "1970-01-01")
kitchen_faucet = kitchen_faucet[order(kitchen_faucet$Time), ]

# Separate Data -----------------------------------------------------------

# split the data
kitchen_faucet_pre = kitchen_faucet |> filter(Time >= '2019-03-01' & Time <= '2019-11-30')
kitchen_faucet_post = kitchen_faucet |> filter(Time >= '2020-07-01' & Time <= '2020-10-31')

# plot split data
kitchen_faucet_pre_plot = ggplot(data = kitchen_faucet_pre, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Kitchen Faucet Water Flow Pre-2020")
kitchen_faucet_post_plot = ggplot(data = kitchen_faucet_post, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Kitchen Faucet Water Flow Post-2020")

# Missing Values Pre 2020 -------------------------------------------------

kitchen_faucet_pre = kitchen_faucet_pre |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_pre = kitchen_faucet_pre  |> 
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

kitchen_faucet_pre = select(kitchen_faucet_pre, Time, Flow)

kitchen_faucet_pre = bind_rows(kitchen_faucet_pre, new_missing_dates_pre_df) |> 
  arrange(Time)

kitchen_faucet_pre = distinct(kitchen_faucet_pre, Time, .keep_all = TRUE)

# Missing Values Post 2020 ------------------------------------------------

kitchen_faucet_post = kitchen_faucet_post |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_post = kitchen_faucet_post  |> 
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

kitchen_faucet_post = select(kitchen_faucet_post, Time, Flow)

kitchen_faucet_post = bind_rows(kitchen_faucet_post, new_missing_dates_post) |> 
  arrange(Time)

kitchen_faucet_post = distinct(kitchen_faucet_post, Time, .keep_all = TRUE)

# Add Time Features -------------------------------------------------------

kitchen_faucet_pre = kitchen_faucet_pre %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time)) # %>% filter(kitchen_faucet$Month != 'Feb') # (not useful)

kitchen_faucet_post = kitchen_faucet_post %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

# Pre Summed Flow DataFrames ----------------------------------------------------

kitchen_faucet_pre_monthly_flow = kitchen_faucet_pre %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

kitchen_faucet_pre_weekly_flow = kitchen_faucet_pre %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

kitchen_faucet_pre_daily_flow = kitchen_faucet_pre %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

kitchen_faucet_pre_hourly_flow = kitchen_faucet_pre %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Post Summed Flow DataFrames --------------------------------------------

kitchen_faucet_post_monthly_flow = kitchen_faucet_post %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

kitchen_faucet_post_weekly_flow = kitchen_faucet_post %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

kitchen_faucet_post_daily_flow = kitchen_faucet_post %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

kitchen_faucet_post_hourly_flow = kitchen_faucet_post %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Pre Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
kitchen_faucet_pre$Datetime <- as.POSIXct(kitchen_faucet_pre$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
kitchen_faucet_pre$Week <- week(kitchen_faucet_pre$Datetime)
kitchen_faucet_pre$Year <- year(kitchen_faucet_pre$Datetime)
kitchen_faucet_pre$Hour <- hour(kitchen_faucet_pre$Datetime)
kitchen_faucet_pre$Weekday <- wday(kitchen_faucet_pre$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_pre_kitchen_faucet <- kitchen_faucet_pre %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPreKitchen_Faucet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_pre_kitchen_faucet <- weekly_total_flow_pre_kitchen_faucet %>%
  top_n(1, WeeklyTotalFlowPreKitchen_Faucet) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_pre_kitchen_faucet <- highest_total_week_pre_kitchen_faucet$Year
highest_flow_week_pre_kitchen_faucet <- highest_total_week_pre_kitchen_faucet$Week

# Filter the original data for the highest flow week
highest_flow_week_data_pre_kitchen_faucet <- kitchen_faucet_pre %>%
  filter(Year == highest_flow_year_pre_kitchen_faucet & Week == highest_flow_week_pre_kitchen_faucet)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_pre_kitchen_faucet <- highest_flow_week_data_pre_kitchen_faucet %>%
  mutate(WeekdayGroupPreKitchen_Faucet = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPreKitchen_Faucet, Hour) %>%
  summarise(TotalFlowPreKitchen_Faucet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_pre_kitchen_faucet$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_pre_kitchen_faucet$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")


# Post Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
kitchen_faucet_post$Datetime <- as.POSIXct(kitchen_faucet_post$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
kitchen_faucet_post$Week <- week(kitchen_faucet_post$Datetime)
kitchen_faucet_post$Year <- year(kitchen_faucet_post$Datetime)
kitchen_faucet_post$Hour <- hour(kitchen_faucet_post$Datetime)
kitchen_faucet_post$Weekday <- wday(kitchen_faucet_post$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_post_kitchen_faucet <- kitchen_faucet_post %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPostKitchen_Faucet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_post_kitchen_faucet <- weekly_total_flow_post_kitchen_faucet %>%
  top_n(1, WeeklyTotalFlowPostKitchen_Faucet) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_post_kitchen_faucet <- highest_total_week_post_kitchen_faucet$Year
highest_flow_week_post_kitchen_faucet <- highest_total_week_post_kitchen_faucet$Week

# Filter the original data for the highest flow week
highest_flow_week_data_post_kitchen_faucet <- kitchen_faucet_post %>%
  filter(Year == highest_flow_year_post_kitchen_faucet & Week == highest_flow_week_post_kitchen_faucet)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_post_kitchen_faucet <- highest_flow_week_data_post_kitchen_faucet %>%
  mutate(WeekdayGroupPostKitchen_Faucet = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPostKitchen_Faucet, Hour) %>%
  summarise(TotalFlowPostKitchen_Faucet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_post_kitchen_faucet$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_post_kitchen_faucet$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")

# For Cumulative post 2020 --------------------------------------------------------------------------------

# Filtering the original dataset for week number
kitchen_faucet_post_cum <- kitchen_faucet_post %>%
  filter(Week == 38)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, kitchen_faucet_post_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
kitchen_faucet_post_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# For Cumulative pre 2020 --------------------------------------------------------------------------------
# Filtering the original dataset for week number
kitchen_faucet_pre_cum <- kitchen_faucet_pre %>%
  filter(Week == 23)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, kitchen_faucet_pre_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
kitchen_faucet_pre_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# Final bubble map prep-----------------------------

# Month and Week of day vs total flow pre 2020

kitchen_faucet_pre_WH <- kitchen_faucet_pre %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

kitchen_faucet_pre_WH$Day <- factor(kitchen_faucet_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
kitchen_faucet_pre_WH$Hour <- factor(kitchen_faucet_pre_WH$Hour, levels = 0:23)

# Month and Week of day vs total flow post 2020

kitchen_faucet_post_WH <- kitchen_faucet_post %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

kitchen_faucet_pre_WH$Day <- factor(kitchen_faucet_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
kitchen_faucet_pre_WH$Hour <- factor(kitchen_faucet_pre_WH$Hour, levels = 0:23)

# Final heatmap prep-------------------------------------

# Month and Week of day vs total flow post 2020

kitchen_faucet_post_WM <- kitchen_faucet_post %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

kitchen_faucet_post_WM$Month <- factor(kitchen_faucet_post_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
kitchen_faucet_post_WM$Day <- factor(kitchen_faucet_post_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Month and Week of day vs total flow pre 2020

kitchen_faucet_pre_WM <- kitchen_faucet_pre %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

kitchen_faucet_pre_WM$Month <- factor(kitchen_faucet_pre_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
kitchen_faucet_pre_WM$Day <- factor(kitchen_faucet_pre_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))