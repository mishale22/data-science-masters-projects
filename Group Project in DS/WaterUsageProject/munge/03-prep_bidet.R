# Clean Data -------------------------------------------------------------

# find missing values
bidet = na.omit(bidet) # none

# remove duplicates

# unix timestamps to datetime
bidet$Time = as.POSIXct(bidet$Time, origin = "1970-01-01")
bidet = bidet[order(bidet$Time), ]

# Separate Data -----------------------------------------------------------

# split the data
bidet_pre = bidet[bidet$Time < '2020-01-01', ]
bidet_post = bidet[bidet$Time >= '2020-01-01', ]

# plot split data
bidet_pre_plot = ggplot(data = bidet_pre, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Bidet Water Flow Pre-2020")
bidet_post_plot = ggplot(data = bidet_post, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Bidet Water Flow Post-2020")

# Missing Values Pre 2020 -------------------------------------------------

bidet_pre = bidet_pre |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_pre = bidet_pre  |> 
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

bidet_pre = select(bidet_pre, Time, Flow)

bidet_pre = bind_rows(bidet_pre, new_missing_dates_pre_df) |> 
  arrange(Time)

bidet_pre = distinct(bidet_pre, Time, .keep_all = TRUE)

# Missing Values Post 2020 ------------------------------------------------

bidet_post = bidet_post |> 
  arrange(Time) |> 
  mutate(Time_Diff_Sec = round(as.numeric(difftime(Time, lag(Time), units = 'secs'))))

missing_dates_post = bidet_post  |> 
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

bidet_post = select(bidet_post, Time, Flow)

bidet_post = bind_rows(bidet_post, new_missing_dates_post) |> 
  arrange(Time)

bidet_post = distinct(bidet_post, Time, .keep_all = TRUE)

# Add Time Features -------------------------------------------------------

bidet_pre = bidet_pre %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

bidet_post = bidet_post %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

# Pre Summed Flow DataFrames ----------------------------------------------------

bidet_pre_monthly_flow = bidet_pre %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

bidet_pre_weekly_flow = bidet_pre %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

bidet_pre_daily_flow = bidet_pre %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

bidet_pre_hourly_flow = bidet_pre %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Post Summed Flow DataFrames --------------------------------------------

bidet_post_monthly_flow = bidet_post %>%
  group_by(Month) %>%
  summarise(Monthly_Flow = sum(Flow))

bidet_post_weekly_flow = bidet_post %>%
  group_by(Week, Month) %>%
  summarise(Weekly_Flow = sum(Flow))

bidet_post_daily_flow = bidet_post %>%
  group_by(Day) %>%
  summarise(Daily_Flow = sum(Flow))

bidet_post_hourly_flow = bidet_post %>%
  group_by(Hour) %>%
  summarise(Hourly_Flow = sum(Flow))

# Pre Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
bidet_pre$Datetime <- as.POSIXct(bidet_pre$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
bidet_pre$Week <- week(bidet_pre$Datetime)
bidet_pre$Year <- year(bidet_pre$Datetime)
bidet_pre$Hour <- hour(bidet_pre$Datetime)
bidet_pre$Weekday <- wday(bidet_pre$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_pre_bidet <- bidet_pre %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPreBidet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_pre_bidet <- weekly_total_flow_pre_bidet %>%
  top_n(1, WeeklyTotalFlowPreBidet) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_pre_bidet <- highest_total_week_pre_bidet$Year
highest_flow_week_pre_bidet <- highest_total_week_pre_bidet$Week

# Filter the original data for the highest flow week
highest_flow_week_data_pre_bidet <- bidet_pre %>%
  filter(Year == highest_flow_year_pre_bidet & Week == highest_flow_week_pre_bidet)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_pre_bidet <- highest_flow_week_data_pre_bidet %>%
  mutate(WeekdayGroupPreBidet = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPreBidet, Hour) %>%
  summarise(TotalFlowPreBidet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_pre_bidet$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_pre_bidet$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")


# Post Summed Flow DataFrames (Complex) ------------------------------------

# Convert Unix timestamp to datetime
bidet_post$Datetime <- as.POSIXct(bidet_post$Time, origin="1970-01-01", tz="UTC")

# Extract the week of the year, year, hour of the day, and day of the week
bidet_post$Week <- week(bidet_post$Datetime)
bidet_post$Year <- year(bidet_post$Datetime)
bidet_post$Hour <- hour(bidet_post$Datetime)
bidet_post$Weekday <- wday(bidet_post$Datetime, label = TRUE, week_start = 1) # Makes Monday the first day

# Calculate the total flow per week
weekly_total_flow_post_bidet <- bidet_post %>%
  group_by(Year, Week) %>%
  summarise(WeeklyTotalFlowPostBidet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Find the week with the highest total flow
highest_total_week_post_bidet <- weekly_total_flow_post_bidet %>%
  top_n(1, WeeklyTotalFlowPostBidet) %>%
  ungroup()

# Get the year and week number for the highest flow week
highest_flow_year_post_bidet <- highest_total_week_post_bidet$Year
highest_flow_week_post_bidet <- highest_total_week_post_bidet$Week

# Filter the original data for the highest flow week
highest_flow_week_data_post_bidet <- bidet_post %>%
  filter(Year == highest_flow_year_post_bidet & Week == highest_flow_week_post_bidet)

# Calculate the total flow for each hour of each day of the week during the highest flow week
# Group Monday to Friday into one group for totaling
hourly_total_flow_post_bidet <- highest_flow_week_data_post_bidet %>%
  mutate(WeekdayGroupPostBidet = case_when(
    Weekday %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Mon-Fri',
    TRUE ~ as.character(Weekday)
  )) %>%
  group_by(WeekdayGroupPostBidet, Hour) %>%
  summarise(TotalFlowPostBidet = sum(Flow, na.rm = TRUE), .groups = 'drop')

# Create a dummy date with varying hours for plotting
hourly_total_flow_post_bidet$PlotTime <- as.POSIXct(sprintf("2024-01-01 %02d:00:00", hourly_total_flow_post_bidet$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")

# For Cumulative post 2020 --------------------------------------------------------------------------------

# Filtering the original dataset for week number
bidet_post_cum <- bidet_post %>%
  filter(Week == 28)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, bidet_post_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
bidet_post_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# For Cumulative pre 2020 --------------------------------------------------------------------------------
# Filtering the original dataset for week number
bidet_pre_cum <- bidet_pre %>%
  filter(Week == 43)

# Defining a vector of all days of the week in English
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Creating a complete grid of all combinations of days of the week and hours of the day
complete_times <- expand.grid(
  Day = week_days,
  Hour = 0:23  # Assuming the hours are from 0 to 23
)

# Merging the complete times data frame with the original dataset, missing Flow values will be NA
full_data <- left_join(complete_times, bidet_pre_cum, by = c("Day", "Hour"))

# Grouping by day and hour to calculate cumulative flow
# Note: NA values need to be addressed as the original data may have missing values
bidet_pre_WH1 <- full_data %>%
  arrange(Day, Hour) %>%  # Ensure data is ordered by Day and Hour
  group_by(Day, Hour) %>%
  summarise(Flow = ifelse(is.na(Flow), 0, Flow), .groups = 'drop') %>%  # Replace NA with 0
  ungroup() %>%
  group_by(Day) %>%
  mutate(Cumulative_Flow = cumsum(Flow)) %>%  # Calculate cumulative flow
  ungroup()  # Ungroup the data after calculations

# Final bubble map prep-----------------------------

# Month and Week of day vs total flow pre 2020

bidet_pre_WH <- bidet_pre %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

bidet_pre_WH$Day <- factor(bidet_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
bidet_pre_WH$Hour <- factor(bidet_pre_WH$Hour, levels = 0:23)

# Month and Week of day vs total flow post 2020

bidet_post_WH <- bidet_post %>%
  group_by(Day, Hour) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

bidet_pre_WH$Day <- factor(bidet_pre_WH$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
bidet_pre_WH$Hour <- factor(bidet_pre_WH$Hour, levels = 0:23)

# Final heatmap prep-------------------------------------

# Month and Week of day vs total flow post 2020

bidet_post_WM <- bidet_post %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

bidet_post_WM$Month <- factor(bidet_post_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
bidet_post_WM$Day <- factor(bidet_post_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Month and Week of day vs total flow pre 2020

bidet_pre_WM <- bidet_pre %>%
  group_by(Month, Day) %>%
  summarize(SumFlow = sum(Flow, na.rm = TRUE)) %>%
  mutate(SumFlow = ifelse(is.na(SumFlow), 0, SumFlow))

bidet_pre_WM$Month <- factor(bidet_pre_WM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
bidet_pre_WM$Day <- factor(bidet_pre_WM$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
