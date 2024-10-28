#importing data from feedtoilet.csv
data_toilet <- toilet
head(data_toilet)


#Converting the time-stamp to readable format and storing it in the data file

data_toilet$Time <- as.POSIXct(data_toilet$Time, origin="1970-01-01")
data_toilet$EndFlow <- as.POSIXct(data_toilet$EndFlow, origin="1970-01-01")
head(data_toilet)

#created an new column to check the duration of the water flow
data_toilet$Duration <- data_toilet$EndFlow - data_toilet$Time
head(data_toilet)

#understanding plot in under file______________________(basic_plot_1)
# Add a cumulative count for each event occurrence(for Data Understanding)
data_toilet <- data_toilet %>%
  arrange(Time) %>%
  mutate(EventCount = row_number())

#2nd understanding plot  in the under file_____________(basic_plot_2)
# Create a new column for the date without the time
data_toilet$Date <- as.Date(data_toilet$Time)

# Count the number of events per day
daily_event_counts <- data_toilet %>%
  group_by(Date) %>%
  summarise(DailyCount = n())

#split the data as pre_2020 and post_2020
data_pre_2020 <- data_toilet %>%
  filter(as.Date(Time) < as.Date("2020-01-01"))

data_post_2020 <- data_toilet %>%
  filter(as.Date(Time) >= as.Date("2020-01-01"))
#-------------------------------------------------------------------
#analysis based on the sum of the events occured for Month, week, days and hours

# Monthly counts for pre 2020
monthly_counts_pre_2020 <- data_pre_2020 %>% 
  group_by(Month = floor_date(Time, unit = "month")) %>%
  summarize(Count = n())

monthly_counts_pre_2020$Month <- factor(monthly_counts_pre_2020$Month)

# Monthly counts for post 2020
monthly_counts_post_2020 <- data_post_2020 %>% 
  group_by(Month = floor_date(Time, unit = "month")) %>%
  summarize(Count = n())

monthly_counts_post_2020$Month <- factor(monthly_counts_post_2020$Month)
#-------------------------------------------------------------------
# Weekly counts for pre 2020
weekly_counts_pre_2020 <- data_pre_2020 %>% 
  mutate(Month = floor_date(Time, unit = "month")) %>%
  group_by(Month, Week = floor_date(Time, unit = "week")) %>%
  summarize(Count = n())

# Weekly counts for post 2020
weekly_counts_post_2020 <- data_post_2020 %>% 
  mutate(Month = floor_date(Time, unit = "month")) %>%
  group_by(Month, Week = floor_date(Time, unit = "week")) %>%
  summarize(Count = n())

#daily count for pre 2020

# Ensure that the Time column is in POSIXct format
data_pre_2020$Time <- as.POSIXct(data_pre_2020$Time, origin="1970-01-01")

# Create a new column for the day of the week
data_pre_2020$Weekday <- wday(data_pre_2020$Time, label = TRUE, abbr = FALSE)

# Count the number of events per weekday
weekday_event_counts_pre <- data_pre_2020 %>%
  group_by(Weekday) %>%
  summarise(Count = n())

# Order the weekdays
weekday_event_counts_pre$Weekday <- factor(weekday_event_counts_pre$Weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#daily count for post 2020

# Ensure that the Time column is in POSIXct format
data_post_2020$Time <- as.POSIXct(data_post_2020$Time, origin="1970-01-01")

# Create a new column for the day of the week
data_post_2020$Weekday <- wday(data_post_2020$Time, label = TRUE, abbr = FALSE)

# Count the number of events per weekday
weekday_event_counts_post <- data_post_2020 %>%
  group_by(Weekday) %>%
  summarise(Count = n())

# Order the weekdays
weekday_event_counts_post$Weekday <- factor(weekday_event_counts_post$Weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#----------------------------------------------------------------------
#Hourly analysis

# Extract hour from the Time column
data_pre_2020$Hour <- hour(data_pre_2020$Time)

# Group by hour and calculate count
hourly_counts_pre <- data_pre_2020 %>%
  group_by(Hour) %>%
  summarize(Count = n())


# Extract hour from the Time column
data_post_2020$Hour <- hour(data_post_2020$Time)

# Group by hour and calculate count
hourly_counts_post <- data_post_2020 %>%
  group_by(Hour) %>%
  summarize(Count = n())