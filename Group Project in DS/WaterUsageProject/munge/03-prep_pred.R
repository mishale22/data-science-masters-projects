# Clean Data -------------------------------------------------------------

agg = agg %>% rename(Time = unix, Flow = flow)

# remove outliers (1970 data)
agg = agg[agg$Time > 1.5e+09, ]

# remove duplicates

# unix timestamps to datetime
agg$Time = as.POSIXct(agg$Time, origin = "1970-01-01")
agg = agg[order(agg$Time), ]

# missing values
any(is.na(agg$Flow)) # none

# missing 8 months of data
# could view separately

# Separate Data -----------------------------------------------------------

# split the data
agg_pre = agg[agg$Time < '2020-01-01', ]
agg_post = agg[agg$Time >= '2020-01-01', ]

# plot split data
plot_pre2020 = ggplot(data = agg_pre, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Aggregated Water Flow Pre-2020")

plot_post2020 = ggplot(data = agg_post, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Aggregated Water Flow Post-2020") 

# Interpolate Missing Values ----------------------------------------------

# generate missing seconds
missing_seconds_agg_pre = data.frame(Time = seq(min(agg_pre$Time), max(agg_post$Time), by = 1))

# left join pre 2020
agg_pre_old = agg_pre
agg_pre = missing_seconds_agg_pre %>%
  left_join(agg_pre, by = "Time") %>%
  mutate(Flow = ifelse(is.na(Flow), approx(agg_pre$Time, agg_pre$Flow, xout = Time)$y, Flow))

# generate missing seconds
missing_seconds_agg_post = data.frame(Time = seq(min(agg_post$Time), max(agg_post$Time), by = 1))

# left join post 2020
agg_post_old = agg_post
agg_post = missing_seconds_agg_post %>%
  left_join(agg_post, by = "Time") %>%
  mutate(Flow = ifelse(is.na(Flow), approx(agg_post$Time, agg_post$Flow, xout = Time)$y, Flow))

# Data Collection Frequency Histogram --------------------------------------------

# entry frequency
data_freq = ggplot(sink, aes(x = Time)) +
  geom_histogram(bins = 100, fill = "grey", color = "black") +
  labs(x = "Time", y = "Frequency", title = "Data Collection Frequency")

# Add Time Features -------------------------------------------------------

agg_pre = agg_pre %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

agg_post = agg_post %>%
  mutate(Month = month(Time, label = TRUE)) %>%
  mutate(Week = week(Time)) %>%
  mutate(Day = weekdays(Time)) %>%
  mutate(Hour = hour(Time))

# Prediction DataFrames ---------------------------------------------------

agg_pre_hour = agg_pre %>%
  mutate(Time = floor_date(Time, "hour")) %>%
  group_by(Time) %>%
  summarize(Tot_Flow = sum(Flow))

agg_post_hour = agg_post %>%
  mutate(Time = floor_date(Time, "hour")) %>%
  group_by(Time) %>%
  summarize(Tot_Flow = sum(Flow))

# remove missing values
agg_pre_hour = na.omit(agg_pre_hour)

# TimeSeries Data ---------------------------------------------------------

train_ts = zoo(agg_pre_hour$Tot_Flow, order.by = agg_pre_hour$Time)
test_ts = zoo(agg_post_hour$Tot_Flow, order.by = agg_post_hour$Time)
