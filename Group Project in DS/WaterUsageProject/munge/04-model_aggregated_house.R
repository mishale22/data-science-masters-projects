cycle_1_proportion_of_sampling_time_intervals_plot <- ggplot(aggregatedWholeHouse_time_diff_df, aes(x = Var1, y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Interval (minutes)") +
  ylab("Proportion") +
  ggtitle("Plot 1: Proportion of sampling time intervals")

cycle_1_outliers_plot <- ggplot(aggregatedWholeHouse_unusual, aes(x = datetime, y = flow)) +
  geom_point() +  
  xlab("DateTime") +
  ylab("Flow") +
  ggtitle("Plot 2: Outliers")

cycle_1_clean_aggregated_table <- knitr::kable(
  head(aggregatedWholeHouse_part_1_cleaned),
  format = 'pipe',
  caption = 'First segment of clean aggregated whole house time series data')

cycle_1_average_hourly_water_usage_plot <- ggplot(aggregatedWholeHouse_features_hourly_avg_data, aes(x = hour, y = avg_flow)) +
  geom_bar(stat="identity", fill = "steelblue") +
  xlab("Hour of Day") +
  ylab("Average of Flow") +
  ggtitle("Plot 3: Average hourly water usage throughout the day")

cycle_1_average_daily_water_usage_plot <- ggplot(aggregatedWholeHouse_features_weekly_avg_data, aes(x = day_of_week, y = avg_flow)) +
  geom_bar(stat="identity", fill = "steelblue") +
  xlab("Day of Week") +
  ylab("Average of Flow") +
  ggtitle("Plot 4: Average daily water usage throughout the week")

cycle_1_average_monthly_water_usage_plot <- ggplot(aggregatedWholeHouse_features_monthly_avg_data, aes(x = month, y = avg_flow)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  xlab("Month") +
  ylab("Average of Flow") +
  ggtitle("Plot 5: Average monthly water usage")

cycle_1_average_quarterly_water_usage_plot <- ggplot(aggregatedWholeHouse_features_quarterly_avg_data, aes(x = quarter, y = avg_flow)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  xlab("Quarter") +
  ylab("Average of Flow") +
  ggtitle("Plot 6: Average quarterly water usage")

cycle_1_daily_based_time_series_plot <- ggplot(aggregatedWholeHouse_features_daily_data, aes(x = day, y = daily_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Day") +  
  ylab("Sum of Value") + 
  ggtitle("Plot 7: Daily-based time series") 

cycle_1_first_difference_of_daily_time_series_plot <- ggplot(aggregatedWholeHouse_features_daily_data, aes(x = day, y = diff_daily_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Day") +
  ylab("First Difference of Sum") +
  ggtitle("Plot 8: First difference of daily sum time series")

cycle_1_monthly_based_time_series_plot <- ggplot(aggregatedWholeHouse_features_weekly_data, aes(x = week, y = weekly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Week") +
  ylab("Sum of value") +
  ggtitle("Plot 9: Monthly-based time series")

cycle_1_first_difference_of_weekly_time_series_plot <- ggplot(aggregatedWholeHouse_features_weekly_data, aes(x = week, y = diff_weekly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Week") +
  ylab("First Difference of Sum") +
  ggtitle("Plot 10: First difference of weekly sum time series plot")

cycle_1_Hourly_based_time_series_plot <- ggplot(aggregatedWholeHouse_features_hourly_data, aes(x = hour, y = hourly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Hour") +  
  ylab("Sum of Value") + 
  ggtitle("Plot 1: Hourly-based time series") 

cycle_1_first_difference_of_hourly_time_series_plot <- ggplot(aggregatedWholeHouse_features_hourly_data, aes(x = hour, y = diff_hourly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Hour") +
  ylab("First Difference of Sum") +
  ggtitle("Plot 2: First difference of hourly time series")

cycle_1_monthly_based_time_series_plot <- ggplot(aggregatedWholeHouse_features_monthly_data, aes(x = month, y = monthly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Month") +
  ylab("Sum of value") +
  ggtitle("Plot 3: Monthly-based time series")

cycle_1_first_difference_of_monthly_time_series_plot <- ggplot(aggregatedWholeHouse_features_monthly_data, aes(x = month, y = diff_monthly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Month") +
  ylab("First Difference of Sum") +
  ggtitle("Plot 4: First difference of monthly sum time series")

cycle_1_quarterly_based_time_series_plot <- ggplot(aggregatedWholeHouse_features_quarterly_data, aes(x = quarter, y = quarterly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Quarter") +
  ylab("Sum of value") +
  ggtitle("Plot 5: Quarterly-based time series")

cycle_1_first_difference_of_quarterly_time_series_plot <- ggplot(aggregatedWholeHouse_features_quarterly_data, aes(x = quarter, y = diff_quarterly_sum)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(colour = 'steelblue') + 
  xlab("Quarter") +
  ylab("First Difference of Sum") +
  ggtitle("Plot 6: First difference of quarterly sum time series")
