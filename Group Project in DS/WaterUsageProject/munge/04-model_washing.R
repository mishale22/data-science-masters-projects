# Pre Summed Flow Plots ------------------------------------------------------------

# plot monthly flow
washing_pre_monthly_flow_plot = ggplot(washing_pre_monthly_flow, aes(x = Month, y = Monthly_Flow, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Total Monthly Flow", title = "Washing Machine Monthly Flow")  +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# plot weekly flow
washing_pre_weekly_flow_plot = ggplot(washing_pre_weekly_flow, aes(x = Week, y = Weekly_Flow, fill = as.factor(Month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Week", y = "Total Weekly Flow", title = "Washing Machine Weekly Flow", fill = 'Month') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# Plot daily flow
washing_pre_daily_flow_plot = ggplot(washing_pre_daily_flow, aes(x = Day, y = Daily_Flow, fill = as.factor(Day))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day", y = "Total Daily Flow", title = "Washing Machine Daily Flow", fill = 'Day') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# plot hourly flow
washing_pre_hourly_flow_plot = ggplot(washing_pre_hourly_flow, aes(x = Hour, y = Hourly_Flow)) +
  geom_line(color = "darkorchid3", linewidth = 1) +
  geom_point(colour = 'darkorchid4', size = 2) +
  labs(x = "Hour", y = "Total Hourly Flow", title = "Washing Machine Hourly Flow") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# Post Summed Flow Plots ------------------------------------------------------------

# plot monthly flow
washing_post_monthly_flow_plot = ggplot(washing_post_monthly_flow, aes(x = Month, y = Monthly_Flow, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Total Monthly Flow", title = "Washing Machine Monthly Flow") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# plot weekly flow
washing_post_weekly_flow_plot = ggplot(washing_post_weekly_flow, aes(x = Week, y = Weekly_Flow, fill = as.factor(Month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Week", y = "Total Weekly Flow", title = "Washing Machine Weekly Flow", fill = 'Month') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# Plot daily flow
washing_post_daily_flow_plot = ggplot(washing_post_daily_flow, aes(x = Day, y = Daily_Flow, fill = as.factor(Day))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day", y = "Total Daily Flow", title = "Washing Machine Daily Flow", fill = 'Day') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# plot hourly flow
washing_post_hourly_flow_plot = ggplot(washing_post_hourly_flow, aes(x = Hour, y = Hourly_Flow)) +
  geom_line(color = "darkorchid3", linewidth = 1) +
  geom_point(colour = 'darkorchid4', size = 2) +
  labs(x = "Hour", y = "Total Hourly Flow", title = "Washing Machine Hourly Flow") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

# Pre Summed Flow Plots  (Complex) ------------------------------------------------------------

# Define custom colors based on the provided image
custom_colors <- c('Mon-Fri' = '#404040',  # Dark grey for weekdays
                   'Sat' = '#4F81BD',      # Light blue for Saturday
                   'Sun' = '#F4C842')      # Golden for Sunday

# Plot for the highest flowing week with only three lines for Saturday, Sunday, and Mon-Fri
complex_plot1_pre_washing = ggplot(hourly_total_flow_pre_washing, aes(x = PlotTime, y = TotalFlowPreWashing, group = WeekdayGroupPreWashing, color = WeekdayGroupPreWashing)) +
  geom_line(size = 0.8) +  # Thicker lines for better visibility
  scale_x_datetime(labels = date_format("%l%p"), breaks = date_breaks("2 hours")) +  # Adjusted for less cluttered x-axis
  scale_y_continuous(labels = scales::comma) +  # Cleaner y-axis labels
  scale_color_manual(values = custom_colors) +  # Specified custom colors
  labs(title = paste("2019 Analysis of Washing Machine Water Usage for the\nWeek with Highest Flow (Week ", highest_flow_week_pre_washing, " of ", highest_flow_year_pre_washing, ")", sep = ""),
       x = "Time of the Day",
       y = "Total Flow",
       color = "Day of Week") +
  theme_classic() +  # Cleaner theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  # Move legend to the bottom

#Bubble map pre 2020

washing_pre_WH_plot <- ggplot(washing_pre_WH, aes(x=Hour, y=Day, size=SumFlow, colour=SumFlow)) +
  geom_point(alpha=0.7) + # alpha is used to adjust transparency
  scale_size(range = c(1 ,10),breaks=c(50000, 100000, 150000)) + # range is used to set the range of circle sizes
  scale_color_gradient(low="lightblue", high="darkblue", guide = guide_colorbar(barheight = 4)) + # set the colour gradient
  theme_minimal() + # set a minimal theme
  labs(title='2019 Pattern of Hourly Washing Machine\nWater Usage Across Different Weekdays', x='Hour', y='Day of the Week') +
  theme(legend.position="right", # place the legend on the right side of the diagram
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

#  Washing Hourly water usage in different weekday for the highest week

washing_pre_WH1_plot <- ggplot(washing_pre_WH1, aes(x = Hour, y = Cumulative_Flow, group = Day, color = Day)) +
  geom_step() +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  labs(title = "2019 Washing Machine Water Usage by Hour in the\nHighest Usage Week (Week 43 of 2019)",x = "Hour of the Day", y = "Cumulative Flow", color = "Day of the Week") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

#Heat map pre 2020

colors <- c("white", "red", "darkred")
values <- scales::rescale(c(min(washing_pre_WM$SumFlow), mean(washing_pre_WM$SumFlow), max(washing_pre_WM$SumFlow)))

washing_pre_WM_plot <- ggplot(washing_pre_WM, aes(x = Day , y = Month, fill = SumFlow)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = colors, values = values, na.value = "white") +
  labs(title = "2019 Pattern of Daily Washing Machine Water\nUsage Across Different Months", x = "Day of the Week", y = "Month") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Post Summed Flow Plots  (Complex) ------------------------------------------------------------

# Define custom colors based on the provided image
custom_colors <- c('Mon-Fri' = '#404040',  # Dark grey for weekdays
                   'Sat' = '#4F81BD',      # Light blue for Saturday
                   'Sun' = '#F4C842')      # Golden for Sunday

# Plot for the highest flowing week with only three lines for Saturday, Sunday, and Mon-Fri
complex_plot1_post_washing = ggplot(hourly_total_flow_post_washing, aes(x = PlotTime, y = TotalFlowPostWashing, group = WeekdayGroupPostWashing, color = WeekdayGroupPostWashing)) +
  geom_line(size = 0.8) +  # Thicker lines for better visibility
  scale_x_datetime(labels = date_format("%l%p"), breaks = date_breaks("2 hours")) +  # Adjusted for less cluttered x-axis
  scale_y_continuous(labels = scales::comma) +  # Cleaner y-axis labels
  scale_color_manual(values = custom_colors) +  # Specified custom colors
  labs(title = paste("2020 Analysis of Washing Machine Water Usage for the\nWeek with Highest Flow (Week ", highest_flow_week_post_washing, " of ", highest_flow_year_post_washing, ")", sep = ""),
       x = "Time of the Day",
       y = "Total Flow",
       color = "Day of Week") +
  theme_classic() +  # Cleaner theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  # Move legend to the bottom

#Bubble map post 2020

washing_post_WH_plot <- ggplot(washing_post_WH, aes(x=Hour, y=Day, size=SumFlow, colour=SumFlow)) +
  geom_point(alpha=0.7) + # alpha is used to adjust transparency
  scale_size(range = c(1 ,10),breaks=c(50000, 100000, 150000)) + # range is used to set the range of circle sizes
  scale_color_gradient(low="lightblue", high="darkblue", guide = guide_colorbar(barheight = 4)) + # set the colour gradient
  theme_minimal() + # set a minimal theme
  labs(title='2020 Pattern of Hourly Washing Machine\nWater Usage Across Different Weekdays', x='Hour', y='Day of the Week') +
  theme(legend.position="right", # place the legend on the right side of the diagram
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

#  Washing  Hourly water usage in different weekday for the highest week

washing_post_WH1_plot <- ggplot(washing_post_WH1, aes(x = Hour, y = Cumulative_Flow, group = Day, color = Day)) +
  geom_step() +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  labs(title = "2020 Washing Machine Water Usage by Hour in the\nHighest Usage Week (Week 40 of 2020)",x = "Hour of the Day", y = "Cumulative Flow", color = "Day of the Week") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1))

#Heat map post 2020

colors <- c("white", "lightblue", "darkblue")
values <- scales::rescale(c(min(washing_post_WM$SumFlow), mean(washing_post_WM$SumFlow), max(washing_post_WM$SumFlow)))

washing_post_WM_plot <- ggplot(washing_post_WM, aes(x = Day, y = Month, fill = SumFlow)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = colors, values = values, na.value = "white") +
  labs(title = "2020 Pattern of Daily Washing Machine Water\nUsage Across Different Months", x = "Day of the Week", y = "Month") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
