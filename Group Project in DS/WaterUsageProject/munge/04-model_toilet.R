# Model for toilets 

# Pre-2020 Monthly Flow Plot
toilet_pre_monthly_flow_plot <- ggplot(monthly_counts_pre_2020, aes(x = Month, y = Count, fill = Month)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Month", y = "Total Monthly Flow Frequncy", title = "2019 Monthly Analysis of Toilet Flow") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

toilet_pre_monthly_flow_plot

# Post-2020 Monthly Flow Plot
toilet_post_monthly_flow_plot <- ggplot(monthly_counts_post_2020, aes(x = Month, y = Count, fill = Month)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Month", y = "Total Monthly Flow Frequncy", title = "2020 Monthly Analysis of Toilet Flow") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

toilet_post_monthly_flow_plot

#---------------------------------------------------#
#weekly plots

# Pre-2020 Weekly Flow Plot without x-axis labels
toilet_pre_weekly_flow_plot <- ggplot(weekly_counts_pre_2020, aes(x = Week, y = Count, fill = as.factor(Month))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = NULL, y = "Weekly Flow Frequency", title = "2019 Weekly Analysis of Toilet Flow", fill = 'Month') +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

toilet_pre_weekly_flow_plot

toilet_post_weekly_flow_plot <- ggplot(weekly_counts_post_2020, aes(x = Week, y = Count, fill = as.factor(Month))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = NULL, y = "Weekly Flow Frequency", title = "2020 Weekly Analysis of Toilet Flow", fill = 'Month') +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

toilet_post_weekly_flow_plot

#-----------------------------------------------------#
#Daily plots

# Function to adjust plot aesthetics

daily_flow_plot_pre = ggplot(weekday_event_counts_pre, aes(x = Weekday, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Day of the Week") +
  ylab("Count of Occurrences") +
  ggtitle("Number of Events per Day of the Week") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

daily_flow_plot_post = ggplot(weekday_event_counts_post, aes(x = Weekday, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Day of the Week") +
  ylab("Count of Occurrences") +
  ggtitle("Number of Events per Day of the Week") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


adjust_daily_plot <- function(plot) {
  plot +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(x = "Day of the Week", y = "Frequency", title = "Number of Occurence per Day of the Week") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),  
          panel.grid.major = element_blank())  
}

# Pre-2020 Daily Flow Plot with enhanced aesthetics
toilet_pre_daily_flow_plot <- adjust_daily_plot(daily_flow_plot_pre)

toilet_pre_daily_flow_plot

# Post-2020 Daily Flow Plot with enhanced aesthetics
toilet_post_daily_flow_plot <- adjust_daily_plot(daily_flow_plot_post)

toilet_post_daily_flow_plot
#--------------------------------------------------------#
#Hourley plots

toilet_flow_plot_pre <- ggplot(hourly_counts_pre, aes(x = Hour, y = Count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "2019 Hourly Event Count Analysis",
       x = "Hour of the Day",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Post-2020 Hourly Flow Plot
toilet_flow_plot_post <- ggplot(hourly_counts_post, aes(x = Hour, y = Count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "2020 Hourly Event Count Analysis",
       x = "Hour of the Day",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

adjust_plot <- function(plot) {
  plot +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(title = "Hourly Event Count Analysis",
         x = "Hour of the Day",
         y = "Frequency") +
    scale_x_continuous(breaks = seq(0, 23, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour = "gray", size = 0.5),
          panel.grid.minor = element_blank())
}

# Pre-2020 Hourly Flow Plot with enhanced aesthetics
toilet_pre_hourly_flow_plot <- adjust_plot(toilet_flow_plot_pre)

toilet_pre_hourly_flow_plot 

# Post-2020 Hourly Flow Plot with enhanced aesthetics
toilet_post_hourly_flow_plot <- adjust_plot(toilet_flow_plot_post)

toilet_post_hourly_flow_plot




























