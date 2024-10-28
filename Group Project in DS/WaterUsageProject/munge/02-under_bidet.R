# Initial Data Plot -------------------------------------------------------

# Plot the time series flow (2019-2020)
bidet_time_series_flow_plot = ggplot(feedBidet, aes(x = Time, y = Flow)) +
  geom_line(color = "darkorchid3", linewidth = 0.3) +
  labs(x = "Date", y = "Time series Flow", title = "Time Series Flow (2019-2020)")

# Plot flow distribution
flow_distribution_plot <- ggplot(feedBidet, aes(x = Flow)) +
  geom_histogram(binwidth=2.5, fill="darkorchid3", color="black") +
  labs(x = "Flow", y = "Frequency", title = "Distribution of Flow Values")