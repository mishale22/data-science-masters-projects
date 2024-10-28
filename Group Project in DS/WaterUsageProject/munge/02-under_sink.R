# Initial Data Plot -------------------------------------------------------

# plot data
plot1 = ggplot(data = sink, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Time vs Flow")

# outliers at time 0
# should convert to datetime
