# Initial Data Plot 
initial_plot_kitchen = ggplot(data = kitchen_faucet, aes(x = Time, y = Flow)) +
  geom_line() +
  labs(x = "Time", y = "Flow", title = "Initial Time vs Flow")
initial_plot_kitchen

# Stats
median_flow_kitchen = median(kitchen_faucet$Flow) # 0
avg_flow_kitchen = mean(kitchen_faucet$Flow) # 19.52554

# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_flow_kitchen = getmode(kitchen_faucet$Flow) # 0 

# Histogram of Flow
hist_kitchen = ggplot(kitchen_faucet, aes(x = Flow)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of Flow in the Kitchen Faucet") +
  labs(y = "Count", x = "Flow")
hist_kitchen

# Findings:
# there is a gap between some dates
# unix have to be converted to dates
# the most repeated value is 0 