# Initial Data Plot -------------------------------------------------------

# plot data

washing_plot1 <- plot_ly(data = washing, x = ~Time, y = ~Flow, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Time vs Flow of Washing Machine')
washing_plot1

washing_plot2 <- plot_ly(data = washing, x = ~Time, y = ~Flow, type = 'scatter') %>%
  layout(title = 'distribution of Time vs Flow of Washing Machine')
washing_plot2
# outliers at time 0
# should convert to datetime
