#====================================================================#
# Title: Day 1, script 5                                             #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides.

# Restart R again! Then write what you need at the start of this script

# Here's a plot to start you off. Try to figure out what each line is doing.
ggplot(data = nlsy) +
  geom_boxplot(aes(x = factor(sleep_wknd), 
                   y = sleep_wkdy,
                   fill = factor(sleep_wknd))) +
  scale_fill_discrete(guide = FALSE) +
  labs(x = "hours slept on weekends",
       y = "hours slept on weekends",
       title = "The more people sleep on weekends, 
the more they sleep on weekdays",
       subtitle = "According to NLSY data") +
  theme_classic()

# Make a ggplot, any ggplot! Find out a theme you like and use it.
# For inspiration with geoms that we didn't cover, you may want to check
# out this site: http://shiny.stat.ubc.ca/r-graph-catalog/



# save your plot
# These are probably not the right dimensions for your plot, so
# experiment! You can also change from pdf to e.g., png 
# by changing the file name
ggsave(filename = "my_plot.pdf", height = 8, width = 4)

# that function will automatically save the last plot you made
# if you're making lots, you should store them and refer to them by name
# notice what happens in the plots window when you run this
new_plot <- ggplot(nlsy) +
  geom_bar(aes(x = age_bir))

ggsave(plot = new_plot, filename = "another_plot.png")

# Finally, try to recreate the plot from the slides!