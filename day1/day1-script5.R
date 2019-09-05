#====================================================================#
# Title: Day 1, script 5                                             #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides.

# Restart R again! Then write what you need at the start of this script


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