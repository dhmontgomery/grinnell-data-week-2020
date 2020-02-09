# Reproducing images from this article: https://www.citylab.com/transportation/2019/11/vision-zero-data-traffic-deaths-pedestrians-cyclist-safety/601831/

# Load libraries
library(tidyverse)
library(scales)

# Load in data on traffic fatalities in major US cities
# Data from CityLab under a CC BY 4.0 license

traffic_deaths <- read_csv("https://raw.githubusercontent.com/theatlantic/citylab-data/master/vision-zero/vision-zero-fatalities-type.csv")

# Graph overall data
# Note that the output here is slightly different from the published version, which used a non-public ggplot theme and color palette

traffic_deaths %>% # Start with data
	filter(year >= 2012) %>% # Exclude early years for which not all cities have data
	# Combine deaths by mode into overall deaths
	group_by(city, year) %>%
	summarize(fatalities_percap = sum(fatalities_percap)) %>%
	# Graph
	ggplot(aes(x = year, y = fatalities_percap, color = city)) +
	geom_line(size = 2) + 
	geom_point(size = 3) +
	# Add text label
	geom_text(data = . %>% filter(year == 2018), # Label only at rightmost year
			  aes(label = city), 
			  hjust = 0, # Align left
			  nudge_x = 0.1, # Create separation from dots
			  size = 7) +
	scale_x_continuous(expand = expand_scale(mult = c(0.05, 0.3))) + # Add lots of space for labels on the right side of the graph
	scale_color_brewer(palette = "Dark2") + # Pick a pretty color palette
	theme_minimal() + # Pick a theme
	theme(legend.position = "none", # Hide the unnecessary legend
		  axis.title.x = element_blank()) + # Hide the unnecessary "Years" axis title
	labs(title = "Traffic fatalities, 2012-2018",
		 caption = "Source: New York City Vision Zero View, IDOT, DDOT, LADOT, San Francisco Vision Zero (David H. Montgomery/CityLab)",
		 y = "Fatalities per 100,000 residents")

####

# Graph one individual city

traffic_deaths %>% # Start with our data
	filter(city == "New York") %>% # Filter to just NYC
	ggplot(aes(x = year, y = fatalities_percap, color = type)) + # Here we set color by type of fatality rather than city
	geom_vline(aes(xintercept = vz_year), linetype = 3) + # Add a vertical line for when Vision Zero was adopted
	geom_line(size = 2) +
	geom_point(size = 3) +
	# Label the three lines
	geom_text(data = . %>% filter(year == 2014), # Pick an x-axis spot for the labels
			  # This I did manually, seeking a place where all three lines were separated, & flat or downsloped
			  aes(label = type), size = 8, vjust = -0.4, hjust = -0.05, fontface = "bold") +
	# Label the Vision Zero line
	# Note that this code is reusable for any city, as opposed to specifying "x = 2014"
	geom_text(aes(x = vz_year, y = 3.4), label = "<- Vision Zero adopted", hjust = -.02, color = "black", size = 6) +
	scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), # Give a little space at the top, but none at the bottom 
					   limits = c(0, 3.5), # Set a fixed upper limit for comparison with other cities
					   breaks = seq(0, 4, 1)) + # Set breaks
	theme_minimal() +
	theme(axis.title.x = element_blank(),
		  legend.position = "none") +
	labs(title = "New York City traffic deaths, 2012-2018",
		 caption = "Source: NYC Open Data/NYPD (David H. Montgomery/CityLab)",
		 y = "Traffic fatalities per 100,000 residents")

# This might not look right in your preview box, but try clicking "Zoom" for a bigger view. y
# You can either adjust the image's size (via ggsave) to make the spacing work, or pick a size you like and adjust the graph's parameters to fit.

######

# Graph another city: Chicago
# This is a little more complicated, because there's not a good spot to concisely direct-label each line
# So I write some code to automatically put the label above or below the line, depending on the line's slope

traffic_deaths %>% 
	filter(city == "Chicago") %>% 
	# Calculating slope
	group_by(type) %>% # We want to calculate slope within each group of types
	mutate(change = lead(fatalities_percap) / fatalities_percap - 1) %>% # Divide each data point by the subsequent datapoint, and subtract one
	ggplot(aes(x = year, y = fatalities_percap, color = type)) + 
	geom_vline(aes(xintercept = vz_year), linetype = 3) +
	geom_line(size = 2) +
	geom_point(size = 3) +
	# The fancier code for labeling this graph
	geom_text(data = . %>% filter(year == 2017), # Label at 2017 instead of 2014, because it works better here
			  # Use case_when() to choose vjust based on slope
			  aes(label = type, vjust = case_when(change < -0.3 ~ -0.4,
			  									change < 0 ~ -.4,
			  									TRUE ~ 1.2)),
			  size = 8,  hjust = -0.05, fontface = "bold") +
	geom_text(aes(x = vz_year, y = 3.4), label = "<- Vision Zero adopted", hjust = -.02, color = "black", size = 6) +
	scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), 
					   limits = c(0, 3.5), 
					   breaks = seq(0, 4, 1)) + 
	theme_minimal() +
	theme(axis.title.x = element_blank(),
		  legend.position = "none") +
	# Be sure to update the labels!
	labs(title = "Chicago traffic deaths, 2012-2018",
		 caption = "Source: Chicago Police Department/Illinois Department of Transportation (David H. Montgomery/CityLab)",
		 y = "Traffic fatalities per 100,000 residents")

####
# Other ideas: 
# Try making individual graphs for the other cities! 
# You might also explore the best way to make a faceted graph with mode 
