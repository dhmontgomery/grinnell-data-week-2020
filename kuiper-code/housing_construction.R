library(tidyverse)

# Load dataset of housing construction permits, provided by Apartment List
# Data is provided by metro area, or "core-based statistical area"/"CBSA"
# Columns: 
	# cbsa_code: unique identifier for metro area
	# cbsa_name_full: name for metro area 
	# year: year of data
	# permits_sf: single-family permits 
	# permits_mf: multi-family permits (one permit = one unit, not one building)
	# permits_tot: total housing permits
	# population: total population of metro area in given year
	# permits_per_capita: (permits_tot / population) * 1000

metro_permits_annual <- read_csv("https://github.com/dhmontgomery/grinnell-data-week-2020/blob/master/r-for-data-journalism/metro-permits-annual.csv?raw=true")


# Create a new "shortname" column, which drops state identifiers and secondary cities from metro names
# We do this by spliting on either a "-" or a ",", using the "|" as an "or" operator, and keeping only the first column

metro_permits_annual <- metro_permits_annual %>%
	separate(cbsa_name_full, "shortname", sep = "-|,", remove = FALSE, extra = "drop")

# Graph one city's total permits

metro_permits_annual %>% 
	filter(shortname == "New York") %>%
	ggplot(aes(x = year, y = permits_tot)) +
	geom_col() +
	scale_y_continuous(labels = comma, expand = expand_scale(mult = c(0, 0.05))) +
	scale_x_continuous(expand = expand_scale(mult = 0)) +
	theme_minimal() +
	theme(axis.title.x = element_blank()) +
	labs(title = "NYC housing permits issued, 1990-2018",
		 caption = "Source: Apartment List (David H. Montgomery/CityLab)",
		 y = "Housing permits issued per year")

# Split out single- and multi-family units

metro_permits_annual %>% 
	filter(shortname == "New York") %>%
	rename("Multifamily" = permits_mf,
		   "Single family" = permits_sf) %>%
	gather(key = "type", value = "permits", 5:6) %>%
	ggplot(aes(x = year, y = permits, fill = type)) +
	geom_col() +
	scale_y_continuous(labels = comma, expand = expand_scale(mult = c(0, 0.05))) +
	scale_x_continuous(expand = expand_scale(mult = 0)) +
	scale_fill_brewer(palette = "Dark2") +
	theme_minimal() +
	theme(axis.title.x = element_blank()) +
	labs(title = "NYC housing permits issued, 1990-2018",
		 caption = "Source: Apartment List (David H. Montgomery/CityLab)",
		 y = "Housing permits issued per year", 
		 fill = "Type of home")



+
	theme(strip.text = element_text(size = 15),
		  axis.text.x = element_text(angle = 45),
		  axis.text.y = element_blank(),
		  axis.ticks.x = element_line(),
		  axis.title.x = element_blank(),
		  panel.grid.major.y = element_blank(),
		  panel.grid.minor.y = element_blank()) +
	

p <- metro_permits_annual %>%
	mutate(`Single-family home permits` = permits_sf / population * 1000,
		   `Multi-family home permits` = permits_mf / population * 1000) %>%
	gather(key = "type", value = "permits", 10:11) %>%
	ggplot(aes(x = year, y = permits, fill = type, color = type)) +
	# geom_line() +
	geom_col() +
	scale_fill_manual(values = citylab_colors[c(9, 7)]) +
	scale_color_manual(values = citylab_colors[c(9, 7)]) +
	scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
	scale_x_continuous(expand = expand_scale(mult = c(0, 0))) +
	facet_wrap(vars(shortname), ncol = 5, scales = "free_y") +
	theme(strip.text = element_text(size = 15),
		  axis.text.x = element_text(angle = 45),
		  axis.text.y = element_blank(),
		  axis.ticks.x = element_line(),
		  axis.title.x = element_blank(),
		  panel.grid.major.y = element_blank(),
		  panel.grid.minor.y = element_blank()) +
	labs(title = "Housing permits per capita issued, 1990-2018",
		 caption = "Source: Apartment List (David H. Montgomery/CityLab)",
		 y = "Housing permits per 1,000 residents issued per year",
		 fill = "", color = "")

