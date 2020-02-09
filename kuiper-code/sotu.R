# Code to replicate the data analysis and visualization from this CityLab story: https://www.citylab.com/equity/2019/02/state-of-the-union-analysis-cities-data-vis/582136/
# Note that the code below will produce slightly different graphs than the original story, which used CityLab's non-public ggplot theme and color palettes

# Load libraries
library(tidyverse)
library(scales) # For pretty chart axis labels
library(stringr) # For text analysis

# Load our data: texts of every State of the Union from 1790 through 2019, and metadata about those addresses

# Load the metadata
sotu_meta <- read_csv("https://github.com/dhmontgomery/grinnell-data-week-2020/raw/master/kuiper-code/sotu_data/sotu_metadata.csv")

# The raw text is stored in R's RDS format to save space
# This requires a few extra steps to load

fileurl <- "https://github.com/dhmontgomery/grinnell-data-week-2020/blob/master/kuiper-code/sotu_data/sotu_text.rds?raw=true"

sotu_text <- url(fileurl) %>% gzcon() %>% readRDS()

# Analyze our data for SOTU mentions of words about cities
urban_words <- sotu_meta %>% # Start with our metadata
	# Bind together two new columns: one counting words about cities, the other counting total words
	bind_cols(str_count(sotu_text, "(?i) city| cities| urban") %>% # Some regex to find what we want
			  	as_data_frame() %>% # This outputs as a vector; convert it to a data frame
			  	set_names("urban_words"), # Label the column
			  str_count(sotu_text, "\\w+") %>% # Count all words, with regex
			  	as_data_frame() %>% 
			  	set_names("total_words")) %>%
	# Some years saw multiple SOTUs
	# Sometimes both an outgoing & incoming president gave an address.
	# A few times a president gave two SOTUs — sometimes one written and one spoken
	# Let's collapse those so we have no more than one address per year per party
	group_by(party, year) %>%
	summarize(urban_words = sum(urban_words), # 
			  total_words = sum(total_words)) %>%
	ungroup() %>%
	# Clean our data
	mutate(urban_pct = urban_words/total_words, # Go from counts to percentages
		   # Relabel some of the obsolete party labels in our dataset
		   party = str_replace_all(party, "Nonpartisan", "Federalist") %>% str_replace_all("Democratic-Republican", "Democratic"))

# Graph our urban words dataset
urban_words %>%
	ggplot(aes(x = year, y = urban_pct, fill = party, color = party)) +
	# We have a choice: when presidents of different parties gave addresses in the same year, do we stack them (implying higher use of urban words) or show only the taller one?
	geom_col(position = "identity", size = .2) + # position = "identity" overlaps the columns to show only the taller
	# Set position = "stack" for a different look at a few years, such as 1961
	scale_y_continuous(labels = percent_format(accuracy = 0.1), # Format our scale as percents
					   expand = expand_scale(add = c(0, 0.0001))) + # Remove space at the bottom of graph, add a little at top
	scale_x_continuous(breaks = seq(1800, 2020, 20)) + # Set breaks 
	# Define colors for our four parties
	scale_fill_manual(values = c("darkblue", "darkorange", "darkred", "darkgreen")) +
	scale_color_manual(values = c("darkblue", "darkorange", "darkred", "darkgreen")) +
	theme_minimal() +
	theme(axis.title.x = element_blank(), # Hide unnecessary "Year" x-axis title
		  axis.ticks.x = element_line()) + # add x-axis ticks for legibility
	# add labels
	labs(title = "Use of 'city,' 'cities,' or 'urban' in States of the Union",
		 caption = "Source: The American Presidency Project (David H. Montgomery/CityLab)",
		 y = "Percent of all words",
		 fill = "Party", color = "Party")

####

# Now the same, but for mentions of "rural" and "farm"

rural_words <- bind_cols(sotu_meta, 
		  str_count(sotu_text, "(?i) rural| farm") %>% as_data_frame() %>% set_names("urban_words"),
		  str_count(sotu_text, "\\w+") %>% as_data_frame() %>% set_names("total_words")) %>%
	group_by(party, year) %>%
	summarize(urban_words = sum(urban_words),
			  total_words = sum(total_words)) %>%
	ungroup() %>%
	mutate(urban_pct = urban_words/total_words,
		   party = str_replace_all(party, "Nonpartisan", "Federalist") %>% str_replace_all("Democratic-Republican", "Democratic"))
	
rural_words %>%
	ggplot(aes(x = year, y = urban_pct, fill = party, color = party)) +
	geom_col(position = "identity", size = .2) +	
	scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expand_scale(add = c(0, 0.0001))) +
	scale_x_continuous(breaks = seq(1800, 2020, 20)) +
	scale_fill_manual(values = c("darkblue", "darkorange", "darkred", "darkgreen")) +
	scale_color_manual(values = c("darkblue", "darkorange", "darkred", "darkgreen")) +
	theme_citylab() +
	theme(axis.title.x = element_blank(),
		  axis.ticks.x = element_line()) +
	labs(title = "Use of 'rural,' 'farm(s)' or 'farmer(s)' in States of the Union",
		 caption = "Source: The American Presidency Project (David H. Montgomery/CityLab)",
		 y = "Percent of all words",
		 fill = "Party", color = "Party")

####

# A more simplified way to graph this data:

urban_words_decades <- bind_cols(sotu_meta, # Start much the same
		  str_count(sotu_text, "(?i) city| cities| urban") %>% as_data_frame() %>% set_names("urban_words"),
		  str_count(sotu_text, "\\w+") %>% as_data_frame() %>% set_names("total_words")) %>%
	# Here we start getting different
	mutate(president = as_factor(president), # forcats::as_factor() preserves existing order
		   decade = paste0(year - year %% 10, "s")) %>% # Convert to decades, using the modulo operator %%
	# Group and sum now by decade rather than year and party
	group_by(decade) %>%
	summarize(urban_words = sum(urban_words),
			  total_words = sum(total_words)) %>%
	mutate(urban_pct = urban_words/total_words)

# Plot
urban_words_decades %>%
	ggplot(aes(x = decade, y = urban_pct)) +
	geom_col() +
	scale_y_continuous(labels = percent_format(accuracy = 0.01), expand = expand_scale(add = c(0, 0.00017))) +
	scale_x_discrete(breaks = c("1800s", "1820s", "1840s", "1860s", "1880s", "1900s", "1920s", "1940s", "1960s", "1980s", "2000s")) + # Manually set breaks to show every other decade
	theme_minimal() +
	theme(axis.ticks.x = element_line()) + # Add axis ticks for legibility
	labs(title = "How often presidents talked cities in States of the Union",
		 subtitle = "Graph shows use of 'city,' 'cities,' and 'urban' in State of the Union speeches",
		 caption = "Source: The American Presidency Project (David H. Montgomery/CityLab)",
		 x = "Decade",
		 y = "Percent of all words")
