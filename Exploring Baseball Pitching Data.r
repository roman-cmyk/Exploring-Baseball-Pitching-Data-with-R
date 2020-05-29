# ---
# title: "Exploring Baseball Pitching Data"
# author: "Francisco Roman Peña de la Rosa"
# date: "29/5/2020"
# output: Script file
# ---

# Importing libraries
library(dplyr)
library(tidyr)

# Read dataset file
greinke <- read.csv('greinke2015.csv')

# Print the first 6 rows of the data
head(greinke)

# Print the number of rows in the data frame
nrow(greinke)

# Summarize the start_speed variable
summary(greinke$start_speed)

# Get rid of data without start_speed
greinke <- subset(greinke, !is.na(start_speed))

# Print the number of complete entries
nrow(greinke)

# Print the structure of greinke
str(greinke)

# Check if dates are formatted as dates
class(greinke$game_date)

# Change them to dates
greinke$game_date <- as.Date(greinke$game_date, "%m/%d/%Y")

# Check that the variable is now formatted as a date
class(greinke$game_date)

# Separate game_date into "year", "month", and "day"
greinke <- separate(data = greinke, col = game_date,
                    into = c("year", "month", "day"),
                    sep = "-", remove = FALSE)

# Convert month to numeric
greinke$month <- as.numeric(greinke$month)

# Create the july variable
greinke$july <- ifelse(greinke$month == 7, "july", "other")

# View the head() of greinke
head(greinke)

# Print a summary of the july variable
summary(factor(greinke$july))


#***--------------------------------

# VELOCITY DISTRIBUTION
# Make a histogram of Greinke's start speed
hist(greinke$start_speed)


# Create greinke_july
greinke_july <- subset(greinke,greinke$july == 'july')

# Create greinke_other
greinke_other <- subset(greinke, greinke$july == 'other')

# Use par to format your plot layout
par(mfrow = c(1,2))


# Plot start_speed histogram from july
hist(greinke_july$start_speed)

# Plot start_speed histogram for other months
hist(greinke_other$start_speed)