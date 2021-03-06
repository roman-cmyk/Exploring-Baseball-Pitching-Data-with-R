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




#***-----

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





#***----

#FASTBALL VELOCITY DISTRIBUTION

# Create july_ff
july_ff <- subset(greinke_july, pitch_type == "FF")

# Create other_ff
other_ff <- subset(greinke_other, pitch_type == "FF")

# Formatting code, don't change this
par(mfrow = c(1, 2))

# Plot histogram of July fastball speeds
hist(july_ff$start_speed)

# Plot histogram of other month fastball speeds
hist(other_ff$start_speed)



#****----

#DISTRIBUTION COMPARISON WITH COLORS
# Make a fastball speed histogram for other months
hist(other_ff$start_speed,
     col = "#00009950", freq = FALSE,
     ylim = c(0, .35), xlab = "Velocity (mph)",
     main = "Greinke 4-Seam Fastball Velocity")

# Add a histogram for July
hist(july_ff$start_speed, add = TRUE,
     col = "#99000050", freq = FALSE)

# Draw vertical line at the mean of other_ff
abline(v = mean(other_ff$start_speed),
       col = "#00009950", lwd = 2)

# Draw vertical line at the mean of july_ff
abline(v = mean(july_ff$start_speed),
       col = "#99000050", lwd = 2)


#***----
# tapply() for velocity changes

# Summarize velocity in July and other months
tapply(greinke$start_speed, greinke$july, mean)

# Create greinke_ff
greinke_ff <- subset(greinke, greinke$pitch_type == 'FF')


# Calculate mean fastball velocities: ff_velo_month
ff_velo_month <- tapply(greinke_ff$start_speed, greinke_ff$july, mean)

# Print ff_velo_month
ff_velo_month



#***----
# Game-by-game velocity changes

# Create ff_dt
ff_dt <- data.frame(tapply(greinke_ff$start_speed, greinke_ff$game_date, mean))

# Print the first 6 rows of ff_dt
head(ff_dt)




#***----

# Tidying the data frame

# Create game_date in ff_dt
ff_dt$game_date <- as.Date(row.names(ff_dt), "%Y-%m-%d")

# Rename the first column
colnames(ff_dt)[1] <- "start_speed"

# Remove row names
row.names(ff_dt) <- NULL

# View head of ff_dt
head(ff_dt)




#***----

# A game-by-game line plot

# Plot game-by-game 4-seam fastballs
par(mfrow = c(1, 1))
plot(ff_dt$start_speed ~ ff_dt$game_date,
     lwd = 4, type = "l", ylim = c(88, 95),
     main = 'Greinke 4-Seam Fastball Velocity',
     xlab = 'Date', ylab = 'Velocity (mph)')




#***----

# Adding jittered points

# Add jittered points to the plot
points(greinke_ff$start_speed ~ jitter(as.numeric(greinke_ff$game_date)),
       pch = 16, col = "#99004450")



#***----

# Pitch mix tables
# Subset the data to remove pitch types "IN" and "EP"
greinke <- subset(greinke, pitch_type != "IN" & pitch_type != "EP")

# Drop the levels from pitch_type
greinke$pitch_type <- droplevels(greinke$pitch_type)

# Create type_tab
type_tab <- table(greinke$pitch_type, greinke$july)

# Print type_tab
type_tab



#***----

# Pitch mix table using prop.table()

# Create type_prop table
type_prop <- round(prop.table(type_tab, margin = 2), 3)

# Print type_prop
type_prop



#***---- Pitch mix tables - July vs. other

# Pitch mix tables - July vs. other

# Create ff_prop
ff_prop <- type_prop[3, ]

# Print ff_prop
ff_prop

# Print ff_velo_month
ff_velo_month



#***----

# Pitch mix tables - changes in pitch type rates

# Create the Difference column
type_prop$Difference <- (type_prop$July - type_prop$Other) / type_prop$Other

# Print the type_prop
type_prop

# Create the Difference column
type_prop$Difference <- (type_prop$July - type_prop$Other) / type_prop$Other

# Print the type_prop
type_prop

# Plot a barplot
barplot(type_prop$Difference, names.arg = type_prop$Pitch, 
        main = "Pitch Usage in July vs. Other Months", 
        ylab = "Percentage Change in July", 
        ylim = c(-0.3, 0.3))



#***----

# Ball-strike count frequency

# Create bs_table
bs_table <- table(greinke$balls, greinke$strikes)

# Create bs_prop_table
bs_prop_table <- round(prop.table(bs_table), 3)

# Print bs_prop_table
bs_prop_table

# Print row sums
rowSums(bs_prop_table)

# Print column sums
colSums(bs_prop_table)


#***-----

# Make a new variable

# Create bs_count
greinke$bs_count <- paste(greinke$balls, greinke$strikes, sep = "-")

# Print the first 6 rows of greinke
head(greinke)
