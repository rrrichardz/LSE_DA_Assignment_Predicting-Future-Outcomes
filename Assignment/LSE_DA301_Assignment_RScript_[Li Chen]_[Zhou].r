### Assignment activity: Week 4

# Import necessary libraries.
install.packages("tidyverse")
library(tidyverse)
vignette("tidy-data")

# Import data file.
lego <- read.csv(file.choose(), header = TRUE)

# View the data frame.
head(lego)

## Create visualisations to find further insights related to questions.

# Which age group submits the most reviews?
qplot(ages, fill = num_reviews, data = lego, geom = "bar",
      main = "Age group vs. Reviews", xlab = "Ages", ylab = "Num. Reviews")

# Remove rows with age = 0 and try again.
lego_clean <- lego[lego$ages > 0,]

# Find out the min value for ages column.
summary(lego_clean)

# Re-plop the visualisation.
qplot(ages, fill = num_reviews, data = lego_clean, geom = "bar",
      main = "Age group vs. Reviews", xlab = "Ages", ylab = "Num. Reviews")

# What is the most expensive Lego set purchased by customers who are at
# least 25 years old (>=25)?
lego_age25 <- lego[lego$ages >= 25, ]
lego_age25

qplot(ages, list_price, col = list_price, data = lego_age25,
      main = "Price of Lego purchased by customers age 25 and plus",
      xlab = "Age", ylab = "Price")


### Assignment Activity: Week 5

## Prepare the data for analysis.

# Import all libraries.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DataExplorer)

# Import the data file.
games <- read.csv(file.choose(), header = TRUE)

# Sense-check the data set.
summary(games)
as_tibble(games)
str(games)

# Determine missing values.
sum(is.na(games))

# Change values under 'Genre' to lowercase.
games$Genre <- str_to_lower(games$Genre)

# Merge the values for the variables 'Genre' and 'Platform'.
head(games$Genre)
head(games$Platform)

games$Gametype <- str_c(games$Platform, games$Genre, sep = " ")

as_tibble(games$Gametype)

# Change the unit for NA_Sales, EU_Sales and Global_Sales from millions to thousands.
games$NA_Sales <- games$NA_Sales * 1000
games$EU_Sales <- games$EU_Sales * 1000
games$Global_Sales <- games$Global_Sales * 1000
summary(games)

# Only include values above 0 (exclude any games without a sales).
games_new <- subset(games, NA_Sales > 0 & EU_Sales > 0)

summary(games_new)

# Remove extreme outliers with values above 10000.
games_new1 <- subset(games_new, NA_Sales < 10000 & EU_Sales < 10000 & 
                       Global_Sales < 10000)

summary(games_new1)

# Remove outliers with values above 3000.
games_new2 <- filter(games_new1, NA_Sales < 3000 & EU_Sales < 3000 &
                       Global_Sales < 3000)

summary(games_new2)

## Visualise the data

# Evaluate the skewness of the data, visualisation 1 (NA_Sales).
ggplot(games_new2, aes(x = NA_Sales, fill = Genre)) +
  geom_histogram(stat = "count", binwidth = 1, bins = 30) + 
  labs(title = "Video games sales in North America based on Genre",
       x = "North America Sales (in $1000s)",
       y = "Frequency") + 
  theme_classic()

ggplot(games_new2, aes(x = NA_Sales, y = Genre)) +
  geom_boxplot(outlier.colour = "red") + 
  labs(title = "Video games sales in North America based on Genre",
       x = "North America Sales (in $1000s)",
       y = "Genre") + 
  theme_classic()

ggplot(games_new2, aes(x = NA_Sales)) + 
  geom_density() +
  labs(title = "Kerneal Density Plot of video games sales in North America",
       x = "North America Sales (in $1000s)",
       y = "Density")

# Evaluate the skewness of the data, visualisation 2 (EU_Sales).
ggplot(games_new2, aes(x = EU_Sales, fill = Genre)) +
  geom_histogram(stat = "count", binwidth = 1, bins = 30) + 
  labs(title = "Video games sales in Europe based on Genre",
       x = "Europe Sales (in $1000s)",
       y = "Frequency") + 
  theme_classic()

ggplot(games_new2, aes(x = EU_Sales, y = Genre)) +
  geom_boxplot(outlier.colour = "red") + 
  labs(title = "Video games sales in Europe based on Genre",
       x = "Europe Sales (in $1000s)",
       y = "Genre") + 
  theme_classic()

ggplot(games_new2, aes(x = EU_Sales)) + 
  geom_density() +
  labs(title = "Kerneal Density Plot of video games sales in Europe",
       x = "Europe Sales (in $1000s)",
       y = "Density")

# Evaluate the skewness of the data, visualisation 3 (Global_Sales).
ggplot(games_new2, aes(x = Global_Sales, fill = Genre)) +
  geom_histogram(stat = "count", binwidth = 1, bins = 30) + 
  labs(title = "Global video games sales based on Genre",
       x = "Global Sales (in $1000s)",
       y = "Frequency") + 
  theme_classic()

ggplot(games_new2, aes(x = Global_Sales, y = Genre)) +
  geom_boxplot(outlier.colour = "red") + 
  labs(title = "Global video games sales based on Genre",
       x = "Global Sales (in $1000s)",
       y = "Genre") + 
  theme_classic()

ggplot(games_new2, aes(x = Global_Sales)) + 
  geom_density() +
  labs(title = "Kerneal Density Plot of global video games sales",
       x = "Global Sales (in $1000s)",
       y = "Density")

## Visualise the correlation between (Genre & Global_Sales, with EU_Sales
# and NA_Sales as third variables)

# Plot scatterplot to find correlation (with NA_Sales).
ggplot(games_new2, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(aes(colour = Genre, fill = Genre), 
              method = "lm", se = FALSE, size = 1) +
  labs(title = "Scatterplot of global sales of video games by Genre",
       subtitle = "North America sales vs. Global sales",
        x = "North America Sales in ($1000)",
        y = "Global sales in ($1000s)") +
  scale_x_continuous(breaks = seq(0, 3500, 500)) +
  scale_y_continuous(breaks = seq(0, 3500, 500)) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal()

# Plot scatterplot to find correlation (with EU_Sales).
ggplot(games_new2, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(aes(colour = Genre, fill = Genre), 
              method = "lm", se = FALSE, size = 1) +
  labs(title = "Scatterplot of global sales of video games by Genre",
       subtitle = "Europe sales vs. Global sales",
       x = "Europe Sales in ($1000)",
       y = "Global sales in ($1000s)") +
  scale_x_continuous(breaks = seq(0, 3500, 500)) +
  scale_y_continuous(breaks = seq(0, 3500, 500)) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal()


### Assignment Activity: Week 6

## Prepare the data

# Import library.
library(tidyverse)
library(dplyr)

# Import the data set.
games_sales <- read.csv(file.choose(), header = TRUE)

# Sense-check the data set.
summary(games_sales)

# Check for missing values.
sum(is.na(games_sales))

# Create a new object with only numeric values.
games_num <- select_if(games_sales, is.numeric)

# Use the new object to check for correlations between variables.
cor(games_num)

## Apply predictive model

# Apply multi-linear regression model to determine optimal global sales.
model1 <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = games_sales)

# Print the summary statistics.
summary(model1)

## Test the model for predictions

# Predict the optimal global sales price.
predict_games <- predict(model1, newdata = games_sales,
                         interval = "confidence")

# Print the predictions.
predict_games

# Add the predictions as a new column to the data set.
games_sales$Prediction <- predict_games
