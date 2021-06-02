# Introduction----

# The document outline can be used to navigate through the script in RStudio

# Model title: Predicting Rain in Australia
# Goal: Predict "RainTomorrow" Yes or No on weatherAUS data set
# Author: Vincent Pluchet
# Date: June 2nd 2021

# Script description:

# Certain script parts can be skipped (marked below as "optional")
# if the desired focus is running the models only

# The main parts of the script are as follows:
      # Installation: loads packages and libraries (compulsory)
      # Loading the data: loads two data sets (compulsory)
      # Initial data exploration (optional)
      # Building data base: splits Validation and Training set (compulsory)
      # Further Data Exploration (optional)
      # Handling the NA Value (compulsory)
      # Creating clean data: (compulsory)
          # removes NAs and splits training set into train and
          # test sets for model training and crss-validation
      # Further correlations (optional)
      # Models: training and cross-validation on train and test sets
          # It is necessary to run the models in the order
          # of the script, as an ensemble is built progressively
          # No Rain Model, Humidity Model
          # GLM, Random Forest, XG Boost
          # PCA + GLM, KNN, QDA
          # Ensemble
          # General Summary
          # It is not necessary to run the Models to run the 
          # Validation part below.
      # Validation: 
          # Applies the final selected model to the 
          # independent Validation set

# Overall sourcing time with echo: around 8 minutes on a laptop. 
# Running the models is usually done in seconds, but certain 
# trainings and ANOVA calculations (done for information)
# can take 2-3 minutes. Warnings are provided when running time
# is greater than 1 second.


# Installation----
# Listing the required packages
required_packages <- c("tidyverse","ggrepel","gghighlight",
                       "gridExtra","viridisLite","viridis",
                       "scales","lubridate","stats","broom",
                       "knitr","DescTools","corrplot","naniar",
                       "tsbox","dygraphs","ozmaps","sf",
                       "caret","randomForest","xgboost")

# Installing the packages and loading the libraries
# Will simply load the library if the package is already installed
for (p in required_packages) {
  if(!require(p, character.only = TRUE)) {install.packages(p, dep = TRUE,
                                repos = "http://cran.us.r-project.org")}
}

# Load the libraries manually if not loaded in the previous step
library(tidyverse)
library(ggrepel) # includes extra geoms for ggplot
library(gghighlight) # to highlight items on graphs
library(gridExtra) # to display graphs side by side
library(viridisLite) # to display color-blind friendly colors
library(viridis) # to plot color gradients
library(scales) # for percentage scales
library(lubridate) # to convert dates
library(stats) # for glm and other statistical analysis
library(broom) # convert statistical objects into tidy tibbles
library(knitr) # for table designs
library(DescTools) #for VIF function
library(corrplot) # for correlation plots
library(naniar) # for NA visualization
library(tsbox) # for time series
library(dygraphs) # for time series graphs
library(ozmaps) # to load maps of Australia
library(sf) # a standardized way to encode spatial vector data
library(caret) # for models
library(randomForest) # for random forest models
library(xgboost) # for XG Boost model


# Loading the data----

# We load two files:
# - the original data file weatherAUS
# - a small file locationsAUS with data about Australian locations, 
#  containing coordinates, state and population, which we will use 
#  for maps

# If the csv files are available in the working directory:
if(file.exists("weatherAUS.csv")) weatherAUS <- read.csv("weatherAUS.csv")
if(file.exists("locationsAUS.csv")) locationsAUS <- read.csv("locationsAUS.csv")

# Otherwise download the files from GitHub:
url_file <- "https://raw.githubusercontent.com/vpluchet/capstone2/main/weatherAUS.csv"
url_loc <- "https://raw.githubusercontent.com/vpluchet/capstone2/main/locationsAUS.csv"

if(!exists("weatherAUS")) weatherAUS <- read_csv(url(url_file), col_types = cols(.default = "d",
                                                       Date = "c",
                                                       Location = "c",
                                                       WindGustDir = "c",
                                                       WindDir9am = "c",
                                                       WindDir3pm = "c",
                                                       RainToday = "c",
                                                       RainTomorrow = "c"))

if(!exists("locationsAUS")) locationsAUS <- read_csv(url(url_loc), col_types = cols(
  Location = "c",
  lat = "d",
  lng = "d",
  state = "c",
  population = "d"))


# Preparing information for future use----

# Defining some default colors for scatterplots
c_red <- "#F8766D"
c_blue <- "#00B4F0"

n_dat <- nrow(weatherAUS) # for use in report

# Extracting the locations
locations <- weatherAUS$Location %>%
  unique()
locations <- locations[order(locations)] %>% data.frame()
colnames(locations) = "Location"

# Populating locations with coordinates, state and population
# We will use this data for maps
locations <- locations %>% left_join(locationsAUS, by = "Location")

# Checking that there are no NAs in locations
sum(is.na(locations))

# Extracting states co-ordinates for maps
oz_states <- ozmaps::ozmap_states

g_loc_size <- 6 # used to control text size for axis with location


# Creating a function to map Australia with points and a color scale
# We create this function because it will be used a few times when color
# scales are required.
# The inputs to the function are:
    # map_data: the points data, with longitude (lng) and latitude (lat)
    # map_color: the name of the variable in map_data which will be used
    #     to color the points; must be defined with the "sym" function
    # map_color_begin and map_color_end: integers between 0 and 1 to
    #     define the beginning and the end of the color gradient. This is
    #     use to have comparable color scales between the various maps
    # map_title, map_subtitle, map_caption: labs parameters; caption
    #     will typically be computed to reflect the plotted data range
f_map <- function(map_data, map_color, map_color_begin, map_color_end, 
                  map_title, map_subtitle, map_caption){
  ggplot(oz_states) + 
    geom_sf() + 
    coord_sf() +
    geom_point(data = map_data, aes(lng, lat, color = !!map_color), size = 4) +
    theme(panel.background = element_rect(fill = "lightblue"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_blank()) + 
    scale_color_viridis(discrete = FALSE, direction = -1, 
                        begin = map_color_begin, end = map_color_end,
                        option = "turbo") +
    labs(title = map_title,
         subtitle = map_subtitle,
         caption = map_caption)
}


# Initial Data exploration----
# The goal of this part is to determine whether some of the data
# should be either discarded because of insufficient relevance or
# errors


# Visualizing the locations on the map of Australia
# (here we do not use the f_map function, no color scale required)
map_loc <- ggplot(oz_states) + 
  geom_sf() + 
  coord_sf() +
  geom_point(data = locations, aes(lng, lat), size = 1, color = c_red) +
  geom_text_repel(data = locations, aes(lng, lat, label = Location),
            size = 3,
            color = "darkblue",
            max.overlaps = 30) +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Map of Australia with the 49 weather locations")

# Printing the map takes a couple of seconds
# Warning: printing the map sometimes generates a message that the 
# polygon edge was not found. If this happens, re-run the above command
# defining the map (map_loc <- ...) and re-print
print(map_loc)

# Computing percentage of NA per predictor
pna <- weatherAUS %>% is.na() %>% 
  colMeans() %>%
  data.frame(Perc_NA = .) %>%
  arrange(desc(Perc_NA)) %>%
  mutate(Item = rownames(.))

# Plotting the percentages
# The graph shows Sunshine, Evaporation, Cloud3pm, Cloud9am having 
# high NA percentages
gpna <- pna %>%
  mutate(Item = reorder(Item, Perc_NA)) %>%
  ggplot(aes(Item, Perc_NA, label = scales::percent(Perc_NA, 0.1))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(nudge_y = 0.02, size = 3) +
  coord_flip() +
  ylab("NA Percentage") +
  ggtitle("Percentage of NA data per Item")
print(gpna)

# Sunshine has 48% NA values
perc_NA_Sunshine <- percent(mean(is.na(weatherAUS$Sunshine)))
print(perc_NA_Sunshine)

# Computing percentage of NA per Location
# Using nianar package for easier graph
# The plot does not evidence any location with a significantly higher
# Proportion of missing data

g_pnal <- gg_miss_fct(x = weatherAUS, fct = Location) +
  ggtitle("Missing data percentage per Location") +
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()
print(g_pnal)

# Focusing on Sunshine and Cloud3pm and looking at the percentage
# of NA per location

g_sc <- weatherAUS %>% 
  select(Location, Sunshine, Cloud3pm) %>%
  group_by(Location) %>%
  summarise(perc_NA_Sunshine = mean(is.na(Sunshine)),
            perc_NA_Cloud3pm = mean(is.na(Cloud3pm))) %>%
  mutate(Location = reorder(Location, perc_NA_Sunshine)) %>%
  ggplot() +
  geom_point(aes(Location, perc_NA_Sunshine, colour = "Sunshine")) +
  geom_point(aes(Location, perc_NA_Cloud3pm, colour = "Cloud3pm"),
             position = position_nudge(y = 0.004), alpha = 0.8) +
  scale_colour_manual(name="Color",
                      values=c(Sunshine="steelblue", Cloud3pm="orangered3")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip() +
  ylab("NA Percentage") +
  labs(title = "Percentage of Sunshine and Cloud3pm NA data",
       subtitle = "Cloud points slighlty nudged to the right")

# The graph shows that around 7 locations only have full Sunshine and Cloud3pm data
# (0% NA). Other locations like Wollongong have partial Cloud3pm data but no Sunshine.
# Certain locations like Walpole have no data on Sunshine or Cloud3pm
# Several locations have partial data on both predictors
# So we cannot have a simple split of locations between those which have the data
# and those which don't
print(g_sc)


# Counting number of data points by year
# The graph shows that years 2007-2008 and 2017 are incomplete
# We will therefore remove these years in data pre-processing
g_time_n <- weatherAUS %>%
  mutate(Year = year(parse_date(Date, format = "%Y-%m-%d"))) %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  ggplot(aes(Year, count)) +
  geom_point(color = "steelblue") +
  scale_x_continuous(breaks = seq(2007,2019,1)) +
  ggtitle("Number of data points per year")
print(g_time_n)

# Exploring NAs per descriptor over time
# Using the naniar package for easier graph
# The plot shows that data collection has been relatively consistent over time
g_time_NA <- weatherAUS %>%
  mutate(Year = year(parse_date(Date, format = "%Y-%m-%d"))) %>%
  gg_miss_fct(fct = Year) +
  labs(title = "NA percentage per Variable per Year")
print(g_time_NA)

# Counting the number of observations per Location
# The graph shows three locations that have a much lower
# number of records. This requires further exploration.

g_loc_count <- weatherAUS %>% group_by(Location) %>%
  summarise(ncount = n()) %>%
  mutate(Location = reorder(Location, ncount)) %>%
  ggplot(aes(Location, ncount)) +
  geom_point(color = "steelblue") +
  ggtitle("Number of observations per Location in data set") +
  ylab("Number of observations") +
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()
print(g_loc_count)

# Re-plotting the number of observations for the three locations with few records
# The graph shows that records started in 2013 for these locations
# This is not an issue in itself and therefore the data can be kept
# 2017 is incomplete but this year will be removed
i_loc <- c("Uluru", "Nhil", "Katherine")
g_time_3loc <- weatherAUS %>%
  filter(Location %in% i_loc) %>%
  mutate(Year = year(parse_date(Date, format = "%Y-%m-%d"))) %>%
  group_by(Year, Location) %>%
  summarise(count = n()) %>%
  ggplot(aes(Year, count, color = Location)) +
  geom_point() +
  scale_x_continuous(breaks = seq(2007,2019,1)) +
  facet_wrap(Location ~ .) +
  
  labs(title = "Number of data points per year for selected locations")
print(g_time_3loc)


# Box-Plotting the numeric items to detect outliers / unusual data points
# The graph shows some significant dispersion for some predictors.
# There are outliers, however no obvious erroneous values.
# Therefore no specific value restatements appear necessary
gnum <- weatherAUS[sapply(weatherAUS, is.numeric)] %>%
  pivot_longer(cols = 1:ncol(.), 
               names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(Predictor, Value)) +
  geom_boxplot(na.rm = TRUE, color = "steelblue") +
  facet_wrap(Predictor ~ ., scales = "free")
# Showing the plot (takes a few seconds)
print(gnum)

# A few predictors show a heavy weight of zero values
# Rainfall is greater than 0 in 36% of the cases only, 
# and Rainfall is greater than 1 in 22% of the cases only,
# indicating that rainfall is generally rare
# We also check that RainToday is identical to Rainfall >1
perc0_Rainfall <- percent(mean(weatherAUS$Rainfall > 0, na.rm = TRUE))
perc1_Rainfall <- percent(mean(weatherAUS$Rainfall > 1, na.rm = TRUE))
identical(weatherAUS$Rainfall >1, ifelse(weatherAUS$RainToday == "No", FALSE, TRUE))




# Building data base----
# Plus train, test and validation sets


# Building a data base by removing NAs in RainTomorrow
# Retaining only 2009-2016 (complete years)
# Converting character columns to factors
weather_base <- weatherAUS %>% 
  filter(!is.na(RainTomorrow)) %>%
  mutate(Date = parse_date(Date, format = "%Y-%m-%d"),
         Year = year(Date),
         WindGustDir = factor(WindGustDir),
         WindDir9am = factor(WindDir9am),
         WindDir3pm = factor(WindDir3pm),
         Location = factor(Location),
         RainToday = factor(RainToday),
         RainTomorrow = factor(RainTomorrow)) %>%
  filter(Year %in% 2009:2016) %>% 
  select(-Year)

# Computing percentage of data retained (90%)
n_wea <- nrow(weather_base) 
prop_n <- percent(n_wea / n_dat)

# Creating a table with the order of the columns (will be used after certain
# pivot_wider operations)
col_order <- data.frame(Name = colnames(weather_base), Rank = 1:ncol(weather_base))


# Creating a Validation set which will be used only at the very end
set.seed(1, sample.kind = "Rounding")
index_part <- createDataPartition(weather_base$RainTomorrow, times = 1,
                                  p = 0.2, list = FALSE)
validation <- weather_base[index_part,] # Ultimate validation set
weather <-  weather_base[-index_part,] # Working set for model definition

# Making sure Location-Month combinations in validation set 
# are present in base set, as this will be used to populate NAs
validation <- semi_join(x = validation %>%
                                  mutate(key = paste(Location, month(Date), sep = "|")),
                                y = weather %>%
                                  mutate(key = paste(Location, month(Date), sep = "|")),
                                by = "key") %>% select(-key)

# in practice with seed 1 there are no removed rows however procedure kept 
# in case a different seed produces different results
removed <- anti_join(x = validation %>%
                       mutate(key = paste(Location, month(Date), sep = "|")),
                     y = weather %>%
                       mutate(key = paste(Location, month(Date), sep = "|")),
                     by = "key") %>% select(-key)
nrow(removed) # Number of removed rows (nil with seed 1)
# Adding back any rows removed from validation
weather <- rbind(weather, removed)


# Creating train and test sets on the working data set weather
set.seed(1, sample.kind = "Rounding")
index_part <- createDataPartition(weather$RainTomorrow, times = 1,
                                  p = 0.2, list = FALSE)
weather_test <- weather[index_part,] # Test set for model selection
weather_train <- weather[-index_part,] # Training set for model definition


# Creating an index identifying numeric data
index_num <- sapply(weather, is.numeric)

# Also creating an index tagging categorical factors 
# for which correlations will be computed against RainTomorrow
# We exclude Location (effect studied separately) and RainTomorrow (against
# which the correlations will be computed)
index_cat <- sapply(weather, is.factor) & !colnames(weather) %in% c("Location", "RainTomorrow")

# Index identifying the RainTomorrow column
index_RT <- colnames(weather) == "RainTomorrow"




# Further Data exploration----


# Location effect----


# Studying location effect on RainToday

# Plotting average Winter and Summer rainfalls on the map
winter_rainfall <- weather %>%
  filter(month(Date) %in% c(6,7,8) & !is.na(Rainfall)) %>%
  group_by(Location) %>%
  summarise(avg_rainfall = 92 * mean(Rainfall)) %>%
  mutate(Season = "Winter") %>%
  left_join(locations, by = "Location")

summer_rainfall <- weather %>%
  filter(month(Date) %in% c(12,1,2) & !is.na(Rainfall)) %>%
  group_by(Location) %>%
  summarise(avg_rainfall = 90 * mean(Rainfall)) %>%
  mutate(Season = "Summer") %>%
  left_join(locations, by = "Location")

season_rainfall <- bind_rows(winter_rainfall, summer_rainfall)

map_rain <- ggplot(oz_states) + 
  geom_sf() + 
  coord_sf() +
  geom_point(data = season_rainfall, aes(lng, lat, size = avg_rainfall),
             color = c_blue) +
  facet_grid(Season ~ .) +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank()) + 
  scale_color_viridis(discrete = FALSE, direction = -1, option = "turbo") +
  labs(title = "Summer and Winter Rainfall",
       subtitle = "Point size represents season amount",
       caption = "Summer considered as Dec-Jan-Feb, Winter as Jun-Jul-Aug")

# The map shows the diversity of the Australian rainfall. The north of
# Australia receives significant rain in Summer and relatively little
# in Winter. In the south, rainfall is more balanced between both
# Seasons but it rains more in Winter. 
# Not surprisingly, coastal locations get more rain. Central locations
# are quite dry.

# Printing the map takes a few seconds
# Warning: printing the map sometimes generates a message that the 
# polygon edge was not found. If this happens, re-run the above command
# defining the map (map_rain <- ...) and re-print
print(map_rain)


# Plotting average percentage of rain per Location
# The plot shows significant differences in terms of percentage of 
# rainy days per location, as anticipated from the previous map
g_loc_rain <- weather %>% 
  filter(!is.na(RainToday)) %>%
  group_by(Location) %>%
  summarise(prop_rain = mean(RainToday == "Yes")) %>%
  mutate(Location = reorder(Location, prop_rain)) %>%
  ggplot(aes(Location, prop_rain)) +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Percentage of \"RainToday = Yes\" per Location in weather data set") +
  ylab("Percentage") +
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()
print(g_loc_rain)



# Time effect----

# Assessing seasonality component
# We will focus on Canberra rainfall
# Looking at raining trends in Canberra
rain_can <- weather %>%
  filter(Location == "Canberra" & !is.na(Rainfall)) %>%
  select(time = Date, value = Rainfall)

tsw <- ts_ts(rain_can)
ts_plot('Canberra Rainfall' = tsw,
        title = "Daily Rainfall in Canberra")

# Plotting the trend over time
# This confirms that there is some time effect
g_ts <- tsw %>% ts_trend() %>% ts_dygraphs()
print(g_ts)

# We can explore a potential Year effect
# by plotting the proportion of rainy days over time
g_time <- weather %>%
  filter(!is.na(RainToday)) %>%
  group_by(Date) %>%
  summarise(prop_rain = mean(RainToday == "Yes")) %>%
  ggplot(aes(Date, prop_rain)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  ggtitle("Proportion of rainy days over time") +
  scale_y_continuous(labels = scales::percent)
# The plot shows a moderate Year effect
print(g_time)


# We can further explore seasonality impact by looking
# at the proportion of rainy days with error bars per month
# June and July are the wetter months however variations are significant
g_day_rain <- weather %>%
  filter(!is.na(RainToday)) %>%  
  mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(Year, Month) %>%
  summarise(prop_rain = mean(RainToday == "Yes"))%>%
  group_by(Month) %>%
  summarise(avg = mean(prop_rain),
            se = sd(prop_rain) / sqrt(n()),
            ymin = avg - 2 * se,
            ymax = avg + 2 * se) %>%
  ggplot(aes(x = Month, y = avg, ymin = ymin, ymax = ymax)) +
  geom_point(color = "steelblue") +
  geom_errorbar(color = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:12) +
  ggtitle("Proportion of rainy days with error bar as average +/- 2 sd")
# The plot shows a definite month effect
print(g_day_rain)



# Correlations between factors----

# Studying correlation between numeric variables

# Computing the correlation matrix between numerical factors
correl <- cor(weather[index_num], use="pairwise.complete.obs")

# Plotting the correlations with 4 clusters
# The plot shows that a few variables are highly correlated 
# (like Pressure9am and Pressure3pm)
# In regression, "multicollinearity" refers to predictors
# that are correlated with other predictors. We will need to be mindful
# of this when building the model and create predictors with less
# correlation. High multicollinearity can be a major problem
# because it increases the variance of the regression coefficients,
# making them unstable.
# Having said that, we can also see many factors which are not
# highly correlated between each other.
g_cor <- corrplot(correl,
                  method = "circle",       
                  order = "hclust",         # Ordering method of the matrix
                  hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
                  addrect = 4,              # If order = "hclust", number of cluster rectangles
                  rect.col = 3,             # Color of the rectangles
                  rect.lwd = 3)             # Line width of the rectangles





# Correlations with RainTomorrow----


# We start by Studying graphic correlation between RainToday and RainTomorrow
weather %>%
  filter(!is.na(RainToday)) %>%  
  ggplot(aes(x = RainToday, y = RainTomorrow)) +
  geom_count(aes(size = after_stat(prop), group = RainToday))

# The graph confirms that if it does not rain today, rain tomorrow is unlikely,
# but if it rains today, the chances of rain tomorrow are roughly 50-50
# which is higher than the average probability of rain (23%)
# We can visualise the probabilities with a table
t_probrtt <- weather %>%
  filter(!is.na(RainToday)) %>%  
  group_by(RainToday) %>%
  summarise(Rain_Tomorrow = mean(RainTomorrow == "Yes"),
            No_Rain_Tomorrow = mean(RainTomorrow == "No")) %>%
  mutate(across(where(is.numeric), ~ percent(.x)))

kable(t_probrtt, caption = "RainTomorrow vs RainToday",
      format = "simple", align = "c")

# Let's analyse another factor: WindGustDir, alongside WindGustSpeed
# We will focus on Canberra and Perth
# The plot shows that the higher the WingGustSpeed, the higher the chance of 
# rain tomorrow (usually but not always)
# There is also an indication that WindGustDir plays a role, especially for Perth
g_wg <- weather %>%
  filter(Location %in% c("Canberra", "Perth")) %>%
  filter(!is.na(WindGustSpeed)) %>%
  ggplot(aes(WindGustDir, WindGustSpeed, color = RainTomorrow)) +
  geom_point() +
  labs(title = "Effect of WindGustSpeed and WindGustDir on RainTomorrow",
       subtitle = "Canberra, Perth") +
  facet_grid(Location ~. )
print(g_wg)

# To evidence further the effect of WindGustDir in the chosen locations,
# we will plot the percentage of RainTomorrow vs WindGustDir
# We first compute the average percentage of rain in each location
# which will also be displayed in the graph. We add Darwin too.
p_rt <- weather %>%
  filter(Location %in% c("Perth", "Canberra", "Darwin")) %>%
  group_by(Location) %>%
  summarise(Rain_Tomorrow = mean(RainTomorrow == "Yes"))

# The graph below shows the percentage of RainTomorrow, depending on 
# The WindGustDir. For Darwin and Perth, there is a clear effect of Northern
# and Western winds, implying greater chances of rain tomorrow.
# For Canberra, the effect of WindGustDir is less obvious.
g_wgp <- weather %>%
  filter(Location %in% c("Perth", "Canberra", "Darwin")) %>%
  filter(!is.na(WindGustDir)) %>%
  group_by(Location, WindGustDir)%>%
  summarise(Rain_Tomorrow = mean(RainTomorrow == "Yes")) %>%
  mutate(Avg = p_rt[match(Location, p_rt$Location), ]$Rain_Tomorrow) %>%
  ggplot(aes(WindGustDir, Rain_Tomorrow, color = Location, fill = Location)) +
  geom_bar(stat = "identity") + 
  geom_point(aes(WindGustDir, Avg), color = "DarkBlue") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Rain Tomorrow per WindGustDir",
       subtitle = "Blue dots represent the average percentage of Rain of the location") +
  ylab("Rain Percentage") +
  coord_flip() + 
  facet_wrap(Location ~ .)

print(g_wgp)

# Pearson’s chi-squared test (χ2) is a statistical test applied
# to sets of categorical data to evaluate how likely it is that
# any observed difference between the sets arose by chance. 
# It is the most widely used of many chi-squared tests.
# It tests a null hypothesis stating that the frequency distribution of 
# certain events observed in a sample is consistent with a particular 
# theoretical distribution

# Pearson’s chi-squared test is used to assess three types of comparison: 
# goodness of fit, homogeneity, and independence. 
# For all three tests, the computational procedure includes the following steps:
#   
#   Calculate the chi-squared test statistic, χ2

# Determine the degree of freedom (df) of that statistic.
# Select a desired level of confidence for the result of the test.
# Compare χ2
# to the critical value from the chi-squared distribution with df degrees of 
# freedom and the selected confidence level.
# Sustain or reject the null hypothesis.
# If the test statistic exceeds the critical value of χ2
# the null hypothesis can be rejected, and the alternative hypothesis can be accepted.
# If the test statistic falls below the critical value of χ2, 
# no clear conclusion can be reached. The null hypothesis is sustained, but not necessarily accepted.

# We define a function to compute chi-squared between a factor and RainTomorrow
Cor_RainT <- function(c){
  test <- chisq.test(x = c, y = weather$RainTomorrow, 
                     correct = FALSE, simulate.p.value = FALSE)
  data.frame(X_squared = test$statistic, p_value = test$p.value)
}

# We perform the test versus each categorical variable
# The chi-squared test confirms that we can reject the hypothesis 
# that the categorical variables and RainTomorrow are independent
table_chi_cat <- t(data.frame(sapply(weather[index_cat], FUN = "Cor_RainT")))

kable(table_chi_cat, digits = 10, caption = "p-values for corr between categorical variables and RainTomorrow",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))

# Now we look at the relationship between numerical variables and RainTomororw
# We start by looking at Humidity3pm and Sunshine versus Rain Tomorrow
# The plot shows that Humidity3pm and Sunshine may be good predictors of whether
# there will be rain tomorrow or not, because the range of values differ
# reasonably well between both situations
g_Humidity3pm <- weather %>% 
  ggplot(aes(RainTomorrow, Humidity3pm)) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle("Humidity3pm")
g_Sunshine <- weather %>% 
  ggplot(aes(RainTomorrow, Sunshine)) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle("Sunshine")
grid.arrange(g_Humidity3pm, g_Sunshine, nrow = 1)

# However a scatter plot reveals that the delimitation
# between RainTomorrow Yes and No is very imprecise.
# This will certainly make predictions difficult: for example, 
# we can have high Humidity, low Sunshine and still no rain
g_HumSun <- weather %>%
  ggplot(aes(Humidity3pm, Sunshine, color = RainTomorrow)) +
  geom_point(na.rm = TRUE) +
  scale_color_manual(values = c(c_red, c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "SunShine vs Humidity3pm",
       subtitle = "Color represents RainTomorrow")
# printing the graph takes a few seconds
print(g_HumSun)

# We repeat the graph, selecting 6 locations only
# The same complex pattern is observed, confirming that the overall
# pattern must be present in many places
g_HumSun_sel <- weather %>%
  filter(Location %in% c("Canberra", "Perth", "Darwin", 
                         "Brisbane", "Sydney", "AliceSprings")) %>%
  ggplot(aes(Humidity3pm, Sunshine, color = RainTomorrow)) +
  geom_point(na.rm = TRUE) +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "SunShine vs Humidity3pm",
       subtitle = "Color represents RainTomorrow") +
  facet_wrap(Location ~ .)
print(g_HumSun_sel)


# Extending the previous study by looking at the relationship 
# between all numeric data versus RainTomorrow

# Creating an index to select numeric data and RainTomorrow
index_numRT <- index_num | index_RT

# Building the plot
# We use pivot_longer in order to build all graphs at once, using only
# one variable Value and facet_wrapping by Predictor
g_rel <- weather[index_numRT] %>%
  relocate(RainTomorrow, .before = 1) %>%
  pivot_longer(cols = 2:ncol(.), 
               names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(RainTomorrow, Value)) +
  geom_boxplot(na.rm = TRUE, color = "steelblue") +
  facet_wrap(Predictor ~ ., scales = "free")

# Printing the plot (takes a few seconds)
# The plot shows that Cloud3pm also seems a good predictor (but we know
# that it is highly correlated with Sunshine).
# And so are morning indicators for Cloud and Humidity
# Pressure and WindGustSpeed also seem relatively good indicators
# For other variables, the relationship is less strong
print(g_rel)



# Handling the NA Values----


# Handling NA Values for numerical data

# We create a temp data frame containing the median of each predictor
# per Location & Month - indeed each predictor behaves differently
# by Location and Month
NA_Repl_temp <- aggregate(weather[index_num],
                          by  = list(weather$Location, month(weather$Date)),
                          FUN = "median",
                          na.rm = TRUE) %>%
  rename(Location = Group.1, Month = Group.2)

# There remains a few NAs however, therefore we will fill these with the overall
# median per month
NA_Repl <- NA_Repl_temp%>%
  group_by(Month) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE),.))) %>%
  as.data.frame()

# We can visualize NA_Repl_temp and NA_Repl before removing the temp file
# view(NA_Repl_temp)
# view(NA_Repl)

# Handling NA values for categorical data (factors): we will populate
# NAs with the most frequent value (the mode) for the Location/Month

# We will not use the DescTools::Mode function as it returns 
# the numeric value of the factor, we will rather define our
# own mode function:
f_mode <- function(d) {
  f <- d %>% na.omit()
  unique_f <- unique(f)
  unique_f[which.max(tabulate(match(f, unique_f)))]
}

NA_Repl_tempf <- aggregate(weather[index_cat],
                           by  = list(weather$Location, month(weather$Date)),
                           FUN = "f_mode") %>%
  rename(Location = Group.1, Month = Group.2)

# There remains a few NAs however only for Albany and Newcastle
# where WindGustDir is entirely missing as shown in the below
# analysis:
t_naWGD <- aggregate(NA_Repl_tempf[3:6],
          by = list(NA_Repl_tempf$Location),
          FUN = function(x) mean(is.na(x))) %>% 
  filter(if_any(2:5, ~ . != 0)) %>%
  rename(Location = Group.1) %>%
  mutate(across(where(is.numeric), ~percent(.x, accuracy = 0.01)))
kable(t_naWGD, caption = "NA Percentages", align = "c")

# We will fill the WindGustDir NAs for Albany with Walpole
# and Newcastle with NorahHead, as these locations are geographically close

Walpole <- NA_Repl_tempf %>%
  filter(Location == "Walpole") %>%
  select(WindGustDir)

NorahHead <-  NA_Repl_tempf %>%
  filter(Location == "NorahHead") %>%
  select(WindGustDir)

NA_Replf <- NA_Repl_tempf %>%
  mutate(WindGustDir = ifelse(
    Location == "Albany", as.character(Walpole[match(Month, rownames(Walpole)), 1]),
    ifelse(
      Location == "Newcastle", as.character(NorahHead[match(Month, rownames(NorahHead)), 1]),
    as.character(WindGustDir))) %>%
    factor(levels = levels(NA_Repl_tempf$WindGustDir))
    ) %>%
  mutate(key = paste(Location, Month, sep = "|"))

# We can check that 24 NAs in NA_Repl_tempf are now 0 in NA_Replf
sum(is.na(NA_Repl_tempf))
sum(is.na(NA_Replf))

# Removing the temp files
rm(NA_Repl_temp)
rm(NA_Repl_tempf)

# Converting NA_Repl in a long format which will be easier for left_join use
# NA_Repl_l contains the values which will replace NAs in the data
NA_Repl_l <- NA_Repl %>%
  pivot_longer(cols = 3:ncol(NA_Repl), names_to = "Predictor", values_to = "NA_Value") %>%
  as.data.frame() %>%
  mutate(key = paste(Location, Month, Predictor, sep = "|")) %>%
  relocate(key, .before = 1) %>%
  select(key, NA_Value)

# Doing the same for the categorical data
NA_Replf_l <- NA_Replf %>%
  pivot_longer(cols = 3:ncol(NA_Replf), names_to = "Predictor", values_to = "NA_Value") %>%
  as.data.frame() %>%
  mutate(key = paste(Location, Month, Predictor, sep = "|")) %>%
  relocate(key, .before = 1) %>%
  select(key, NA_Value)


# Creating clean data----

# We now populate weather with this data
# We first replace the numerical NA values
# We use pivot-longer to move all numerical data into one column Value
# and use left-join to get the NA replacement value 
# (this is faster than using the match function)
weather_clean <- weather %>%
  mutate(key = paste(Location, month(Date), sep = "|")) %>%
  pivot_longer(cols = where(is.numeric), names_to = "Predictor", values_to = "Value") %>%
  mutate(key = paste(key, Predictor, sep = "|")) %>%
  left_join(NA_Repl_l, by = "key") %>%
  mutate(Value = ifelse(is.na(Value), NA_Value, Value)) %>%
  select(-c("key", "NA_Value")) %>%
  pivot_wider(names_from = Predictor, values_from = Value)

# We now replace the categorical NA values
weather_clean <- weather_clean %>%
  mutate(key = paste(Location, month(Date), sep = "|")) %>%
  pivot_longer(cols = 3:7,
               names_to = "Predictor", values_to = "Value") %>%
  mutate(key = paste(key, Predictor, sep = "|")) %>%
  left_join(NA_Replf_l, by = "key") %>%
  mutate(Value = ifelse(is.na(Value), NA_Value, as.character(Value))) %>%
  select(-c("key", "NA_Value")) %>%
  pivot_wider(names_from = Predictor, values_from = Value) %>%
  mutate( WindGustDir = factor(WindGustDir),
          WindDir9am = factor(WindDir9am),
          WindDir3pm = factor(WindDir3pm),
          Location = factor(Location),
          RainToday = factor(RainToday),
          RainTomorrow = factor(RainTomorrow))


# Re-ordering the columns as per the original column order
col_order_new <- data.frame(Name = colnames(weather_clean), Rank_new = 1:ncol(weather_clean)) %>%
  left_join(col_order, by = "Name")
weather_clean <- weather_clean[,order(col_order_new$Rank)]

# We can compare the number of NA values in weather and weather_clean (nil)
sum(is.na(weather))
sum(is.na(weather_clean))

# Using MissRanger (not used as too time consuming)
# DO NOT RUN (code kept only for reference)
# temp_w <- weather %>% select(-Date,-Location,-added)
# impute_w <- missRanger(temp_w, pmm.k = 3, splitrule = "extratrees", num.trees = 30)
# sum(is.na(impute_w))
# weather <- cbind(Location = weather$Location, Date = weather$Date, inpute_w)


# Adding Year and Month to the data set, as well as the variations of certain
# variables betwen 9am and 3pm, which Will be used in the models
# We do not include the variation on Cloud because Cloud is an index rather than
# a quantitative measurement
weather_clean <- weather_clean %>% 
  mutate(VarHumidity = Humidity3pm - Humidity9am,
         VarTemp = Temp3pm - Temp9am,
         VarPressure = Pressure3pm - Pressure9am,
         VarWindSpeed = WindSpeed3pm - WindSpeed9am, 
         Year = year(Date),
         Month = month(Date))


# Creating indexes to identify the new Var parameters as well as the 
# the 9am indicators which they could replace in the analysis
index_var <- colnames(weather_clean) %in% c("VarHumidity",
                                              "VarTemp", "VarPressure",
                                              "VarWindSpeed")
names(index_var) <- colnames(weather_clean)
index_9am <- colnames(weather_clean) %in% c("Cloud9am", "Humidity9am",
                                             "Temp9am", "Pressure9am",
                                             "WindSpeed9am",
                                            "WindDir9am")
names(index_9am) <- colnames(weather_clean)

# We also create an index of variables highly correlated with others,
# which we may want to remove from the analysis
index_rem <- index_9am | 
  colnames(weather_clean) %in% c("MinTemp", "MaxTemp", "WindSpeed3pm",
                                "WindDir3pm", "Evaporation", "Rainfall")


# Updating index_num and creating index_numym to include Year and Month
index_num <- sapply(weather_clean, is.numeric) & 
  !colnames(weather_clean) %in% c("Year", "Month")
index_numym <- sapply(weather_clean, is.numeric)
# And updating index_numRT for correlations
index_numRT <- index_num | colnames(weather_clean) == "RainTomorrow"


# Creating train and test sets on the working data set weather_clean
weather_test_clean <- weather_clean[index_part,] # Test set for model selection
weather_train_clean <- weather_clean[-index_part,] # Training set for model definition


# Further correlations----

# Computing the correlations between the retained significant numeric variables
correl_sign <- cor(weather_clean[index_num & !index_rem],
                    use="pairwise.complete.obs")

# Plotting the correlations with 5 clusters. We can see 
# that the Var variables have relatively low correlation with the other variables
# Some correlation remains between Humidity, SunShine and VarTemp, we will
# test the VIF of these parameters later in the process
g_cor_sign <- corrplot(correl_sign,
                  method = "circle",       
                  order = "hclust",         # Ordering method of the matrix
                  hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
                  addrect = 5,              # If order = "hclust", number of cluster rectangles
                  rect.col = 3,             # Color of the rectangles
                  rect.lwd = 3)             # Line width of the rectangles

# Plotting selected numerical predictors vs RainTomorrow
# in weather_clean. This graph is a follow-up of g_rel which
# was based on weather, before NA replacement. We observe similar trends.
# Humidity3pm seems the best predictor of Rain Tomorrow.
# Among the new variables, VarTemp and VarHumidity show some
# differentiation between No-Rain and Rain.
index_cor_clean <- index_numRT & !index_9am
g_rel_clean <- weather_clean[index_cor_clean] %>%
  relocate(RainTomorrow, .before = 1) %>%
  pivot_longer(cols = 2:ncol(.), 
               names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(RainTomorrow, Value)) +
  geom_boxplot(na.rm = TRUE, color = "steelblue") +
  facet_wrap(Predictor ~ ., scales = "free")
# Printing the graph takes a few seconds
print(g_rel_clean)

# We can plot interesting graphs with the newly created variable
# For instance, we expect low pressure and a drop in pressure before rain.
# So let's plot Pressure3pm and VarPressure
g_PvP <- weather_clean %>%
  ggplot(aes(Pressure3pm, VarPressure, color = RainTomorrow)) +
  geom_point(na.rm = TRUE) +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "VarPressure vs Pressure3pm",
       subtitle = "Color represents RainTomorrow")
# The plot confirms the expected trend, however we see that 
# Rain and No Rain still have significant overlaps
print(g_PvP)

# Similarly, we would expect a drop in Temperature ahead of rain
# So let's plot VarTemp and VarPressure
g_vPvT <- weather_clean %>%
  ggplot(aes(VarTemp, VarPressure, color = RainTomorrow)) +
  geom_point(na.rm = TRUE) +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "VarPressure vs VarTemp",
       subtitle = "Color represents RainTomorrow")
# Again the plot confirms the expected train but also the 
# fact that there is no clear-cut delimitation between Yes and No R
print(g_vPvT)




# Models----


# Defining the training and testing sets
# The reason for this step is to facilitate changes in training and
# test sets when required for alternative simulations, for instance
# with and without NA replacement

train_set <- weather_train_clean
test_set <- weather_test_clean


# No Rain model----

# We begin with a very simple model: since rain is rare, we will
# predict no-rain ! 
title_norain <- "Predicting no rain"
pred_norain <- rep_len("No", nrow(test_set)) %>%
  factor(levels = levels(train_set$RainTomorrow))
acc_norain <- mean(pred_norain == test_set$RainTomorrow)
# The accuracy is 77.5%. This is quite high and due to the fact
# that the proportion of rainy days is around 23% (as noted earlier,
# RainToday is Yes when RainFall >1)
print(acc_norain)
# The accuracy is equal to the mean of RainTomorrow "No" in the 
# test set
identical(acc_norain, mean(test_set$RainTomorrow == "No"))


# Plotting the accuracy of the simple model by Location
g_norain <- test_set %>%
  mutate(pred_no = "No") %>%
  group_by(Location) %>%
  summarise(accuracy = mean(pred_no == RainTomorrow),
            prop_NoRain = mean(RainTomorrow == "No") ) %>%
  ggplot(aes(prop_NoRain, accuracy)) +
  geom_point(color = "darkviolet") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Simple model predicting No Rain",
       subtitle = "Location Accuracy versus Proportion of No Rain Tomorrow")
# The plot shows a perfect alignment as expected. The reason for showing
# this plot is to highlight the fact the "Rain Prevalence" impacts
# results accuracy. We need to be mindful of this with future models.
print(g_norain)


# We compute the confusionMatrix to visualise the results of the model
# and compare with future models
t_confM_norain <- confusionMatrix(positive = "Yes", pred_norain, 
                      reference = test_set$RainTomorrow)

# We create a small formatting function for our results table
# since we will use it several times
f_format_res <- function(d, t){
  df <- data.frame(Model = t,
            Accuracy = d$overall["Accuracy"],
            Sensitivity = d$byClass["Sensitivity"],
            Specificity = d$byClass["Specificity"],
            Balanced_Acc = d$byClass["Balanced Accuracy"]) %>%
    mutate(across(where(is.numeric), ~ scales::percent(.x, accuracy = 0.1)))
  rownames(df) <- NULL
  return(df)
  }

# Storing the results of the no_rain model
t_res_norain <- f_format_res(t_confM_norain, title_norain)

# Printing the results. Note that:
# The positive value in the Confusion Matrix is Rain Tomorrow "Yes"
# Sensitivity = accuracy of predicting actual "Rain Tomorrow"
# Specificity = accuracy of predicting actual "No Rain Tomorrow"
# Balanced_Accuracy = average between Sensitivity and Specificity
# Obviously the Specificity is 1 (actual No-Rain predicted with 100% success),
# and Sensitivity is nil. Balanced accuracy is 50% only. This indicates
# that the "No Rain" model is a poor model
kable(t_res_norain,  format = "simple", caption = "Results",
      align = "c")


# Humidity Model----

# We continue with another model based on Humidity3pm. This variable
# indeed seemed to be a good predictor of RainTomorrow
# based on g_rel and g_rel_clean graphs shown earlier.

# The below table shows that the 3rd Quartile of Humidity3pm (60)
# with No-RainTomorrow is almost equal to the first Quartile (57)
# if there is RainTomorrow. Therefore there is limited overlap, which
# can be used for prediction.
t_Q3 <- tapply(weather_clean$Humidity3pm, weather_clean$RainTomorrow, summary)
print(t_Q3)

# Simple model based on Humidity3pm
# Model will predict RainTomorrow if Humidity3pm is above a certain value
title_humidity <- "Humidity3pm with cutoff"

# Training the Humidity model on the train set
cutoff_humidity_seq <- seq(50, 100, 1)
f_humidity <- function(c){
  pred_h <- ifelse(train_set$Humidity3pm > c, "Yes", "No")
  mean(pred_h == train_set$RainTomorrow)
}
train_humidity <- sapply(cutoff_humidity_seq, f_humidity)
g_hum_cutoff <- data.frame(cutoff_humidity = cutoff_humidity_seq, 
           trained_accuracy = train_humidity) %>%
  ggplot(aes(cutoff_humidity, trained_accuracy)) +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  gghighlight(trained_accuracy == max(trained_accuracy),
              label_key = cutoff_humidity,
              use_direct_label = TRUE,
              unhighlighted_params = list(color = "steelblue1")) +
  labs(title = "Accuracy vs Cutoff value - Humidity model",
       subtitle = "Optimal cutoffs highlighted")
  
print(g_hum_cutoff)

# The optimal cutoff is 77
cutoff_humidity <- cutoff_humidity_seq[which.max(train_humidity)]
print(cutoff_humidity)

# Testing the humidity model on the test set
# If Humidity3pm is above the cutoff, we predict rain. If not, we
# predict No Rain.
pred_humidity <- ifelse(test_set$Humidity3pm > cutoff_humidity,
                      "Yes", "No")  %>%
  factor(levels = levels(train_set$RainTomorrow))
acc_humidity <- mean(pred_humidity == test_set$RainTomorrow)
# The accuracy is 82.3%
print(acc_humidity)

# Balanced accuracy has improved to 65% but with still poor Sensitivity 35%
t_confM_Hum <- confusionMatrix(positive = "Yes", pred_humidity, reference = test_set$RainTomorrow)

t_res_Hum <- f_format_res(t_confM_Hum, title_humidity)
t_res1 <- bind_rows(t_res_norain, t_res_Hum)
kable(t_res1,  format = "simple", caption = "Results",
      align = "c")


# The cutoff is surprisingly high. Based on the quantile table
# for Humidity3pm, we were expecting a cutoff around 60.
# We can understand this by plotting the third quartile of Humidity3pm
g_Humidity3pm_loc <- weather_clean %>% 
  select(Location, Humidity3pm, RainTomorrow) %>%
  group_by(Location, RainTomorrow) %>%
  summarise(Q3 = quantile(Humidity3pm, probs = 0.75)) %>%
  ggplot(aes(Location, Q3, color = RainTomorrow)) +
  geom_point() +
  geom_hline(yintercept = cutoff_humidity, lty = 2,
             color = "steelblue") + 
  coord_flip() +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "Humidity3pm Third Quartile",
       subtitle = "Dotted line represents optimal cutoff")

# The plot shows that humidity levels have very different ranges per 
# location. In particular, there are many locations where Humidity levels
# are high even in the case of no rain and much higher than the third
# quartile (60). This pushes the model cutoff higher.
# This also suggests that using a single cutoff is an over-simplification.
# In the next section, we will test the Location:Humidity interaction.
print(g_Humidity3pm_loc)

# By plotting accuracy versus the proportion of no Rain in the
# test set, we can see that Rain prevalence is impacting the 
# accuracy in this model too, to a certain extent

loc_mean_NoRT <- test_set %>%
  group_by(Location) %>%
  summarise(prop_NoRain = mean(RainToday == "No")) # computing No-Rain
# prevalence using RainToday

g_hum_acc <- test_set %>%
  mutate(pred_r = pred_humidity) %>%
  group_by(Location) %>%
  summarise(accuracy = mean(pred_r == RainTomorrow)) %>%
  inner_join(loc_mean_NoRT, by = "Location") %>%
  ggplot(aes(prop_NoRain, accuracy)) +
  geom_point(color = "darkviolet") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Model predicting Rain with cutoff on Humidity3pm",
       subtitle = "Location Accuracy versus Proportion of No Rain Tomorrow") +
  geom_smooth(se = FALSE)

print(g_hum_acc)


# GLM model----

# GLM Step 1----
# Starting with a glm using selected significant variables


# Rather than writing manually the formula for the glm parameters,
# we select the variables with indexes and use the paste function

# We define a set of parameters using all the available
# parameters (this does not include Date but includes
# Year and Month), but excluding the highly correlated ones. 
# Factors like Location and WindGustDir are included.
# The parameters will be used in the glm model.

index_date <- colnames(train_set) == "Date"
index_RT <- colnames(train_set) == "RainTomorrow"
index_sign <- !index_date & !index_RT & !index_rem # significant parameters
param_sign <- as.formula(paste("RainTomorrow",
                                 paste(colnames(train_set[index_sign]), collapse = "+"),
                                 sep = "~"))
# Printing the parameters which will be used in glm
print(param_sign)

# As we will run several GLM models in this section, we define a function
# which computes all the needed output. The function will take the chosen
# parameters as an input and will produce a list containing the following
# output:
# Fit = the fitted model
# Fit_Summary = summary of the fitted model (includes p_values)
# Pred = the predictions
# Accuracy = the accuracy measured on the test set
# Graph_pval = a graph showing high p-values
# Table_VIF = a table containing VIF (variance inflation factor,
# see further explanations further down)
# Graph_VIF = a graph showing high VIF

# Note: mt below stands for model title, used in graphs and some tables

f_glm_list <- function(p, mt){
  
  # fitting the glm model
  fit_g <- glm(formula = p,
               data = train_set, 
               family = "binomial")
  
  print(mt)
  print(summary(fit_g))
  
  # computing predictions and accuracy
  pred_g <- predict(fit_g, newdata = test_set,
                    type = "response")
  pred_g <- ifelse(pred_g > 0.5, "Yes", "No") %>%
    factor(levels = levels(weather_train$RainTomorrow))
  acc_g <- mean(pred_g == test_set$RainTomorrow)
  
  # storing the summary in a data frame
  fit_sum <- summary(fit_g)$coefficients %>% data.frame() %>%
    rename(p_value = Pr...z..) %>%
    mutate(Variable = rownames(.))
  
  # plotting high p_values
  g_pval <- fit_sum %>%
    mutate(Variable = reorder(rownames(.), p_value)) %>%
    filter(p_value > 0.001) %>%
    ggplot(aes(Variable, p_value)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    geom_hline(yintercept = 0.05, color = c_red) +
    geom_text(aes(x = 1, y = 0.07, label = "5%"), 
              colour = "red", size = 3) +
    labs(title = "GLM model, p_values greater than 0.1%",
         subtitle = mt)
  
  # computing and plotting VIF
  t_vif <- VIF(fit_g) %>% as.data.frame() %>%
    rename(VIF = "GVIF^(1/(2*Df))") %>%
    mutate(Variable = rownames(.))
  g_vif <- t_vif %>% 
    mutate(Variable = reorder(Variable, VIF)) %>%
    filter(VIF > 1) %>%
    ggplot(aes(Variable,VIF)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    geom_hline(yintercept = 5, color = c_red) +
    labs(title = "GLM model - Variable Inflation Factor (VIF) > 1",
         subtitle = mt)
  
  # function output is a list
  return(list(Fit = fit_g, 
              Fit_Summary = fit_sum,
              Pred = pred_g, 
              Accuracy = acc_g,
              Graph_pval = g_pval, 
              Table_VIF = t_vif,
              Graph_VIF = g_vif))
}

# Assigning a title to the model for future reference
# We will call this model "Global GLM" as we will later compare it 
# with a separate model by location, which we will call Local GLM
# As this model uses significant predictors only, we identify it with
# the suffix _sign
title_glm_sign <- "Global GLM"

# Running the glm model on the defined significant parameters
# Running the function takes a few seconds
glm_sign <- f_glm_list(param_sign, title_glm_sign)

# The summary and the below graph show that all retained variables 
# have low p-values with three exceptions:
# - certain values of WindGustDir have high p_values however N and W
# combinations have high significance, as anticipated in the data
# exploration, indicating that we should retain this variable
# - certain locations are not significant but we will ignore this
# as location will be handled separately in the next step
# - Year is not highly significant (p_value 1%) (however we tested
# separately that removing it reduces accuracy)
# Note that variables not appearing on the graph have p_values lower
# than 1%
print(glm_sign$Graph_pval)


# We also look at the multicollinearity which
# can distort p-values
# The most common way to detect multicollinearity
# is by using the variance inflation factor (VIF), 
# which measures the correlation and strength of 
# correlation between the predictor variables in a regression model.

# The value for VIF starts at 1 and has no upper limit. 
# A general rule of thumb for interpreting VIFs is as follows:
# A value of 1 indicates there is no correlation 
# between a given predictor variable and any other predictor 
# variables in the model.
# A value between 1 and 5 indicates moderate correlation but this is
# often not severe enough to require attention.
# A value greater than 5 indicates potentially severe correlation
# between a given predictor variable and other predictor variables
# in the model. In this case, the coefficient estimates
# and p-values in the regression output are likely unreliable.

# We use the VIF function from DescTools

# The below plot shows that all variables are well below 5.
# This confirms the significance of the chosen variables
print(glm_sign$Graph_VIF)

# Assessing the accuracy of the model on the test set
# We get an accuracy around 84.9%
acc_glm_sign <- glm_sign$Accuracy
print(acc_glm_sign)

# Sensitivity has improved to 52% and Balanced Accuracy to 73%
t_confM_glm_sign <- confusionMatrix(positive = "Yes", glm_sign$Pred, 
                                    reference = test_set$RainTomorrow )
t_res_glm_sign <- f_format_res(t_confM_glm_sign, title_glm_sign)
kable(t_res_glm_sign,  format = "simple", caption = "Results",
      align = "c")

# Computing the ANOVA table to analyze the importance of 
# each variable
# WARNING: TAKES AROUND ONE MINUTE
t_anova_glm_sign <- data.frame(anova(glm_sign$Fit))
rownames(t_anova_glm_sign)[1] <- "Intercept"

# Plotting ANOVA
g_anova_glm_sign <- t_anova_glm_sign %>%
  mutate(Variable = rownames(.)) %>%
  filter(!Variable == "Intercept") %>%
  mutate(Variable = reorder(Variable, Deviance)) %>%
  ggplot(aes(Variable, Deviance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Analysis of Variance",
       subtitle = title_glm_sign)
# The ANOVA plot shows the most important variables at the top
# of the graph, mainly Humidity3pm, Sunshine, WindGustSpeed, Location
# However we will not discard the other variables, which still contribute
# and have low VIF and low p-values as we have seen (for instance, even
# removing Year would decrease accuracy slightly)
g_anova_glm_sign

# GLM Humidity----
# Note that a model based on Location and Location:Humidity only
# would have an accuracy of 83.1%
title_glm_LocHum <- "GLM Location & Humidity"
param_LocHum <- as.formula("RainTomorrow ~ Location + 
                            Location:Humidity3pm")
glm_LocHum <- f_glm_list(param_LocHum, title_glm_LocHum)
acc_glm_LocHum <- glm_LocHum$Accuracy
print(acc_glm_LocHum)

# Computing the confusion Matrix
t_confM_glm_LocHum <- confusionMatrix(positive = "Yes", glm_LocHum$Pred, 
                                    reference = test_set$RainTomorrow )
t_res_glm_LocHum <- f_format_res(t_confM_glm_LocHum, title_glm_LocHum)

# The below table shows that the global GLM with significant variables
# has the best accuracy, Sensitivity (ability to correctly predict Rain) 
# and balanced accuracy, at the price of a slightly reduced Specificity
t_res2 <- bind_rows(t_res1, t_res_glm_LocHum, t_res_glm_sign)
kable(t_res2,  format = "simple", caption = "Results",
      align = "c")

# In order to compare the global model with other models,
# we compute and store the average accuracy per location
t_loc_globglm <- data.frame(Location = test_set$Location,
                            RainTomorrow = test_set$RainTomorrow,
                            pred_glm = glm_sign$Pred) %>%
  group_by(Location) %>%
  summarise(glob_glm_acc = mean(pred_glm == RainTomorrow))



# NA replacement Impact----

# It is important to check whether NA replacement has impacted
# GLM results. We will therefore compare the current results with
# a GLM model applied on the data excluding NAs.

# We temporarily change the train and test sets to take data
# without any NA. We revert to the weather_train and weather_test sets,
# which contain the NAs and use na.omit to remove these rows
train_set <- weather_train %>% na.omit() %>%
  mutate(VarHumidity = Humidity3pm - Humidity9am,
         VarTemp = Temp3pm - Temp9am,
         VarPressure = Pressure3pm - Pressure9am,
         VarWindSpeed = WindSpeed3pm - WindSpeed9am, 
         Year = year(Date),
         Month = month(Date))
test_set <- weather_test %>% na.omit() %>%
  mutate(VarHumidity = Humidity3pm - Humidity9am,
         VarTemp = Temp3pm - Temp9am,
         VarPressure = Pressure3pm - Pressure9am,
         VarWindSpeed = WindSpeed3pm - WindSpeed9am, 
         Year = year(Date),
         Month = month(Date))

# We note the significant reduction of the size of the data sets
# In particular, we have only 26 locations instead of 49
length(unique(train_set$Location))
length(unique(weather_clean$Location))
# We test that the same locations are in the test set
identical(unique(train_set$Location), unique(test_set$Location))

# We no perform the GLM on this reduced set
title_glm_noNA <- "GLM excluding NAs"
# Running the glm takes a few seconds
glm_noNA <- f_glm_list(param_sign, title_glm_noNA)

# Storing key results
t_confM_glm_noNA <- confusionMatrix(positive = "Yes", glm_noNA$Pred, 
                                    reference = test_set$RainTomorrow )
t_res_glm_noNA <- f_format_res(t_confM_glm_noNA, title_glm_noNA)
t_res_NAnoNA <- bind_rows(t_res_glm_sign, t_res_glm_noNA)

# The table shows a better accuracy when NAs are fully excluded
kable(t_res_NAnoNA,  format = "simple", caption = "Results with and without NAs",
      align = "c")

# However, this higher accuracy is partly due to the location mix of the
# remaining 26 locations. We can see this by computing the accuracy
# per location
t_loc_noNA <- data.frame(Location = test_set$Location,
                         RainTomorrow = test_set$RainTomorrow,
                         pred_glm = glm_noNA$Pred) %>%
  group_by(Location) %>%
  summarise(glob_glm_noNA = mean(pred_glm == RainTomorrow))

# Plotting accuracy per location for global GLM and GLM excluding NAs
g_NAnoNA <- t_loc_globglm %>%
  mutate(Location = reorder(Location, glob_glm_acc)) %>%
  ggplot(aes(Location, glob_glm_acc)) +
  geom_point(color = "steelblue") +
  geom_point(data = t_loc_noNA, aes(Location, glob_glm_noNA),
             color = "red", na.rm = TRUE) +
  labs(title = "Global GLM (blue) and GLM excluding NAs (red) accuracy per location") +
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()
# The plot shows that in 14 cases, no-NA accuracy is better however in
# 12 cases, no-NA accuracy is worse. It is therefore a mixed picture.
# We conclude that there is no evidence that NA replacement have 
# significantly distorted the GLM results.
g_NAnoNA 


# We can test the glm model on weather_clean with exactly the same
# locations
title_glm_26loc <- "Global GLM same locations"
loc_noNA <- unique(train_set$Location)
train_set <- weather_train_clean %>%
  filter(Location %in% loc_noNA)
test_set <- weather_test_clean %>%
  filter(Location %in% loc_noNA)
glm_26loc <- f_glm_list(param_sign, title_glm_26loc)
# Storing key results
t_confM_glm_26loc <- confusionMatrix(positive = "Yes", glm_26loc$Pred, 
                                    reference = test_set$RainTomorrow )
t_res_glm_26loc <- f_format_res(t_confM_glm_26loc, title_glm_26loc)
t_res_NA26loc <- bind_rows(t_res_glm_sign, t_res_glm_noNA, t_res_glm_26loc)

# The table shows that results are now very close
kable(t_res_NA26loc,  format = "simple", caption = "Results with and without NAs",
      align = "c")


# Parameters impact----

# We now restore the train and test sets with the clean data
train_set <- weather_train_clean
test_set <- weather_test_clean

# Before moving to the next step and exploring interactions
# we need to check whether selecting only the significant parameters
# was the right choice. We will therefore compare with two other models:
# - a model based on the original parameters (excluding the created Var
# parameters)
# - a model based on the significant parameters without removing the highly
# correlated parameters


# Original Params----

# A model based on all original parameters would have an accuracy of
# 84.8%, with Temperature indicators having a high VIF
# and several indicators having high p_values, therefore producing
# a less reliable model than the "Significant Variables" model
index_all <- !index_date & !index_var &!index_RT
param_all <- as.formula(paste("RainTomorrow",
                               paste(colnames(train_set[index_all]), collapse = "+"),
                               sep = "~"))
print(param_all)
title_glm_all <- "GLM all original vars"
# running glm (takes a few seconds)
glm_all <- f_glm_list(param_all, title_glm_all)
acc_glm_all <- glm_all$Accuracy
print(acc_glm_all)
print(glm_all$Graph_VIF)
print(glm_all$Graph_pval)

# Storing results for future printing
t_confM_glm_all <- confusionMatrix(positive = "Yes", glm_all$Pred, test_set$RainTomorrow)
t_res_glm_all <- f_format_res(t_confM_glm_all, title_glm_all)



# All new params----

# A model based on all "new" parameters would have an accuracy of
# 84.9%, however with two Temperature indicators having a high VIF
index_allnew <- !index_date & !index_9am &!index_RT
param_allnew <- as.formula(paste("RainTomorrow",
                              paste(colnames(train_set[index_allnew]), collapse = "+"),
                              sep = "~"))
print(param_allnew)
title_glm_allnew <- "GLM all new vars"
# running glm (takes a few seconds)
glm_allnew <- f_glm_list(param_allnew, title_glm_allnew)
acc_glm_allnew <- glm_allnew$Accuracy
print(acc_glm_allnew)
print(glm_allnew$Graph_VIF)
print(glm_allnew$Graph_pval)
# Storing results for future printing
t_confM_glm_allnew <- confusionMatrix(positive = "Yes", glm_allnew$Pred, test_set$RainTomorrow)
t_res_glm_allnew <- f_format_res(t_confM_glm_allnew, title_glm_allnew)


# Step 1 Summary----

# We can summarize the results of this section as below
t_res3 <- bind_rows(t_res2, t_res_glm_all, t_res_glm_allnew)

kable(t_res3, caption = "Model accuracy on Test set",
      format = "simple",
      align = "c")

# Based on the above table, adding other variables does not improve
# results. The GLM model with significant variables also
# offers the advantage of having variables with low VIF
# It is therefore the preferred model in this section.




# GLM Step 2----
# Assessing the interactions between significant variables


# In step 1, we did not include any interactions between the variable.
# We now run a model with two-by-two interactions to see which 
# interactions are significant

# We will however not include interactions for Location (will be handled separately
# in the next stage), some of the categorical predictors and Year, in order
# to avoid too much complexity

# Defining the parameter
index_loc <- colnames(train_set) == "Location"
index_no_inter <- index_loc | colnames(train_set) %in% c("Year", "WindGustDir")
index_inter <- index_sign & !index_no_inter
p_1 <- "RainTomorrow ~"
p_2 <- paste(colnames(train_set[index_no_inter]), collapse = "+")
p_3 <- paste(" + (",
             paste(colnames(train_set[index_inter]), collapse = "+"), ")^2", sep = "")
param_inter <- as.formula(paste(p_1, p_2, p_3, sep=""))
# The ()^2 in the formula will create two by two interactions
print(param_inter)


# Running the model (takes a few seconds)
title_glm_inter <- "GLM with interactions"
glm_inter <- f_glm_list(param_inter, title_glm_inter)

# We see an improvement in the residual deviance
# The accuracy has improved slightly to 85.2%
acc_glm_inter <- glm_inter$Accuracy
print(acc_glm_inter) 

# Storing key results
t_confM_glm_inter <- confusionMatrix(positive = "Yes", glm_inter$Pred, test_set$RainTomorrow)
t_res_glm_inter <- f_format_res(t_confM_glm_inter, title_glm_inter)

# Many interactions are not significant when looking at the summary
# We will filter down the number of interactions and re-run a model
# We keep only interactions with a p-value less than 5% and VIF < 6
inter_sign <- glm_inter$Fit_Summary %>%
  mutate(Variable = str_replace(Variable, "RainTodayYes", "RainToday")) %>%
  inner_join(glm_inter$Table_VIF, by = "Variable") %>% 
  filter(p_value < 0.05 &
           str_detect(Variable, ":") & VIF < 6)
    
kable(inter_sign$Variable)

# We run a model with these interactions only
p_1 <- "RainTomorrow ~"
p_2 <- paste(colnames(train_set[index_sign]), collapse = "+")
p_3 <- paste(" +",
             paste(inter_sign$Variable, collapse = "+"), sep = "")
param_inter_sign <- as.formula(paste(p_1, p_2, p_3, sep=""))
print(param_inter_sign)

# Running the model (takes a few seconds)
title_glm_inter_sign <- "GLM main interactions"
glm_inter_sign <- f_glm_list(param_inter_sign, title_glm_inter_sign)

# Accuracy is 84.8%, taking us back to the same results
# as the GLM model with no interactions
acc_glm_inter_sign <- glm_inter_sign$Accuracy
print(acc_glm_inter_sign)

# Storing key results
t_confM_glm_inter_sign <- confusionMatrix(positive = "Yes", glm_inter_sign$Pred, test_set$RainTomorrow)
t_res_glm_inter_sign <- f_format_res(t_confM_glm_inter_sign, title_glm_inter_sign)

# Some variables have high VIF, due to the inter-correlations
# and some have high p-values
glm_inter_sign$Graph_pval
glm_inter_sign$Graph_VIF

# We could continue by computing the ANOVA and selecting
# variables further. However, this would take us to an accuracy
# of 84.8% or below. Therefore we conclude that adding interactions
# does not improve the GLM model. Going forward, we retain the 
# significant parameters only.

# We summarize the results of this section (reminder: Global GLM
# is our preferred model, with significant variables only)

t_res4 <- bind_rows(t_res3, t_res_glm_inter, t_res_glm_inter_sign)
kable(t_res4, caption = "Model accuracy on Test set",
      format = "simple",
      align = "c")



# GLM Step 3----
# Running separate models per location


# We will now compare the global model with a model that treats
# each location as a separate data subset.
# Indeed there are reasons to believe that parameters may behave
# differently based on the location

# We use the significant parameters and remove the location only
index_l <- !index_loc & index_sign
param <- as.formula(paste("RainTomorrow",
                              paste(colnames(train_set[index_l]), collapse = "+"),
                              sep = "~"))
print(param)

# We build a new function which allows to parametrize the data set
f_glm <- function(d){glm(formula = param,
                         data = d, 
                         family = "binomial")}

# We then create a list of model, one for each location in the training set
# Takes a few seconds to run
m_loc <- lapply(split(train_set, factor(train_set$Location)),
            FUN = f_glm)

# m_loc contains 49 GLM models, one per location
length(m_loc)

# We split the test set into locations as well
test_list <- split(test_set, factor(test_set$Location))

# We can verify that the names of both lists are identical
# Which will allow us to use the Map function
identical(names(m_loc), names(test_list))

# We can now apply the models in m_loc to the test_set, 
# separately for each location. This is done through the Map function.
# We use the augment function in order to add the fitted data to the
# test set
temp_res <- Map(augment, m_loc, 
                newdata = test_list,
                type.predict = "response")

# Converting the list back into a data frame
test_res <- data.frame(Reduce(rbind, temp_res))
# Computing the predictions
test_res <- test_res %>%
  rename(fit_glm_loc = .fitted)
test_res <- test_res %>%
  mutate(pred_glm_loc = ifelse(fit_glm_loc > 0.5, "Yes", "No") %>%
           factor(levels = levels(train_set$RainTomorrow)))

# Note that we can access the model for each location
# For instance for Canberra:
summary(m_loc[["Canberra"]])

# Overall accuracy is 85.8%
acc_glm_loc <- mean(test_res$pred_glm_loc == test_res$RainTomorrow)
print(acc_glm_loc)

# Storing key results
title_glm_loc <- "Local GLM"
t_confM_glm_loc <- confusionMatrix(positive = "Yes", test_res$pred_glm_loc, test_res$RainTomorrow)
t_res_glm_loc <- f_format_res(t_confM_glm_loc, title_glm_loc)


# The table shows that a separate GLM model by location has better
# accuracy than the global GLM models. The balanced accuracy is also
# improved due to higher Sensitivity.
t_res5 <- bind_rows(t_res2, t_res_glm_loc)

kable(t_res5, digits = 3, caption = "GLM Accuracy",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))


# Testing cutoff shows that the optimal probability cutoff could
# be 0.51 however the improvement is not significant and would
# decrease Sensitivity by reducing Yes predictions. We will
# retain the usual cutoff.
cutoff <- seq(0.4, 0.6, 0.01)
f_cutoff <- function(c){
  t <- test_res %>%
    mutate(pred = ifelse(fit_glm_loc > c, "Yes", "No") %>%
             factor(levels = levels(train_set$RainTomorrow)))
  mean(t$pred == t$RainTomorrow)
}
test_cutoff <- sapply(cutoff, f_cutoff)
g_prob_cutoff <- data.frame(cutoff, accuracy = test_cutoff) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point(color = "steelblue") +
  gghighlight(accuracy == max(accuracy),
              label_key = cutoff,
              use_direct_label = TRUE,
              unhighlighted_params = list(color = "steelblue1")) +
  labs(title = "GLM Accuracy versus probability cutoff",
       subtitle = "Optimal cutoff highlighted on graph")
print(g_prob_cutoff)
max(test_cutoff)
cutoff[which.max(test_cutoff)]


# Computing accuracy per location
# We also add latitude and longitude which we will use in a map plot
t_loc <- test_res %>%
  group_by(Location) %>%
  summarise(loc_glm_acc = mean(pred_glm_loc == RainTomorrow)) %>%
  left_join(locations, by = "Location")

# We now plot the accuracy per location for both global and 
# local models
# Note that the accuracy per location for the global model was
# previously stored in t_loc_globglm
g_glm <- t_loc %>%
  mutate(Location = reorder(Location, loc_glm_acc)) %>%
  ggplot(aes(Location, loc_glm_acc)) +
  geom_point(color = "steelblue") +
  geom_point(data = t_loc_globglm, aes(Location, glob_glm_acc),
                 color = "red") +
  labs(title = "Global GLM (red) and Local GLM (blue) accuracy per location",
       subtitle = "Local model has better accuracy in a majority of cases") +
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()
# The plot shows that the local model (one model per location)
# has better accuracy in a majority of cases than a global GLM model
print(g_glm)

# We also plot the accuracy versus the No-Rain proportion

g_glm_vsNo <- t_loc %>%
  left_join(loc_mean_NoRT, by = "Location") %>%
  rename(accuracy = loc_glm_acc) %>%
  ggplot(aes(prop_NoRain, accuracy)) +
  geom_point(color = "darkviolet") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Local GLM Model",
       subtitle = "Location Accuracy versus Proportion of No Rain Tomorrow") +
  geom_smooth(se = FALSE)
# We can see that whilst there is still a link between the No Rain prevalence
# and the model accuracy, the link is not very strong. This is consistent
# with the better balanced accuracy observed.
print(g_glm_vsNo)




# GLM Step 4----
# Trying to understand where the model fails


# Plotting the locations on a map with their accuracy
# We will use the m_map function
# Defining variables used in the plot
m_data <- t_loc # contains location co-ordinates and accuracy
m_color <- sym("loc_glm_acc") # color will be based on accuracy
m_color_begin <- 0 # used for color scale
m_color_end <- 1 - min(m_data$loc_glm_acc) # used for color scale
m_title <- "Local GLM accuracy"
m_subtitle <- "Color of points showing accuracy levels"
m_caption <- paste("Accuracy range", 
                   paste(percent(range(m_data$loc_glm_acc), accuracy = 0.1),
                         collapse = " - "),
                   sep = " : ") 
# Plotting the map
# The map shows the accuracy is lower in coastal locations
# These are areas where we expect the weather to change frequently,
# due to the influence of the ocean.
map_glm_loc <- f_map(m_data, m_color, m_color_begin, m_color_end, 
                     m_title, m_subtitle, m_caption)
print(map_glm_loc)


# Focusing on Norfolk Island data, which is the location where 
# the GLM model gave the lowest accuracy

# Pulling key results
title_glm_Norfolk <- "Norfolk Island"

# Pulling the predictions for Norfolk Island
t1 <-  test_res %>% 
  filter(Location == "NorfolkIsland") %>%
  pull(pred_glm_loc)
# Pulling actual RainTomorrow for Norfolk Island
t2 <- test_res %>%
  filter(Location == "NorfolkIsland") %>%
  pull(RainTomorrow)
# Computing the confusion matrix
t_confM_glm_Norfolk <- confusionMatrix(positive = "Yes", t1, reference = t2)
# Formatting for printing
t_res_glm_Norfolk <- f_format_res(t_confM_glm_Norfolk, title_glm_Norfolk)

# Comparing Norfolk results with the overall results
# All indicators are lower, as shown in the below table
t_res_comp <- bind_rows(t_res_glm_loc, t_res_glm_Norfolk)
kable(t_res_comp, digits = 3, caption = "Local GLM Accuracy: overall vs Norfolk ",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))

# Let's dig deeper in Norfolk's data
w_Norfolk <- weather_clean %>%
  filter(Location == "NorfolkIsland")

# Let's look at the relationship between the numeric variables
# and RainTomorrow in Norfolk Island
g_rel_Norfolk <- w_Norfolk[index_cor_clean] %>%
  relocate(RainTomorrow, .before = 1) %>%
  pivot_longer(cols = 2:ncol(.), 
               names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(RainTomorrow, Value)) +
  geom_boxplot(na.rm = TRUE, color = "steelblue") +
  facet_wrap(Predictor ~ ., scales = "free") +
  labs(title = "Norfolk Island")
# The graph helps to understand why the model fails: there are 
# significant overlaps in all variables between Rain and No-Rain
print(g_rel_Norfolk)

# The plot below shows that there are no clear-cut separations
# on a combination of Humidity3pm and WindGustSpeed, for example,
# between No Rain Tomorrow and Rain Tomorrow. 
# The same is true for Humidity3pm:Sunshine and Humidity3pm:Temp3pm
# This shows that it is difficult for any model to do better
g_Nf_HW <- w_Norfolk %>%
  ggplot(aes(Humidity3pm, WindGustSpeed, color = RainTomorrow)) +
  geom_point() +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "Norfolk Island",
        subtitle = "Interaction between Humidity3pm and WindGustSpeed")
print(g_Nf_HW)

g_Nf_HS <- w_Norfolk %>%
  ggplot(aes(Humidity3pm, Sunshine, color = RainTomorrow)) +
  geom_point() +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "Norfolk Island",
       subtitle = "Interaction between Humidity3pm and Sunshine")
print(g_Nf_HS)

g_Nf_HT <- w_Norfolk %>%
  ggplot(aes(Humidity3pm, Temp3pm, color = RainTomorrow)) +
  geom_point() +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "Norfolk Island",
       subtitle = "Interaction between Humidity3pm and Temp3pm")
print(g_Nf_HT)



# RandomForest model----

# We will add a Random Forest model

# Global Random Forest----
# We start with a global model using all significant variables
param_rf <- param_sign
print(param_rf)

title_rf_glob <- "Global Random Forest"

# Should the model be trained?
modelLookup("rf")

# Nodesize
# The below simulation was tried to find the optimal nodesize
# The outcome shows that the nodesize does not impact results significantly
# Therefore the simulation is kept as comments for reference only
# WARNINGS: takes a couple of minutes to run
# nodesize <- seq(1, 12, 1)
# acc <- sapply(nodesize, function(ns){
#   train_rf <- randomForest(formula = param_rf,
#                            data = train_set,
#                            nodesize = ns,
#                            ntree = 50)
#   pred_rf <- predict(train_rf, newdata = test_set) %>%
#     factor(levels = levels(weather_train$RainTomorrow))
#   mean(pred_rf == test_set$RainTomorrow)
# })
# qplot(nodesize, acc)

# mtry
# Likewise, the value of mtry does not have a material impact

# We therefore run the model without pre-training
set.seed(1000, sample.kind = "Rounding")
# Running the model (takes a few seconds)
fit_rf <- randomForest(formula = param_rf,
                       data = train_set,
                       ntree = 50)
pred_rf <- predict(fit_rf, newdata = test_set) %>%
  factor(levels = levels(weather_train$RainTomorrow))
# The accuracy is 85.2%
acc_rf_glob <- mean(pred_rf == test_set$RainTomorrow)
print(acc_rf_glob)

# Storing key results
t_confM_rf_glob <- confusionMatrix(positive = "Yes", pred_rf, test_set$RainTomorrow)
t_res_rf_glob <- f_format_res(t_confM_rf_glob, title_rf_glob)

# The plot shows that beyond 40 trees there is little improvement
# in the overall accuracy
plot(fit_rf)
print(fit_rf)
# Importance of variables
rf_glob_imp <- data.frame(fit_rf$importance) %>%
  mutate(Variable = rownames(.))
g_rf_glob_imp <- rf_glob_imp %>% 
  mutate(Variable = reorder(Variable, MeanDecreaseGini)) %>%
  ggplot(aes(Variable, MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(title = "Importance of each Variable",
       subtitle = title_rf_glob)
print(g_rf_glob_imp)


# The comparison with the global GLM shows that GLM and RandomForest
# attribute different importances to various parameters. For instance
# Year is not negligible in RF.
grid.arrange(g_anova_glm_sign, g_rf_glob_imp,
             ncol = 2)



# Local Random Forest----

# We now build a separate RandomForest per location
title_rf_loc <- "Local Random Forest"
param_rfl <- param
print(param_rfl)

# We define a function with the data set as parameter
f_rf <- function(d){randomForest(formula = param_rfl,
                       data = d,
                       ntree = 50)}

# We apply the function to each location separately to get separate RF
# models per location
# Takes a few seconds to run
m_loc <- lapply(split(train_set, factor(train_set$Location)),
                FUN = f_rf)
# We apply the models to the previously obtained test_res, which
# is equal to the test set, with the added glm predictions
# this will allow us to progressively build an ensemble
test_list <- split(test_res, factor(test_res$Location))
# Since augment does not work for rf, we first pull the predictions
# and then add them to the test_set using bind_cols
temp_res <- Map(predict, m_loc, 
                newdata = test_list)
test_list_1 <- Map(bind_cols, test_list, "pred_rf_loc" = temp_res)
# We keep test_list as a list in order to apply further models
# Meanwhile we create a data frame with the predictions
test_res_1 <- data.frame(Reduce(rbind, test_list_1))
# We can compute the average accuracy of the Random Forest model
# The accuracy 85.5% is close to the global Random Forest so there is
# no significant benefit to a local model
acc_rf_loc <- mean(test_res_1$pred_rf_loc == test_res_1$RainTomorrow)
print(acc_rf_loc)

# Storing key results
t_confM_rf_loc <- confusionMatrix(positive = "Yes", test_res_1$pred_rf_loc, test_res_1$RainTomorrow)
t_res_rf_loc <- f_format_res(t_confM_rf_loc, title_rf_loc)

# We add the accuracy of the random forest models to the t_res table
t_res6 <- bind_rows(t_res_glm_loc, t_res_rf_glob, t_res_rf_loc)
# The table shows that both GLM and RF models have similar accuracies
kable(t_res6, caption = "Model accuracy on Test set",
      align = "c",
      format = "simple")


# XGBoost ----

# XGBoost is a decision-tree-based ensemble Machine Learning
# algorithm that uses a gradient boosting framework.

title_xg_glob <- "Global XG Boost"

# Converting train and test set to matrices (required by xg)
# XG Boost works only with numerical data :
# For Location, we will use latitude and longitude
# For the other categorical data, we convert factors to numeric
train_set_temp <- train_set %>%
  left_join(locations, by = "Location") %>%
  select(-c("Location", "state", "population")) %>%
  mutate(across(where(is.factor), ~as.numeric(.x)))
test_set_temp <- test_set %>%
  left_join(locations, by = "Location") %>%
  select(-c("Location", "state", "population")) %>%
  mutate(across(where(is.factor), ~as.numeric(.x)))
index_xg <- sapply(train_set_temp, is.numeric) & !colnames(train_set_temp) == "RainTomorrow"

data_xg <- as.matrix(train_set_temp[index_xg])
label_xg <- ifelse(train_set_temp$RainTomorrow == 1, 0, 1)
test_xg <- as.matrix(test_set_temp[index_xg])


#Training XG Boost----
# WARNING: RUNNING THIS PART IS OPTIONAL. TAKES AROUND 4 MINUTES.
# It is possible to skip directly to Running XG Boost

run_xg_train <- 0 # enter 1 to run the training

if(run_xg_train == 1) {
  
f_xg <- function(m, n, e){
  xg_glob <- xgboost(data = data_xg,
                     label = label_xg,
                     max.depth = m, #tree depth
                     booster = "gbtree", # gblinear",
                     eta = e,
                     nthread = 2, #number of CPU threads
                     nrounds = n, #number of passes on the data
                     eval.metric = "logloss", # = "error"
                     print_every_n = 10,
                     objective = "binary:logistic", #binary classification
                     verbose = 0)
  
  # Computing the predictions
  pred <- predict(xg_glob, test_xg)
  prediction <- as.numeric(pred > 0.5)
  pred_xg <- ifelse(prediction == 1, "Yes", "No") %>%
    factor(levels = levels(train_set$RainTomorrow))
  mean(pred_xg == test_set$RainTomorrow)}
range_m <- seq(2, 10, 1)
range_n <- seq(40, 60, 10)
range_xg <- expand.grid(range_m, range_n) %>%
  mutate(case = rownames(.))
train_xg <- mapply(f_xg, range_xg$Var1, range_xg$Var2, e = 0.5)
plot(range_xg$case, train_xg)
# Best results are obtained with m=6 and n=50
range_xg[which.max(train_xg),]

# Optimizing the learning rate
range_e <- seq(0.1, 1, 0.1)
train_xg <- mapply(f_xg, m = 6, n = 50, range_e)
plot(range_e, train_xg)
# Best results are obtained with e = 0.5
range_e[which.max(train_xg)]
} # End of if statement for XG training

# Running XG Boost----
# Warning: takes a few seconds
# The parameters are defined based on the previous trials
xg_glob <- xgboost(data = data_xg,
               label = label_xg,
                    max.depth = 6, #tree depth
                    booster = "gbtree", # gblinear",
                    eta = 0.5,
                    nthread = 2, #number of CPU threads
                    nrounds = 50, #number of passes on the data
                    eval.metric = "logloss", # = "error"
                    print_every_n = 10,
                    objective = "binary:logistic", #binary classification
                    verbose = 1)

# Computing the predictions
pred <- predict(xg_glob, test_xg)
prediction <- as.numeric(pred > 0.5)
pred_xg <- ifelse(prediction == 1, "Yes", "No") %>%
  factor(levels = levels(train_set$RainTomorrow))
# The accuracy is 86.1%
acc_xg_glob <- mean(pred_xg == test_set$RainTomorrow)
print(acc_xg_glob)
# Storing key results
t_confM_xg_glob <- confusionMatrix(positive = "Yes", pred_xg, reference = test_set$RainTomorrow)
t_res_xg_glob <- f_format_res(t_confM_xg_glob, title_xg_glob)

kable(t_res_xg_glob, 
      format = "simple",
      align = "c")

# Storing the predictions by Date and Location to populate the ensemble
pred_xg_temp <- data.frame(
  key = paste(test_set$Location, test_set$Date, sep = "|"),
  pred_xg = pred_xg)
test_res_temp <- test_res_1 %>% 
  mutate(key = paste(test_res_1$Location, test_res_1$Date, sep = "|")) 
test_res_2 <- test_res_temp %>%
  left_join(pred_xg_temp, by = "key") %>%
  select(-key)
# Checking that we are getting the same result in test_res
mean(test_res_2$pred_xg == test_res_2$RainTomorrow)

rm(pred_xg_temp)
rm(test_res_temp)
rm(train_set_temp)
rm(test_set_temp)

# Comparing with the other models
t_res7 <- bind_rows(t_res_glm_sign, t_res6, t_res_xg_glob)
# XG Boost achieves the best results so far
kable(t_res7, caption = "Model accuracy on test set",
      format = "simple",
      align = "c")

# We can visualize the importance of data in the XG Boost model
importance_matrix <- xgb.importance(model = xg_glob)
xgb.plot.importance(importance_matrix = importance_matrix)



# PCA + GLM----


# We have seen that certain predictors remain correlated, even
# after the replacement of morning values by var. We now try to 
# address this issue by doing a principal component analysis of
# the predictors, followed by a glm study.

# We do not expect an improvement in accuracy from this step, as 
# the theory predicts that results will be identical. However, some
# improvement could be observed if PCA allows to factor specific effects
# which disappeared when we removed certain non significant predictors
# in the previous GLM.
# In any case, PCA can contribute to more stable models because its
# components are de-correlated, and as such, is worth investigating.

# We will also use the results from PCA for other algorithms like KNN
# and QDA

# For the PCA, we retain only the numeric indicators
# We only exclude the 9am indicators, which are replaced by the Var KPIs
index_pca <- index_numym & !index_9am

# We apply the PCA to the train set, with data scaling
set.seed(1000, sample.kind = "Rounding")
pca <- prcomp(train_set[index_pca], scale. = TRUE)

# Out of 17 components, we need 15 components to cover at least
# 99% of the variance
t_pca <- t(summary(pca)$importance)

kable(t_pca, digits = 3, caption = "PCA Variance",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))

# We retain all the components, except numbers 8 and 11 which, 
# from experience have limited significance in the glm
rem <- c(8,11)
train_pca_temp <- as.data.frame(pca$x[,-rem])

# We add RainTomorrow as well as two categorical predictors which we 
# know from the previous glm are significant, Location, RainToday and
# WindGustDir
train_pca <- bind_cols(
  train_set %>% select(RainTomorrow, Location, RainToday, WindGustDir),
  train_pca_temp)

# We prepare the test set by applying the pca to it:
t1 <- predict(pca, newdata = test_set[index_pca])
dim(t1)
t2 <- data.frame(t1)
test_pca <- bind_cols(
  test_set %>% select(RainTomorrow, Location, RainToday, WindGustDir),
  t2[,-rem])

# We now have a train set (train_pca) and a test set (test_pca)
# We will train glm on the train set and test it on the test set


# PCA + Global GLM----
# We start with a global GLM (one model for all locations)
# We prepare the GLM parameters
title_pca_glm <- "PCA + Global GLM"
param_pca_glm <- as.formula(paste("RainTomorrow", 
                                  paste(colnames(train_pca[-1]), collapse = "+"),
                                  sep = "~"))
print(param_pca_glm)

# We customise our glm function to use train_pca and test_pca
f_pca_glm <- function(p, mt){
  
  # fitting the glm model
  fit_g <- glm(formula = p,
               data = train_pca, 
               family = "binomial")
  
  print(mt)
  print(summary(fit_g))
  
  # computing predictions and accuracy
  pred_g <- predict(fit_g, newdata = test_pca,
                    type = "response")
  pred_g <- ifelse(pred_g > 0.5, "Yes", "No") %>%
    factor(levels = levels(weather_train$RainTomorrow))
  acc_g <- mean(pred_g == test_pca$RainTomorrow)
  
  # storing the summary in a data frame
  fit_sum <- summary(fit_g)$coefficients %>% data.frame() %>%
    rename(p_value = Pr...z..) %>%
    mutate(Variable = rownames(.))
  
  # plotting high p_values
  g_pval <- fit_sum %>%
    mutate(Variable = reorder(rownames(.), p_value)) %>%
    filter(p_value > 0.001) %>%
    ggplot(aes(Variable, p_value)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    geom_hline(yintercept = 0.05, color = c_red) +
    geom_text(aes(x = 1, y = 0.07, label = "5%"),
                  colour = "red", size = 3) +
    labs(title = "GLM model, p_values greater than 0.1%",
         subtitle = mt)
  
  # computing and plotting VIF
  t_vif <- VIF(fit_g) %>% as.data.frame() %>%
    rename(VIF = "GVIF^(1/(2*Df))") %>%
    mutate(Variable = rownames(.))
  g_vif <- t_vif %>% 
    mutate(Variable = reorder(Variable, VIF)) %>%
    filter(VIF > 1) %>%
    ggplot(aes(Variable,VIF)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    geom_hline(yintercept = 5, color = c_red) +
    labs(title = "GLM model - Variable Inflation Factor (VIF) > 1",
         subtitle = mt)
  
  # function output is a list
  return(list(Fit = fit_g, 
              Fit_Summary = fit_sum,
              Pred = pred_g, 
              Accuracy = acc_g,
              Graph_pval = g_pval, 
              Table_VIF = t_vif,
              Graph_VIF = g_vif))
}


# We run the GLM with the defined function
# Takes a few seconds 
pca_glm <- f_pca_glm(param_pca_glm, title_pca_glm)

# Accuracy is 84.8%, as expected close to the previously run GLM
# (slightly lower because we removed some predictors)
print(pca_glm$Accuracy)

# All variables are significant, only certain WindGust directions 
# and a few locations are not
print(pca_glm$Graph_pval)

# All variables have a VIF around 1, indicating perfect de-correlation
print(pca_glm$Graph_VIF)
# Comparing with the VIF plot of the glm with significant variables
# we see that VIFs, which were already good, are even closer to 1 with PCA
grid.arrange(glm_sign$Graph_VIF, pca_glm$Graph_VIF, ncol = 2)

# Computing ANOVA (takes around one minute)
t_anova_pca_glm <- data.frame(anova(pca_glm$Fit))
rownames(t_anova_pca_glm)[1] <- "Intercept"

# Plotting ANOVA
g_anova_pca_glm <- t_anova_pca_glm %>%
  mutate(Variable = rownames(.)) %>%
  filter(!Variable == "Intercept") %>%
  mutate(Variable = reorder(Variable, Deviance)) %>%
  ggplot(aes(Variable, Deviance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Analysis of Variance",
       subtitle = title_pca_glm)
# The ANOVA plot shows the most important variables at the top
# of the graph. PC2, RainToday, PC1, Location are the most significant.
# There are 3 PCs of less significance at the bottom of the graph.
print(g_anova_pca_glm)

# Storing key results
t_confM_pca_glm <- confusionMatrix(positive = "Yes", pca_glm$Pred, 
                                    reference = test_set$RainTomorrow )
t_res_pca_glm <- f_format_res(t_confM_pca_glm, title_pca_glm)

# The table shows that the global PCA-GLM model almost identical
# to the global GLM model as expected (slightly lower due to the
# removal of two PCs)
t_res8 <- bind_rows(t_res_glm_sign, t_res_pca_glm)

kable(t_res8, caption = "Model accuracy on test set",
      format = "simple",
      align = "c")

# We can visualise the nil correlation between PCs
g_cor_pc <- corrplot(cor(pca$x),
                       method = "circle")

# Plotting the relationship between PCs and RainTomorrow
g_rel_pc <- data.frame(RainTomorrow = train_set$RainTomorrow, pca$x) %>%
  pivot_longer(cols = 2:ncol(.), 
               names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(RainTomorrow, Value)) +
  geom_boxplot(na.rm = TRUE, color = "steelblue") +
  facet_wrap(Predictor ~ ., scales = "free")
# The plot shows that, apart from PC1 and PC2, most PCs do not have
# significantly differentiated ranges between RainTomorrow No and Yes
print(g_rel_pc)

# We can also do scatterplots for the 4 most significant PCs
g_pc_1_2 <- train_pca %>%
  ggplot(aes(PC1, PC2, color = RainTomorrow)) +
  geom_point() +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "PC2 vs PC1",
       subtitle = "Color = RainTomorrow")
g_pc_6_13 <- g_pc1_2 <- train_pca %>%
  ggplot(aes(PC6, PC13, color = RainTomorrow)) +
  geom_point() +
  scale_color_manual(values = c(c_red,c_blue),
                     labels = c("No", "Yes")) +
  labs(title = "PC13 vs PC6",
       subtitle = "Color = RainTomorrow")

# The first plot shows two areas between Rain and no Rain, 
# but with no definite frontier.
# The second plot shows that Rain & No Rain have significant overlaps
# This confirms the limitations than any model will face in
# predicting RainTomorrow

# Printing the plots takes a few seconds
print(g_pc_1_2)
print(g_pc_6_13)


# PCA + Local GLM----
# We now move to a local GLM, one per location

# We will keep all PCs for the local analysis
train_pca_loc <- bind_cols(
  train_set %>% select(RainTomorrow, Location, RainToday, WindGustDir),
  as.data.frame(pca$x))

test_pca_loc <- bind_cols(
  test_set %>% select(RainTomorrow, Location, RainToday, WindGustDir),
  t2)

# Defining the parameter
title_pca_glm_loc <- "PCA + Local GLM"
param_pca_glm_loc <- as.formula(paste("RainTomorrow", 
                paste(colnames(train_pca_loc[-c(1,2)]), collapse = "+"),
                  sep = "~"))
print(param_pca_glm_loc)

f_pca_glm_loc <- function(d){glm(formula = param_pca_glm_loc,
                         data = d, 
                         family = "binomial")}

# Creating one glm model per location in the training set
# Takes a few seconds to run
m_loc <- lapply(split(train_pca_loc, factor(train_set$Location)),
                FUN = f_pca_glm_loc)

# m_loc contains 49 models, one per location
length(m_loc)

# We split the test set into locations as well
test_pca_list_loc <- split(test_pca_loc, factor(test_set$Location))

# We can verify that the names of both lists are identical
# Which will allow us to use the Map function
identical(names(m_loc), names(test_pca_list_loc))

# We can now apply the models in m_loc to the test_set, 
# separately for each location. This is done through the Map function.
# We use the augment function in order to add the fitted data to the
# test set

temp_res <- Map(augment, m_loc,
                newdata = test_pca_list_loc,
                type.predict = "response")

# Converting the list back into a data frame
test_res_pg <- data.frame(Reduce(rbind, temp_res))
# Computing the predictions
test_res_pg <- test_res_pg %>%
  rename(fit_pca_glm_loc = .fitted)
test_res_pg <- test_res_pg %>%
  mutate(pred_pca_glm_loc = ifelse(fit_pca_glm_loc > 0.5, "Yes", "No") %>%
           factor(levels = levels(train_set$RainTomorrow)))

# Note that we can access the model for each location
# For instance for Canberra:
summary(m_loc[["Canberra"]])

# Overall accuracy is 85.8%
acc_pca_glm_loc <- mean(test_res_pg$pred_pca_glm_loc == test_res_pg$RainTomorrow)
print(acc_pca_glm_loc)

# Storing key results
t_confM_pca_glm_loc <- confusionMatrix(positive = "Yes", test_res_pg$pred_pca_glm_loc, test_res_pg$RainTomorrow)
t_res_pca_glm_loc <- f_format_res(t_confM_pca_glm_loc, title_pca_glm_loc)

# The below table shows that the accuracy is similar to the
# local GLM model, as expected
t_res9 <- bind_rows(t_res8, t_res_glm_loc, t_res_pca_glm_loc)
kable(t_res9, digits = 3, caption = "Model accuracy on test set",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))



# KNN model----


# We again start with a global model for parameter tuning
title_knn_glob <- "PCA + Global KNN"
# We will run knn on the PCA data with numeric parameters only
param_knn <- as.formula(paste("RainTomorrow", 
                                      paste(colnames(train_pca[-c(1:4)]), collapse = "+"),
                                      sep = "~"))
print(param_knn)

# Training the model to find the optimal k
# WARNING: TAKES TWO TO THREE MINUTES
# It is possible to skip this training

run_train_knn <- 0 # enter 1 to run

if(run_train_knn == 1) {

train_k <- seq(10, 30, 5)
train_knn <- function(kpar){
  fit <- knn3(formula = param_knn,
                  data = train_pca,
                  k = kpar)
  pred <- predict(fit, newdata = test_pca, type = "class")
  
  mean(pred == test_set$RainTomorrow)}

train_k_acc <- sapply(train_k, train_knn)

g_k <- data.frame(k = train_k, accuracy = train_k_acc) %>%
  ggplot(aes(k, accuracy)) +
  geom_point(color = "steelblue") +
  gghighlight(accuracy == max(accuracy),
              label_key = k,
              use_direct_label = TRUE,
              unhighlighted_params = list(color = "steelblue1")) +
  labs(title = "KNN Accuracy versus value of k",
       subtitle = "Optimal cutoff highlighted on graph")
print(g_k)

# The optimal k in the simulation is 15
print(train_k[which.max(train_k_acc)])
# The accuracy is 84.3%
acc_knn_glob <- max(train_k_acc)
print(acc_knn_glob)
} # End of If statement on training KNN

# Building a local model
title_knn_loc <- "PCA + Local KNN"

# We use all PCs for the local model
param_knn_loc <- as.formula(paste("RainTomorrow", 
                              paste(colnames(train_pca_loc[-c(1:4)]), collapse = "+"),
                              sep = "~"))
print(param_knn_loc)

chosen_k <- 15 # using the global best k (note that some simulations showed
# that changing k has little impact however)

f_knn <- function(d){knn3(formula = param_knn_loc,
                  data = d,
                  k = chosen_k)}

# We apply the function to each location separately to get separate KNN
# models per location
set.seed(1000, sample.kind = "Rounding")
m_loc <- lapply(split(train_pca_loc, factor(train_set$Location)),
                FUN = f_knn)
# We apply the models for each location
# We first check that the list are in the same location order
identical(names(m_loc), names(test_pca_list_loc))
temp_res <- Map(predict, m_loc, 
                newdata = test_pca_list_loc, type = "class")
# Adding the predictions to the test list
test_list_2 <- split(test_res_2, factor(test_res$Location))
test_list_3 <- Map(bind_cols, test_list_2, "pred_knn_loc" = temp_res)
# Also creating a data frame 
test_res_3 <- data.frame(Reduce(rbind, test_list_3))

# The accuracy is 84.2%
acc_knn_loc <- mean(test_res_3$pred_knn_loc == test_res_3$RainTomorrow)
print(acc_knn_loc)

# Storing key results
t_confM_knn_loc <- confusionMatrix(positive = "Yes", test_res_3$pred_knn_loc, test_res_3$RainTomorrow)
t_res_knn_loc <- f_format_res(t_confM_knn_loc, title_knn_loc)

# The table shows that KNN has slightly less accuracy
t_res10 <- bind_rows(t_res_pca_glm_loc, t_res_rf_loc, t_res_xg_glob, t_res_knn_loc)
kable(t_res10, caption = "Model accuracy on test set",
      format = "simple",
      align = "c")


# QDA----

# QDA works on numeric data. We will use the PCA data and the same
# parameters as KNN
title_qda_glob <- "PCA + Global QDA"
param_qda <- param_knn
print(param_qda)

# There is no parameter to be tuned
modelLookup("qda")

# Running the global model
# Takes around twenty seconds
set.seed(1000, sample.kind = "Rounding")
qda_glob <- train(form = param_qda,
                  data = train_pca,
                  method = "qda")

pred_qda_glob <- predict(qda_glob, newdata = test_pca)

# The accuracy is low at 82.3%
acc_qda_glob <- mean(pred_qda_glob == test_pca$RainTomorrow)
print(acc_qda_glob)

# Storing key results
t_confM_qda_glob <- confusionMatrix(positive = "Yes", pred_qda_glob, test_pca$RainTomorrow)
t_res_qda_glob <- f_format_res(t_confM_qda_glob, title_qda_glob)

# We now run a local model
title_qda_loc <- "PCA + Local QDA"
param_qda_loc <- param_knn_loc
print(param_qda_loc)

f_qda <- function(d){train(form = param_qda_loc,
                          data = d,
                          method = "qda")}

# We apply the function to each location separately to get separate
# models per location
# WARNING: Takes around 40 seconds
# Note that we can ignore the warnings in the Console, as they are
# purely informative about the sampling method used
m_loc <- lapply(split(train_pca_loc, factor(train_set$Location)),
                FUN = f_qda)
# We apply the models to the test_pca data split by location
# We first check that names are identical in both lists
identical(names(m_loc), names(test_pca_list_loc))
temp_res <- Map(predict, m_loc, 
                newdata = test_pca_list_loc, type = "raw")
# Adding the predictions to the test list
test_list_4 <- Map(bind_cols, test_list_3, "pred_qda_loc" = temp_res)
# Also creating a data frame 
test_res_4 <- data.frame(Reduce(rbind, test_list_4))

# The accuracy is 84.3%
acc_qda_loc <- mean(test_res_4$pred_qda_loc == test_res_4$RainTomorrow)
print(acc_qda_loc)

# Storing key results
t_confM_qda_loc <- confusionMatrix(positive = "Yes", test_res_4$pred_qda_loc, test_res_4$RainTomorrow)
t_res_qda_loc <- f_format_res(t_confM_qda_loc, title_qda_loc)


# Model results----

# The table shows that QDA has slightly less accuracy but better 
# Sensitivity
t_res11 <- bind_rows(t_res10, t_res_qda_loc)
kable(t_res11, caption = "Model accuracy on test set",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))


# Model comparisons----

# Comparative model performance by location
# Counting how many times each model has the best accuracy per location
s_loc1 <- test_res_4 %>%
  group_by(Location) %>%
  summarise(
    GLM = mean(pred_glm_loc == RainTomorrow),
    RF = mean(pred_rf_loc == RainTomorrow),
    XG = mean(pred_xg == RainTomorrow),
    KNN = mean(pred_knn_loc == RainTomorrow),
    QDA = mean(pred_qda_loc == RainTomorrow))

best1 <- data.frame(best_model = colnames(s_loc1)[1 + apply(s_loc1 %>% select(-Location), 1, which.max)])

g_best1 <- best1 %>%
  group_by(best_model) %>%
  summarise(count = n()) %>%
  mutate(best_model = reorder(best_model, count)) %>%
  ggplot(aes(best_model, count)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  labs(title = "Comparative model performance by location",
       subtitle = "Count = number of locations where the model had the best accuracy",
       caption = "XG Boost and GLM have the highest counts") +
  geom_text(aes(label=count), 
            position = position_dodge(width=0.9),
            vjust = 0.1)
# The plot confirms that XG and GLM are the most accurate models, not 
# only globally but per location too.
# This suggests to build an ensemble and see if performance can be 
# further improved
print(g_best1)


# Ensemble----

# We predict RainTomorrow based on the majority vote; we will make two 
# predictions, one with the 5 models, one with the best 3.
# For the best 3, we select the best two models and we add QDA which has
# the best Sensitivity (adding RF instead of QDA would improve accuracy
# slightly but deteriorate Sensitivity)

title_ens_5 <- "Ensemble 5"
title_ens_3 <- "Ensemble 3"

test_res_5 <- test_res_4 %>%
  mutate(test_5 = (pred_glm_loc == "Yes") + 
           (pred_rf_loc == "Yes") +
           (pred_xg == "Yes") +
           (pred_knn_loc == "Yes") +
           (pred_qda_loc == "Yes"),
         test_3 = (pred_glm_loc == "Yes") + 
           (pred_xg == "Yes") +
           (pred_qda_loc == "Yes"),
         pred_ens_5 = ifelse(test_5 > 2, "Yes", "No") %>%
           factor(levels = levels(weather_train$RainTomorrow)),
         pred_ens_3 = ifelse(test_3 > 1, "Yes", "No") %>%
           factor(levels = levels(weather_train$RainTomorrow)))

acc_ens_5 <- mean(test_res_5$pred_ens_5 == test_res_5$RainTomorrow) 
acc_ens_3 <- mean(test_res_5$pred_ens_3 == test_res_5$RainTomorrow) 
# The accuracy of the ensemble of 5 is 86.2%
# The accuracy of the ensemble of 3 is 86.3%
print(acc_ens_5)
print(acc_ens_3)

# Storing key results
t_confM_ens_5 <- confusionMatrix(positive = "Yes", test_res_5$pred_ens_5, test_res_5$RainTomorrow)
t_res_ens_5 <- f_format_res(t_confM_ens_5, title_ens_5)
t_confM_ens_3 <- confusionMatrix(positive = "Yes", test_res_5$pred_ens_3, test_res_5$RainTomorrow)
t_res_ens_3 <- f_format_res(t_confM_ens_3, title_ens_3)


# The table shows that the ensemble 3 has a the best results
t_res_12 <- bind_rows(t_res11, t_res_ens_5, t_res_ens_3)
kable(t_res_12, caption = "Model accuracy on test set",
      format = "simple",
      align = "c")


# General Summary----

# We collect the results from the various models
t_res13 <- bind_rows(t_res_norain,
                     t_res_Hum,
                     t_res_qda_glob,
                     t_res_glm_LocHum,
                     t_res_knn_loc,
                     t_res_qda_loc,
                     t_res_glm_sign,
                     t_res_xg_glob,
                     t_res_rf_loc,
                     t_res_ens_5,
                     t_res_ens_3,
                     t_res_glm_loc,
                     t_res_pca_glm_loc
                      )
# The table below shows all models studied, ranked by increasing
# accuracy and balanced accuracy:
temp <- t_res13 %>%
  mutate(across(c(2:5), ~parse_number(.x) / 100))
temp <- temp[order(
  temp$Accuracy,
  temp$Balanced_Acc),]
t_res14 <- temp %>%
  mutate(across(c(2:5), ~percent(.x, accuracy = 0.1)))
                      
kable(t_res14, digits = 3, caption = "Models ranked by Accuracy then Balanced Accuracy",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))


# Comparative model performance by location
# Counting how many times each model has the best accuracy per location
# We add the ensembles
s_loc2 <- test_res_5 %>%
  group_by(Location) %>%
  summarise(
    GLM = mean(pred_glm_loc == RainTomorrow),
    RF = mean(pred_rf_loc == RainTomorrow),
    XG = mean(pred_xg == RainTomorrow),
    KNN = mean(pred_knn_loc == RainTomorrow),
    QDA = mean(pred_qda_loc == RainTomorrow),
    ENS3 = mean(pred_ens_3 == RainTomorrow),
    ENS5 = mean(pred_ens_5 == RainTomorrow))

best2 <- data.frame(best_model = colnames(s_loc2)[1 + apply(s_loc2 %>% select(-Location), 1, which.max)])

g_best2 <- best2 %>%
  group_by(best_model) %>%
  summarise(count = n()) %>%
  mutate(best_model = reorder(best_model, count)) %>%
  ggplot(aes(best_model, count)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  labs(title = "Best models: location count",
       subtitle = "Number of locations where the model had the best accuracy",
       caption = "Ensemble of 3 and XG Boost have the highest counts") +
  geom_text(aes(label=count), 
             position = position_dodge(width=0.9),
             vjust = 0.1)

print(g_best2)

# Plotting the results by location
g_summary <- s_loc2 %>%
  pivot_longer(cols = 2:ncol(s_loc2), 
               names_to = "Model",
               values_to = "Accuracy") %>%
  mutate(Location = reorder(Location, Accuracy)) %>%
  ggplot(aes(Location, Accuracy, color = Model)) +
  geom_point() +
  labs(title = "Model accuracy per Location") +
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()

print(g_summary)

# In view of the table, we select as best approach
# the ensemble with three models: GLM, XG Boost and QDA.

# We will now apply the selected model on the validation set


# Validation----

# We retrain the model on the entire weather_train + weather_test sets
# We then apply the model to the separate validation set, which
# has never been used so far

# The validation set contains NA data which needs to be filled
# We populate NAs in the validation set with the default values 
# already computed [note that the validation data was not used to
# define the NA replacement values]
# We use left_join with the already computed NA_repl_l
validation2 <- validation %>%
  mutate(key = paste(Location, month(Date), sep = "|")) %>%
  pivot_longer(cols = where(is.numeric), names_to = "Predictor", values_to = "Value") %>%
  mutate(key = paste(key, Predictor, sep = "|")) %>%
  left_join(NA_Repl_l, by = "key") %>%
  mutate(Value = ifelse(is.na(Value), NA_Value, Value)) %>%
  select(-c("key", "NA_Value")) %>%
  pivot_wider(names_from = Predictor, values_from = Value)

# We now replace the categorical NA values
validation2 <- validation2 %>%
  mutate(key = paste(Location, month(Date), sep = "|")) %>%
  pivot_longer(cols = 3:7,
               names_to = "Predictor", values_to = "Value") %>%
  mutate(key = paste(key, Predictor, sep = "|")) %>%
  left_join(NA_Replf_l, by = "key") %>%
  mutate(Value = ifelse(is.na(Value), NA_Value, as.character(Value))) %>%
  select(-c("key", "NA_Value")) %>%
  pivot_wider(names_from = Predictor, values_from = Value) %>%
  mutate( WindGustDir = factor(WindGustDir),
          WindDir9am = factor(WindDir9am),
          WindDir3pm = factor(WindDir3pm),
          Location = factor(Location),
          RainToday = factor(RainToday),
          RainTomorrow = factor(RainTomorrow))


# Re-ordering the columns as per the original column order
col_order_new <- data.frame(Name = colnames(validation2), Rank_new = 1:ncol(validation2)) %>%
  left_join(col_order, by = "Name")
validation2 <- validation2[,order(col_order_new$Rank)]

# We can compare the number of NA values
sum(is.na(validation))
sum(is.na(validation2)) # nil

# Adding the calculated variables
validation2 <- validation2 %>% 
  mutate(VarHumidity = Humidity3pm - Humidity9am,
         VarTemp = Temp3pm - Temp9am,
         VarPressure = Pressure3pm - Pressure9am,
         VarWindSpeed = WindSpeed3pm - WindSpeed9am, 
         Year = year(Date),
         Month = month(Date))

# Defining train and test sets accordingly (the advantage of this step
# is the be able to re-use previous syntax)
train_set <- weather_clean # equivalent to weather_train + weather_test, 
# which corresponds to 80% of the initial data set weather_AUS
test_set <- validation2 # separate set, corresponds to 20% of the
# initial data set weather_AUS, with NAs now filled

# Retraining the chosen model on the new train set
# We start with pca
set.seed(1000, sample.kind = "Rounding")
pca <- prcomp(train_set[index_pca], scale. = TRUE)

# Out of 17 components, we need 15 components to cover at least
# 99% of the variance: similar results to the previous pca
t_pca <- t(summary(pca)$importance)

kable(t_pca, digits = 3, caption = "PCA Variance",
      format = "simple",
      format.args = list(decimal.mark = ".", big.mark = ","))

# Creating the train and test sets by adding the selected categorical
# factors to the PCA data (we also add Date for the purpose of having
# a unique identifier Location-Date to populate the ensemble)
train_pca_loc <- bind_cols(
  train_set %>% select(RainTomorrow, Location, RainToday, WindGustDir, Date),
  as.data.frame(pca$x))

t1 <- predict(pca, newdata = test_set[index_pca])
dim(t1)
t2 <- data.frame(t1)
test_pca_loc <- bind_cols(
  test_set %>% select(RainTomorrow, Location, RainToday, WindGustDir, Date),
  t2)

# We now run the local glm models

# Reminder of the parameter for the local GLM model
print(param_pca_glm_loc)

# Computing the models. Takes a few seconds to run
m_loc <- lapply(split(train_pca_loc, factor(train_set$Location)),
                FUN = f_pca_glm_loc)

# m_loc contains 49 models, one per location
length(m_loc)

# We split the test set into locations as well
test_pca_list_loc <- split(test_pca_loc, factor(test_set$Location))

# We can verify that the names of both lists are identical
# Which will allow us to use the Map function
identical(names(m_loc), names(test_pca_list_loc))

# We can now apply the models in m_loc to the test_set, 
# separately for each location. This is done through the Map function.
# We use the augment function in order to add the fitted data to the
# test set

temp_res <- Map(augment, m_loc,
                newdata = test_pca_list_loc,
                type.predict = "response")

# Converting the list back into a data frame
test_res_val <- data.frame(Reduce(rbind, temp_res))
# Computing the predictions
test_res_val <- test_res_val %>%
  rename(fit_pca_glm = .fitted)
test_res_val <- test_res_val %>%
  mutate(pred_pca_glm = ifelse(fit_pca_glm > 0.5, "Yes", "No") %>%
           factor(levels = levels(train_set$RainTomorrow)))

# Note that we can access the model for each location
# For instance for Canberra:
summary(m_loc[["Canberra"]])

# Overall GLM accuracy is 86% (for information only)
acc_pca_glm_val <- mean(test_res_val$pred_pca_glm == test_res_val$RainTomorrow)
print(acc_pca_glm_val)


# We now add the QDA predictions

# We apply the function to each location separately to get separate
# models per location
# WARNING: Takes around 40 seconds
# Note that we can ignore the warnings in the Console, as they are
# purely informative about the sampling method used
m_loc <- lapply(split(train_pca_loc, factor(train_set$Location)),
                FUN = f_qda)
# We will apply the models to the test data split by location
# We use test_res_val which includes the GLM predictions in order
# to build the ensemble
test_pca_list_loc <- split(test_res_val, factor(test_res_val$Location))
# We first check that names are identical in both lists
identical(names(m_loc), names(test_pca_list_loc))
# We apply the QDA models
temp_res <- Map(predict, m_loc, 
                newdata = test_pca_list_loc, type = "raw")
# Adding the predictions to the test list
test_list_val <- Map(bind_cols, test_pca_list_loc, "pred_qda_loc" = temp_res)
# Also creating a data frame 
test_res_val2 <- data.frame(Reduce(rbind, test_list_val))

# The QDA accuracy is 84% (for information only)
acc_qda_val <- mean(test_res_val2$pred_qda_loc == test_res_val2$RainTomorrow)
print(acc_qda_val)


# We now apply the XG Boost model

# Converting train and test set to matrices (required by xg)
# XG Boost works only with numerical data :
# For Location, we will use latitude and longitude
# For the other categorical data, we convert factors to numeric

train_set_temp <- train_set %>%
  left_join(locations, by = "Location") %>%
  select(-c("Location", "state", "population")) %>%
  mutate(across(where(is.factor), ~as.numeric(.x)))
test_set_temp <- test_set %>%
  left_join(locations, by = "Location") %>%
  select(-c("Location", "state", "population")) %>%
  mutate(across(where(is.factor), ~as.numeric(.x)))
index_xg <- sapply(train_set_temp, is.numeric) & !colnames(train_set_temp) == "RainTomorrow"

data_xg <- as.matrix(train_set_temp[index_xg])
label_xg <- ifelse(train_set_temp$RainTomorrow == 1, 0, 1)
test_xg <- as.matrix(test_set_temp[index_xg])

# Running the model
# Warning: takes a few seconds
# The parameters are defined based on the previous trials
xg_glob <- xgboost(data = data_xg,
                   label = label_xg,
                   max.depth = 6, #tree depth
                   booster = "gbtree", # gblinear",
                   eta = 0.5,
                   nthread = 2, #number of CPU threads
                   nrounds = 50, #number of passes on the data
                   eval.metric = "logloss", # = "error"
                   print_every_n = 10,
                   objective = "binary:logistic", #binary classification
                   verbose = 1)

# Computing the predictions
pred <- predict(xg_glob, test_xg)
prediction <- as.numeric(pred > 0.5)
pred_xg <- ifelse(prediction == 1, "Yes", "No") %>%
  factor(levels = levels(train_set$RainTomorrow))
# The accuracy is 85.7% (for information)
acc_xg_val <- mean(pred_xg == test_set$RainTomorrow)
print(acc_xg_val)

# Storing the predictions by Date and Location to populate the ensemble
pred_xg_temp <- data.frame(
  key = paste(test_set$Location, test_set$Date, sep = "|"),
  pred_xg = pred_xg)
test_res_temp <- test_res_val2 %>% 
  mutate(key = paste(test_res_val2$Location, test_res_val2$Date, sep = "|")) 
test_res_val3 <- test_res_temp %>%
  left_join(pred_xg_temp, by = "key") %>%
  select(-key)
# Checking that we are getting the same result in test_res
mean(test_res_val3$pred_xg == test_res_val3$RainTomorrow)

rm(pred_xg_temp)
rm(test_res_temp)
rm(train_set_temp)
rm(test_set_temp)


# Computing the ensemble

test_res_val4 <- test_res_val3 %>%
  mutate(test_3 = (pred_pca_glm == "Yes") + 
           (pred_xg == "Yes") +
           (pred_qda_loc == "Yes"),
         pred_ens_3 = ifelse(test_3 > 1, "Yes", "No") %>%
           factor(levels = levels(weather_train$RainTomorrow)))

# The accuracy is 86.2%
acc_val <- mean(test_res_val4$pred_ens_3 == test_res_val4$RainTomorrow)
print(acc_val)

# Storing key results
title_val <- "Validation set"
t_confM_val <- confusionMatrix(positive = "Yes", test_res_val4$pred_ens_3, test_res_val4$RainTomorrow)
t_res_val <- f_format_res(t_confM_val, title_val)

#Global Results----
# We obtain an accuracy of 86.2% and a balanced accuracy of 76.3%
# Sensitivity is 58.2% (accuracy of predicting rain)
# Specificity is 94.4% (accuracy of predicting no-rain)
kable(t_res_val, caption = "Model accuracy on Validation set",
      format = "simple",
      align = "c")

# We can also view the predictive values
t_res_predval = data.frame(
  Rain_Accuracy = t_confM_val$byClass["Pos Pred Value"],
  No_Rain_Accuracy = t_confM_val$byClass["Neg Pred Value"]) %>%
  mutate(across(everything(), ~ scales::percent(.x, accuracy = 0.1)))
rownames(t_res_predval)[1] <- "Validation set"

# The accuracy of predicting Rain is 75.1% (precision, positive predicting value)
# The accuracy of predicting No-Rain is 88.6%
kable(t_res_predval, caption = "Predictive values",
      format = "simple",
      align = "c")

# Local results----

# Plotting the locations on a map with their Accuracy
# Computing Accuracy per location
t_val_loc <- test_res_val4 %>%
  group_by(Location) %>%
  summarise(Accuracy = mean(pred_ens_3 == RainTomorrow)) %>%
  left_join(locations, by = "Location")
# Defining variables used in the plot
m_data <- t_val_loc
m_color <- sym("Accuracy")
m_color_begin <- 0 # used for color scale
m_color_end <- 1 - min(m_data$Accuracy) # used for color scale
m_title <- "GLM accuracy (Validation set)"
m_subtitle <- "Color of points showing accuracy levels"
m_caption <- paste("Accuracy range", 
                     paste(percent(range(m_data$Accuracy), accuracy = 0.1),
                           collapse = " - "),
                     sep = " : ") 
# Plotting the map
# The map shows the accuracy is lower in coastal locations
# These are areas where we expect the weather to change frequently,
# due to the influence of the ocean.
map_val_loc <- f_map(m_data, m_color, m_color_begin, m_color_end, 
                     m_title, m_subtitle, m_caption)
print(map_val_loc)


# Plotting the locations on a map with their Sensitivity
t_val_sens <- test_res_val4 %>%
  filter(RainTomorrow == "Yes") %>%
  group_by(Location) %>%
  summarise(Sensitivity = mean(pred_ens_3 == RainTomorrow)) %>%
  left_join(locations, by = "Location")
# Defining variables used in the plot
m_data <- t_val_sens
m_color_begin <- 0 # used for color scale
m_color <- sym("Sensitivity")
m_color_end <- 1 - min(m_data$Sensitivity) # used for color scale
m_title <- "Sensitivity (Validation set)"
m_subtitle <- "Color of points showing sensitivity levels"
m_caption <- paste("Sensitivity range", 
                   paste(percent(range(m_data$Sensitivity), accuracy = 0.1),
                         collapse = " - "),
                   sep = " : ") 

# The map shows the sensitivity is very weak (<50%) in a few locations only
map_val_sens <- f_map(m_data, m_color, m_color_begin, m_color_end, 
                      m_title, m_subtitle, m_caption)
print(map_val_sens)


# Plotting the locations on a map with their Specificity
t_val_spec <- test_res_val4 %>%
  filter(RainTomorrow == "No") %>%
  group_by(Location) %>%
  summarise(Specificity = mean(pred_ens_3 == RainTomorrow)) %>%
  left_join(locations, by = "Location")
# Defining variables used in the plot
m_data <- t_val_spec
m_color_begin <- 0 # used for color scale
m_color <- sym("Specificity")
m_color_end <- 1 - min(m_data$Specificity) # used for color scale
m_title <- "Specificity (Validation set)"
m_subtitle <- "Color of points showing specificity levels"
m_caption <- paste("Specificity range", 
                   paste(percent(range(m_data$Specificity), accuracy = 0.1),
                         collapse = " - "),
                   sep = " : ") 

# The map shows strong specificity across virtually all locations
map_val_spec <- f_map(m_data, m_color, m_color_begin, m_color_end, 
                      m_title, m_subtitle, m_caption)
print(map_val_spec)


# Plotting Accuracy versus No-Rain mean
t_val_NR <- test_res_val4 %>%
  group_by(Location) %>%
  summarise(prop_NoRain = mean(RainToday == "No")) # computing No-Rain
# prevalence using RainToday

g_val_NR <- t_val_loc %>%
  inner_join(t_val_NR, by = "Location") %>%
  ggplot(aes(prop_NoRain, Accuracy)) +
  geom_point(color = "darkviolet") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "PCA GLM (Validation set)",
       subtitle = "Location Accuracy versus Proportion of No Rain Tomorrow") +
  geom_smooth(se = FALSE)
# The plot shows some link between No-Rain prevalence and accuracy
print(g_val_NR)


# Plotting the accuracy per location on the Validation set
g_val_loc <- t_val_loc %>%
  mutate(Location = reorder(Location, Accuracy)) %>%
  ggplot(aes(Location, Accuracy)) +
  geom_point(color = "steelblue") +
  labs(title = "Accuracy per location",
       subtitle = "Validation Set") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.775, 0.975, 0.025)) + 
  geom_hline(yintercept = acc_val, lty = 3, color = "steelblue") +
  geom_text(aes(x = "Cobar", y = acc_pca_glm_val),
            label = paste("Mean accuracy", 
                          percent(acc_val, accuracy = 0.1)),
            color = "steelblue",
            size = 3) +
  theme(axis.text.y = element_text(size = g_loc_size)) +
  coord_flip()
# 46 locations have an accuracy above 80%
# 3 locations have an accuracy below 80%
print(g_val_loc)


# END OF SCRIPT----



