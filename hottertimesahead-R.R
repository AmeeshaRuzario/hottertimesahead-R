# Load the required libraries
library(dplyr)
library("ggplot2")
library(lubridate)
library("ggpubr")
library(scales) # Load the scales package for formatting labels

data= read.csv("C:/Users/varsh/OneDrive/Desktop/Indian Summers - Over the years-2007-11.csv")

#1.to display all data
head(data, 10)

#2.to find if there is null
is.na(data)

#3.to find number of null values
colSums(is.na(data))





library(dplyr)
# 4.Calculate the mean of 'feelslikemax' for each 'City', sort in descending order, and reset the index
city_feels_like <- data %>%
  group_by(City) %>%
  summarize(feelslikemax_mean = mean(feelslikemax, na.rm = TRUE)) %>%
  arrange(desc(feelslikemax_mean)) %>%
  ungroup()

# 5.Calculate the mean of 'tempmax' for each 'City', sort in descending order, and reset the index
city_max <- data %>%
  group_by(City) %>%
  summarize(tempmax_mean = mean(tempmax, na.rm = TRUE)) %>%
  arrange(desc(tempmax_mean)) %>%
  ungroup()
print(city_feels_like)
print(city_max)



#6.Plot the bar graph

# Load the ggplot2 package
library(ggplot2)


# Calculate the mean of 'tempmax' for each city
city_max <- data %>%
  group_by(City) %>%
  summarize(tempmax_mean = mean(tempmax, na.rm = TRUE)) %>%
  arrange(desc(tempmax_mean)) %>%
  ungroup()

# Create the bar plot with a built-in color palette
ggplot(city_max, aes(x = reorder(City, tempmax_mean), y = tempmax_mean, fill = City)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # Using the viridis color palette
  labs(title = "Cities with the highest mean measured temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#-------------------------------------------------------------------------------------------------------------------------


