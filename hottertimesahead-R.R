# Load the required libraries
library(dplyr)
library("ggplot2")
library(lubridate)
library("ggpubr")
library(scales) # Load the scales package for formatting labels

data= read.csv("C:/Users/varsh/OneDrive/Desktop/hottertimesahead-R-main/Indian Summers - Over the years-2007-11.csv")

#1.to display all data
head(data, 10)

#2.to find if there is null
is.na(data)



library(dplyr)
# 3.Calculate the mean of 'feelslikemax' for each 'City', sort in descending order, and reset the index
city_feels_like <- data %>%
  group_by(City) %>%
  summarize(feelslikemax_mean = mean(feelslikemax, na.rm = TRUE)) %>%
  arrange(desc(feelslikemax_mean)) %>%
  ungroup()

# 4.Calculate the mean of 'tempmax' for each 'City', sort in descending order, and reset the index
city_max <- data %>%
  group_by(City) %>%
  summarize(tempmax_mean = mean(tempmax, na.rm = TRUE)) %>%
  arrange(desc(tempmax_mean)) %>%
  ungroup()
print(city_feels_like)
print(city_max)



#5.Plot the bar graph

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

#Anusha

#1.checking the highest temp max city with date and year

library(dplyr)

result <- data %>%
  group_by(City, Date) %>%
  summarise(mean_tempmax = mean(tempmax)) %>%
  arrange(desc(mean_tempmax)) %>%
  head(10)

# To reset the row names
result <- as.data.frame(result)
rownames(result) <- NULL

# Print the result
print(result)

#2.to find number of null values
colSums(is.na(data))


#3.graph
library(dplyr)
library(ggplot2)

data$other_temp <- data$feelslikemax - data$tempmax
data$temp_gap <- data$tempmax - data$tempmin

humid_city <- data %>%
  group_by(City) %>%
  summarise(mean_other_temp = mean(other_temp)) %>%
  arrange(desc(mean_other_temp))

ggplot(humid_city, aes(x = "", y = mean_other_temp, fill = City)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Mean Other Temperature by City (Pie Chart)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())



#explaination
#other_temp is calculated by subtracting the 'tempmax' column from the 'feelslikemax' column. It represents the difference between the actual maximum temperature ('tempmax') 
#and the temperature that it feels like ('feelslikemax').
#temp_gap is calculated by subtracting 'tempmin' from 'tempmax' and represents the temperature difference between the maximum and minimum temperatures for each record.



#-----------------------------------------------------------------------------------------------------------------------

#devanmita

# Load the required libraries
library(dplyr)
library(ggplot2)

# Calculate the mean of 'windspeed' for each 'City', sort in descending order, and reset the index
city_windspeed <- data %>%
  group_by(City) %>%
  summarize(windspeed_mean = mean(windspeed, na.rm = TRUE)) %>%
  arrange(desc(windspeed_mean)) %>%
  ungroup()

# Print the resulting data frame
print(city_windspeed)



# Create a line graph for the mean wind speed by city
ggplot(city_windspeed, aes(x = City, y = windspeed_mean, group = 1)) +
  geom_line(color = "skyblue") +
  geom_point(color = "skyblue") +
  labs(x = "City", y = "Mean Wind Speed", title = "Mean Wind Speed by City (Line Graph)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
