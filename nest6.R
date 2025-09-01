### Sex ratio model and temperature data

#load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

## Nest 6 ##
nest <- read.csv("C:/Users/olivi/Dropbox/Unibe/Thesis/Data/Logger Data/Nests Ticino/6/nest6.csv", header = TRUE)
head(nest)
str(nest)

##Clean data
# Parse with multiple possible formats
nest$parsed <- parse_date_time(nest$Date.Time,
                             orders = c("mdy HMS p", "mdy HM", "mdy HMS", "mdy HMp"))

# Create Date and Time columns
nest <- nest %>%
  mutate(
    Date = as.Date(parsed),
    Time = format(parsed, "%H:%M:%S")
  ) %>%
  select(-parsed)
print(nest)

# Filter data for buried time
nest$datetime <- as.POSIXct(paste(nest$Date, nest$Time))

filtered <- nest %>%
  filter(between(datetime,
                 as.POSIXct("2025-06-20 13:26:01"),
                 as.POSIXct("2025-08-14 07:26:01")))

## Plot
#Make columns for temperature and moisture
temp_moi <- filtered %>%
  pivot_wider(
    names_from = Unit,
    values_from = Value
  ) %>%
  rename(
    Temperature = C,
    Mositure = `%RH`
  )

# Sepparate bottom and top logger values for plot
top <- temp_moi %>%
  filter(Type == "t")
bottom <- temp_moi %>%
  filter(Type == "h")

## Only TSP plot
# Estimate TSP
tsp <- top %>%
  summarize(mid = min(datetime) + (max(datetime) - min(datetime)) / 2) %>%
  pull(mid)

top_tsp <- top %>% filter(datetime >= tsp)
bottom_tsp <- bottom %>% filter(datetime >= tsp)

ggplot() +
  geom_line(data = top_tsp, aes(x = datetime, y = Temperature, color = "Top"), size = 1, alpha = 0.7) +
  geom_line(data = bottom_tsp, aes(x = datetime, y = Temperature, color = "Bottom"), size = 1, alpha = 0.7) +
  labs(title = "Temperature Curve during TSP in a Real Nest in Ticino",
       x = "Date",
       y = "Temperature (°C)",
       color = "Logger Position") +
  scale_color_manual(values = c("Top" = "orange", "Bottom" = "darkred")) +
  theme_minimal() +
  theme(legend.position = "top",
          plot.title = element_text(size = 18, face = "bold"),        
          axis.title.x = element_text(size = 14),     
          axis.title.y = element_text(size = 14),     
          axis.text.x = element_text(size = 12),                       
          axis.text.y = element_text(size = 12),                      
          legend.title = element_text(size = 13),                   
          legend.text = element_text(size = 12))                      


## Full plot
# Scatterplot
 ggplot() +
   geom_line(data = top, aes(x = datetime, y = Temperature, color = "Top"), size = 0.7, alpha = 0.7) +
   geom_line(data = bottom, aes(x = datetime, y = Temperature, color = "Bottom"), size = 0.7, alpha = 0.7) +
   labs(title = "Temperature Curve during TSP in Real Nest from Ticino",
        x = "Date",
        y = "Temperature (°C)",
        color = "Logger Position") +
   scale_color_manual(values = c("Top" = "orange", "Bottom" = "darkred")) +
   theme(legend.position = "top") +
   theme_minimal()
 
 # Mositure plot
 ggplot() +
   geom_line(data = bottom, aes(x = datetime, y = Mositure, color = "Bottom"), size = 0.7, alpha = 0.7) +
   labs(title = "Moisture Curve during TSP in Real Nest from Ticino",
        x = "Date",
        y = "Moisture (%RH)",
        color = "Logger Position") +
   scale_color_manual(values = c("Bottom" = "darkblue")) +
   theme_minimal() +
   theme(legend.position = "top")
 
 
 
 
