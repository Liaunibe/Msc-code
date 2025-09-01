### Msc Thesis Olivia###
### 'Nesting ecology of the incasive pond turtle Trachemys scripta' ###
### Code to analyse nest site variables and sex ratio ###

library(ggplot2)
library(dplyr)
library(lubridate)

## Nest Variables ##
sites <- read.csv("C:/Users/olivi/Dropbox/Unibe/Thesis/Data/Nest Site Variables.csv", header = TRUE)
head(sites)
str(sites)

## Clean
sites <- sites %>%
  filter(type != 'abondened')

# convert everything into the right ... category?
sites$date <- dmy(sites$date)
sites$time <- as.numeric(sites$time)
sites$site <- as.factor(sites$site)
sites$type <- as.factor(trimws(sites$type))
sites$weather <- ifelse(sites$weather == "", NA, sites$weather)
sites$weather <- as.factor(sites$weather)
sites$veg_scrub <- as.numeric(sites$veg_scrub)
sites$duration <- as.numeric(sites$duration)
sites$soil_surface_temperature <- as.numeric(sites$soil_surface_temperature)
sites$moisture <- as.numeric(sites$moisture)  
sites$pH <- as.numeric(sites$pH)
sites$incline <- as.numeric(sites$incline)
sites$canope_cover <- as.numeric(sites$canope_cover)

str(sites)

# Calculate total vegetation cover to see if there is no mistake
veg_total <- rowSums(sites[,c("veg_soil","veg_grass","veg_herb","veg_org", 'veg_scrub')])
list(veg_total)

## EDA
summary(sites)

#compare chosen and control groups

chosen <- sites %>% filter(type %in% c("real", "predated"))
control <- sites %>% filter(type == "control")

chosen$group <- "chosen"
control$group <- "control"
combined <- rbind(chosen, control)

variables <- c("incline", "veg_soil", "veg_grass", "veg_herb", "veg_org", "canope_cover")

# Loop through numeric variables
for (col in variables) {
  p <- ggplot(combined, aes_string(x = col, fill = "group")) +
    geom_histogram(alpha = 0.5, position = 'dodge') +
    scale_fill_manual(values = c("chosen" = "skyblue", "control" = "darkgreen")) +
    theme_minimal() +
    ggtitle(paste("Histogram of", col, "by Group")) +
    xlab(col) 
  print(p)
}


