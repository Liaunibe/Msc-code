### Msc Thesis Olivia###
### 'Nesting ecology of the invasive pond turtle Trachemys scripta' ###
### Code to analyse nest site variables ###

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
sites$site <- as.factor(sites$site)
sites$type <- as.factor(trimws(sites$type))
sites$duration <- as.numeric(sites$duration)
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

#Compare chosen and control groups
chosen <- sites %>% filter(type %in% c("real", "predated"))
control <- sites %>% filter(type == "control")

chosen$group <- "chosen"
control$group <- "control"
combined <- rbind(chosen, control)

variables <- c("incline", "veg_soil", "veg_grass", "veg_herb", "veg_org", "canope_cover")

# Histogramms to compare distributions
for (col in variables) {
  p <- ggplot(combined, aes_string(x = col, fill = "group")) +
    geom_histogram(alpha = 0.5, position = 'dodge') +
    scale_fill_manual(values = c("chosen" = "skyblue", "control" = "darkgreen")) +
    theme_minimal() +
    ggtitle(paste("Histogram of", col, "by Group")) +
    xlab(col) 
  print(p)
}

#GLM
# Make a binary response variable
combined$chosen <- ifelse(combined$type %in% c("real", "predated"), 1, 0)

incline <- glm(chosen ~ incline,
               data = combined, family = binomial)
summary(incline)

veg <- glm(chosen ~ veg_soil + veg_grass + veg_herb + veg_org,
                data = combined, family = binomial)
summary(veg_soil)

canope <- glm(chosen ~ canope_cover,
              data = combined, family = binomial)
summary(canope)

complete <- glm(chosen ~ incline + veg_soil + veg_grass + veg_herb + veg_org + canope_cover,
                data = combined, family = binomial)
summary(complete)

library(car)
vif(complete)
