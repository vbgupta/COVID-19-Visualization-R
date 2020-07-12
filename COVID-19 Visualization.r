# packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(ggplot2)
library(dplyr)
library(readr)

# dataset
data <- read.csv("coronavirus.csv", header = TRUE)
#data

date_cases <- data %>% group_by(date, country) %>% summarise(total = sum(cases))
date_cases

#trying line plot to show total cases as the dates progressed
plot1 <- ggplot(date_cases, aes(x = date, y = total, group = 1)) + geom_line(color = "blue", size = 1.2) +
ylab("Cumulative Confirmed Cases") + theme(axis.text.x = element_text(angle =90)) +
#+ xlim(0,30)
geom_smooth(method = "auto", se = FALSE, color = "red")
plot1

china_cases <- data %>% filter(country == "China") %>% 
group_by(country, date) %>% summarise(total_china = sum(cases)) 
#china_cases

#lineplot for china cases
chinaline <- ggplot(china_cases, aes(x = date, y = total_china, group = 1)) + 
geom_line(color = "blue", size = 1.5) +
ylab("China Cases Trend") + theme(axis.text.x = element_text(angle = 90)) +
geom_smooth(method = "auto", se = FALSE, color = "red")
chinaline

us_cases <- data %>% filter(country == "US") %>% group_by(country, date) %>%
summarise(total_us = sum(cases))
#us_cases

#us line plot
us_line <- ggplot(us_cases, aes(x = date, y = total_us, group = 1)) +
geom_line(color = "red", size = 1.5) + 
theme(axis.text.x = element_text(angle = 90)) +
ylab("US cases over time")
us_line

# ploting china cases vs us
#create data frame for both china and us

china_us_df <- left_join(us_cases, china_cases, by = c("date" = "date"))
#china_us_df

china_us_plot <- ggplot(china_us_df) +
geom_line(aes(x = date, y = total_us, group = 1), color = "red") +
geom_line(aes(x = date, y = total_china, group = 1), color = "darkblue") +
ylab("Cases and country") + xlab("Date") + 
theme(axis.text.x = element_text(angle = 90))
china_us_plot

china_us_area <- ggplot(china_us_df) +
geom_area(aes(x = date, y = total_us, group = 1, fill = "US")) +
geom_area(aes(x = date, y = total_china, group = 1, fill = "China")) +
ylab("Cases and country") + xlab("Date") + 
theme(axis.text.x = element_text(angle = 90), 
     axis.line = element_line(size = 1, color = "black"),
     #panel.grid.major = element_line(color = "black"),
     #panel.grid.minor = element_line(color = "black"),
     #panel.background = element_blank()) +
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.title = element_blank())
ggtitle("China vs US Cases")
china_us_area


