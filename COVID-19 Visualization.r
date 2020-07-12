# packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(ggplot2)
library(dplyr)
library(readr)

# dataset
data <- read.csv("coronavirus.csv", header = TRUE)
data

date_cases <- data %>% group_by(date) %>% summarise(total = sum(cases))
date_cases

#trying line plot to show total cases as the dates progressed
plot1 <- ggplot(date_cases, aes(x = date, y = total, group = 1)) + geom_line(color = "blue") +
ylab("Cumulative Confirmed Cases") + theme(axis.text.x = element_text(angle =90)) +
#+ xlim(0,30)
geom_smooth(method = "auto", se = FALSE, color = "red")
plot1

china_cases <- data %>% filter(country == "China") %>% 
group_by(country, date) %>% summarise(total_china = sum(cases)) 
china_cases

#lineplot for china cases
chinaline <- ggplot(china_cases, aes(x = date, y = total_china, group = 1)) + 
geom_line(color = "blue") +
ylab("China Cases Trend") + theme(axis.text.x = element_text(angle = 90)) +
geom_smooth(method = "auto", se = FALSE, color = "red")
chinaline

us_cases <- data %>% filter(country == "US") %>% group_by(country, date) %>%
summarise(total_us = sum(cases))
us_cases

#us line plot
us_line <- ggplot(us_cases, aes(x = date, y = total_us, group = 1)) +
geom_line(color = "blue") + theme(axis.text.x = element_text(angle = 90)) +
ylab("US cases over time")
us_line

# ploting china cases vs us
#create data frame for both china and us

china_us_df <- left_join(us_cases, china_cases, by = c("date" = "date"))
china_us_df

china_us_plot <- ggplot(china_us_df) +
geom_line(aes(x = date, y = total_us, group = 1), color = "red") +
geom_line(aes(x = date, y = total_china, group = 1), color = "darkblue") +
ylab("Cases and country") + xlab("Date") + 
theme(axis.text.x = element_text(angle = 90)) 
china_us_plot


