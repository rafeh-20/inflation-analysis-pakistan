library(ggplot2)
library(dplyr)
library(moments)

findMode <- function(x){ 
  ta <- table(x)
  tam <- max(ta)
  if (all(ta == tam))
    mod <- NA
  else if(is.numeric(x))
    mod <- as.numeric(names(ta)[ta == tam])
  else
    mod <- names(ta)[ta == tam]
  return(mod)
}

# Read File
data <- read.csv(file="ExchangeRate.csv")

data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Line Graph
ggplot(data, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Exchange Rate") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))



data$Close <- as.numeric(data$Close)

# Calculate descriptive statistics
descriptive_stats <- data.frame(
  N = length(data$Close),
  Mean = round(mean(data$Close), 2),
  Median = round(median(data$Close), 2),
  Mode = round(findMode(data$Close), 2),
  Min = round(min(data$Close), 2),
  Max = round(max(data$Close), 2),
  LowerQ = round(quantile(data$Close, 0.25), 2),
  UpperQ = round(quantile(data$Close, 0.75), 2),
  SD = round(sd(data$Close), 2),
  Variance = round(var(data$Close), 2),
  Kurtosis = round(kurtosis(data$Close), 2),
  Skewness = round(skewness(data$Close), 3)
)

print(descriptive_stats)


# Frequency Distribution Table

class_intervals <- seq(180, 300, by = 10)

frequency_table <- table(cut(data$Close, breaks = class_intervals))

cumulative_frequency <- cumsum(frequency_table)

frequency_table <- data.frame(
  Class_Interval = names(frequency_table),
  Frequency = as.numeric(frequency_table),
  Cumulative_Frequency = cumulative_frequency
)

print(frequency_table)

# Frequency Distribution Graph

ggplot(frequency_table, aes(x = Class_Interval, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Exchange Rate", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

