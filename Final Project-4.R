#Name   : Joel Thomas Zachariah
#Date   : 12/09/2024
#Class  : ALY6010(Probability and Statistics)
#Dataset : Maryland State Patrol

#
#Clean canvas ----
#clears global environment
rm(list=ls())
#clears the plots
if (!is.null(dev.list())) dev.off() 
try(p_unload(p_loaded(),character.only = TRUE), silent = TRUE) #clears loaded packages
options(scipen = 100) #disable scientific notation for R session
#clears console
cat("\014")

#Packages ----
library(pacman)
library(tidyverse)
library(readr)

#Set Working Directory ----
setwd("C:/Users/joelt/OneDrive/Documents/ALY 6010/Project")

#Choosing file and storing in a variable ----
filename <- file.choose()
data <- readRDS(filename)


#Choosing columns ----
data <- data[,c("date","location","subject_age","subject_race","subject_sex","type","reason_for_stop",
                "reason_for_search","reason_for_arrest","search_conducted","contraband_found","outcome")]

#Correcting Datatypes ----
data$reason_for_stop <- factor(data$reason_for_stop)
data$date <- as.Date(data$date)

data <- data %>% mutate(dayofweek = wday(date, label = TRUE)) #to give week according to the day

#Frequency table of stops by race ----
table(data$subject_race)
race <- data %>% group_by(subject_race) %>% summarize(count = n())

#Cross-tabulation of race and search conducted ----
table(data$subject_race, data$search_conducted)

#remove Other, Unknown and NA from table ----
data <- data %>% filter(subject_race != 'unknown', subject_race != 'other', subject_race != 'NA',dayofweek != 'NA', subject_age != 'NA')

#More advance cross-tabulation ----
#install.packages('gmodels')
library(gmodels)
CrossTable(data$subject_race, data$search_conducted, prop.chisq = FALSE)

library(ggplot2)

#Histogram of stops by day of week ----
png("Distribution of Traffic Stops.png", width = 800, height = 600)
ggplot(data, aes(x=dayofweek)) +
  geom_bar(fill = "green", color = "black") +
  labs(
    x = "Day of Week",
    y = "Frequency",
    title = "Distribution of Traffic Stops"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1)
  )
dev.off()

#Bar plot of stops by race ----
png("Traffic Stops by Race.png", width = 800, height = 600)
ggplot(data, aes(x = subject_race))+
  geom_bar()+
  labs(
    x = "Race",
    y = "Count",
    title = "Traffic Stops by Race"
  ) +
  theme(
    axis.line = element_line(color = "black", size = 1)
  )+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
dev.off()

#Calculate the proportion of searches for each race ----
search_propotions <- data %>% group_by(subject_race) %>% 
  summarise(search_rate = mean(search_conducted, na.rm = TRUE))

#install formattable package to plot percentage ----
#install.packages('formattable')
library(formattable)

png("Search_Propotion.png", width = 800, height = 600)
ggplot(search_propotions, aes(x = subject_race, y =search_rate)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = percent(search_rate)), vjust=-0.3, size=3.5)+
  labs(
    x = "Race",
    y = "Proportion of Stops Resulting in Search",
    title = "Search rates by race"
  )+
  theme(
    axis.line = element_line(color = "black", size = 1)
  )+
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_y_continuous(limits = c(0,1),labels = scales::percent)
dev.off()

# Faceted histogram using ggplot2 to compare across multiple classes
png("Stop_time_by_race.png", width = 800, height = 600)
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~ subject_race) +
  labs(title = "Distribution of Stop Times by Race",
       x = "Time of Day",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#Load Libraries ----
#install.packages('psych')
library(dplyr)
library(ggplot2)
library(psych)

#Clean up the age column
age <- data%>% group_by(subject_age) %>% summarize(counts = n())

data <- data %>% filter(subject_age != 'NA') #Remove NA

data <- data %>% filter(subject_age > 16) #remove age less than legal driving

#Descriptive Statistics Table ----
## For the entire Sample
overall_stats <- t(psych::describe(data$subject_age))

## BY group ('subject race')
group_stats <- data %>% group_by(subject_race) %>% summarise(
  mean_age = mean(subject_age, na.rm = TRUE),
  sd_age = sd(subject_age, na.rm = TRUE),
  min_age = min(subject_age, na.rm = TRUE),
  max_age = max(subject_age, na.rm = TRUE),
  N = n()
)

#Print statistics ina three-line table format ----
print(knitr::kable(group_stats,format = 'pipe'))

#2.Visualization

#Reduce table to new outcome table with only the two columns needed ----
data_outcome <- data[, c("outcome", "subject_age")] %>%
  filter(outcome != 'NA')

#Scatter Plot of Age vs Outcome 
png("Age vs Outcome.png",width = 800,height = 600)
ggplot(data_outcome,aes(x=subject_age,y=outcome)) +
  geom_jitter(width = 0.3,alpha = 0.5)+
  labs(title = "Age vs Outcome",
       x = "Age", y = "Outcome")
dev.off()

#Review the outcome frequency chart ----
outcome <- data %>% group_by(outcome) %>% summarize(count = n())

#Jitter Plot for search conducted by race ----
png("Search conducted by Race.png", width = 800, height = 600)
ggplot(data,aes(x = subject_race, y = search_conducted)) +
  geom_jitter(Width = 0.3, alpha = 0.5) +
  labs(title = "Search conducted by Race",
       x = "Race",
       y = "Search Conducted")
dev.off()

#Box Plot of age by race to detect outliers ----
data_age <- data[, c("subject_age", "subject_race")]
data_age$subject_race <- as.character(data_age$subject_race)
png("Age Distirbution by Race.png", width = 800, height = 600)
boxplot(subject_age ~ subject_race, data = data_age,
        main = "Age Distirbution by Race",
        xlab = "Race", ylab = "Age")
dev.off()

# Hypothesis Test -----
library(dplyr)

# Conduct a one-sample t-test for mean age

# Perform one-sample t-test
t_test_age <- t.test(data$subject_age, mu =  35 , alternative = "two.sided")

#t.test(data$subject_age, mu = 35, alternative = "greater") tests if mean age is greater than 35
#t.test(data$subject_age, mu = 35, alternative = "less") tests if mean age is less than 35

# Print the results of the t-test
print(t_test_age)

# Check the p-value
if(t_test_age$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis. 
      There is evidence to suggest that the mean age of individuals stopped is significantly different from 35 years.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis. 
      There is no significant evidence to suggest that the mean age differs from 35 years.\n")
}




# Create a contingency table for the observed frequencies
# Use frequency from race table:
#race <- data |> group_by(subject_race) |> summarize(counts = n())
#     Black counts (Searched 8323 Not Searched 333282) | White counts (Searched 34008 Not Searched 2376651)
observed <- matrix(c(81621, 1291, 212977, 8704), nrow = 2, byrow = TRUE,
                   dimnames = list(Race = c("asian/pacific islander", "hispanic"),
                                   Search_Conducted = c("Yes", "No")))

# Print the observed contingency table
print(observed)

# Perform the Chi-square test for independence
chi_test <- chisq.test(observed)

# Print the results of the Chi-square test
print(chi_test)

# Interpretation based on p-value

if(chi_test$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
}

#Correlation and Regression Model
# Required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape2)
install.packages('reshape2')

# Correlation  ----
# Convert logical variables to numeric
data$search_conducted <- as.numeric(data$search_conducted) # TRUE = 1, FALSE = 0
data$contraband_found <- as.numeric(data$contraband_found) # TRUE = 1, FALSE = 0

# Select only numeric columns for correlation
numeric_data <- data %>% 
  select(subject_age, search_conducted, contraband_found)

sapply(numeric_data, sd, na.rm = TRUE) #sd for all the variables

numeric_data <- numeric_data[, sapply(numeric_data, sd, na.rm = TRUE) > 0] #to remove the sd of '0' value.

# Compute correlation matrix
cor_matrix <- cor(numeric_data)
print(cor_matrix)

# Plot the correlation heatmap
png("Correlation_Heatmap.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", # Display correlation coefficients
         title = "Correlation Heatmap", 
         mar = c(0, 0, 1, 0))
dev.off()

# Logistic Regression: Contraband Found as the Dependent Variable
# Fit logistic regression model
logistic_model <- glm(contraband_found ~ subject_age + search_conducted, 
                      data = data)
  
summary(logistic_model)









