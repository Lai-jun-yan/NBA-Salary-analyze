#data = read.csv(file.choose())

library(broom)
library(ggplot2)
library(dplyr)

data <- data %>%
    mutate(team = na_if(team, ""))
data$team[is.na(data$team)] <- "Free agent"

data <- data %>%
  mutate(college = na_if(college, ""))

data$salary <- gsub("\\$|,", "", data$salary)  # 去除 $ 和 ,
data$salary <- as.numeric(data$salary)/1000000  # 轉為數值

data$experience = 2020 - data$draft_year

team_salary <- data %>%
  group_by(team) %>%
  summarise(avg_salary = mean(salary, na.rm = TRUE)) %>%
  arrange(desc(avg_salary))

