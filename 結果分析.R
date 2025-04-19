#球員薪資分布
ggplot(data, aes(x = salary)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Salary Distribution of NBA Players", x = "Salary(Million)", y = "Count") +
  theme_minimal()

#薪資跟評分的關係
ggplot(data, aes(x = rating, y = salary)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Player Rating vs Salary", x = "Rating", y = "Salary") +
  theme_minimal()

model_rating_salary <- lm(salary ~ rating, data = data)
stargazer(model_rating_salary, type = "text", title = "Regression Results", style = "default")

#薪資跟打球年資的關係
ggplot(data, aes(x = experience, y = salary)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Player Rating vs Experience", x = "Rating", y = "Salary") +
  theme_minimal()

model_exp_salary <- lm(salary ~ experience, data = data)
stargazer(model_exp_salary, type = "text", title = "Regression Results", style = "default")

#各隊平均薪資
ggplot(team_salary, aes(x = reorder(team, avg_salary), y = avg_salary)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = round(avg_salary, 0)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Average Salary by Team", x = "Team", y = "Avg Salary") +
  theme_minimal()

anova_result <- aov(salary ~ team, data = data)
summary(anova_result)

#哪個因素最影響薪資
data_std <- data %>%
  mutate(across(c(salary, rating, experience), scale))

model_std <- lm(salary ~ rating + experience, data = data_std)
tidy_model_std <- tidy(model_std)

tidy_model_std$group <- case_when(
  tidy_model_std$term == "rating" ~ "Rating",
  tidy_model_std$term == "experience" ~ "Experience",
  TRUE ~ "Other"
)

impact_summary <- tidy_model_std %>%
  filter(group != "Other") %>%
  group_by(group) %>%
  summarise(total_effect = sum(abs(estimate))) %>%
  arrange(desc(total_effect))

ggplot(impact_summary, aes(x = reorder(group, total_effect), y = total_effect)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Which Factor Influences Salary the Most?",
       x = "Variable Type", y = "Total Impact (Sum of Std. Coefficients)") +
  theme_minimal()

#生成影響力薪資表格
tidy_model_std <- tidy(model_std) %>%
  filter(term %in% c("rating", "experience")) %>%
  mutate(impact = abs(estimate)) %>%
  rename(Estimate = estimate,
         `Std.Error` = std.error,
         `t value` = statistic,
         `p value` = p.value,
         Variable = term)

tidy_model_std %>%
  kbl(digits = 4, caption = "Standardized Regression Coefficients") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, position = "center")






