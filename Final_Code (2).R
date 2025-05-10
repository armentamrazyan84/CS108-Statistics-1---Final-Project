# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Set options to avoid scientific notation and control the number of digits displayed
options(scipen = 999, digits = 6)

# Load the dataset
df <- read_excel("/Users/hmayakparavyan/Desktop/Dataset.xlsx", sheet = "LFS_Year_2023", guess_max = 10000)

# Adding ranges for salaries
sample <- df %>%
  mutate(`Salary in Range` = ifelse((is.na(`Salary in Range`) | `Salary in Range` == "") & !is.na(`Exact Salary`), 
                                  case_when(
                                    `Exact Salary` <= 55000 ~ "Up to 55 000 AMD",
                                    `Exact Salary` >= 55001 & `Exact Salary` <= 110000 ~ "55 001 - 110 000 AMD",
                                    `Exact Salary` >= 110001 & `Exact Salary` <= 220000 ~ "110 001 - 220 000 AMD",
                                    `Exact Salary` >= 220001 & `Exact Salary` <= 440000 ~ "220 001 - 440 000 AMD",
                                    `Exact Salary` >= 440001 & `Exact Salary` <= 600000 ~ "440 001 - 600 000 AMD",
                                    `Exact Salary` >= 600001 & `Exact Salary` <= 700000 ~ "600 001 - 700 000 AMD",
                                    `Exact Salary` >= 700001 ~ "Above 700000"
                                  ), 
                                  `Salary in Range`))


# Sample of data for later graph about Steps to Find Job and Obstacles to Find Job

find_obstacle <- sample %>%
  filter(!is.na(`Steps to Find Job`) & `Steps to Find Job` != "" &
           !is.na(`Obstacles to Find Job`) & `Obstacles to Find Job` != "")





# Wage Distribution by Education Level graph creation

ggplot(sample, aes(x = `Exact Salary`, y = `Education level`, fill = `Education level`)) +
  geom_boxplot() +
  labs(
    title = "Wage Distribution by Education Level",
    x = "Exact Salary (Wage)",
    y = "Education Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 0),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14), 
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom", 
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") 





# Wage Distribution by Age Range graph creation

ggplot(sample, aes(x = `Exact Salary`, y = `Age Range`, fill = `Age Range`)) +
  geom_boxplot() +
  labs(
    title = "Wage Distribution by Age Range",
    x = "Exact Salary (Wage)",
    y = "Age Range"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title = element_text(size = 14), 
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),
    legend.position = "bottom", 
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_fill_brewer(palette = "Set3")





# Clean the data set by removing specific outlines based on conditions(explained in the pdf file)
cleaned_sample <- sample %>%
  filter(
    !(`Education level` == "Secondary specialized" & `Exact Salary` == 1000000) & 
      !(`Education level` == "Master's degree" & `Exact Salary` == 1500000) & 
      !(`Education level` == "Bachelor's degree" & `Exact Salary` == 1500000) & 
      !(`Education level` == "Certified specialist" & `Exact Salary` == 1900000)
  )

# recreating the previous 2 graphs





ggplot(cleaned_sample, aes(x = `Exact Salary`, y = `Education level`, fill = `Education level`)) +
  geom_boxplot() +
  labs(
    title = "Wage Distribution by Education Level",
    x = "Exact Salary (Wage)",
    y = "Education Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 0),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14), 
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom", 
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_fill_brewer(palette = "Set3") 




ggplot(cleaned_sample, aes(x = `Exact Salary`, y = `Age Range`, fill = `Age Range`)) +
  geom_boxplot() +
  labs(
    title = "Wage Distribution by Age Range",
    x = "Exact Salary (Wage)",
    y = "Age Range"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.title = element_text(size = 14), 
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),
    legend.position = "bottom", 
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_fill_brewer(palette = "Set3")




# Create the education rate by education level
education_rate <- sample %>%
  filter(!is.na(`Education level`) & !is.na(`Education Match Status`)) %>%
  group_by(`Education level`) %>%
  summarise(Yes_Rate = mean(`Education Match Status` == "Yes", na.rm = TRUE) * 100)

# Create the bar plot with adjusted palette and handling missing data
ggplot(education_rate, aes(x = `Education level`, y = Yes_Rate, fill = `Education level`)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 50, color = "red", linetype = "dashed", size = 1) + 
  labs(
    title = "Education Match Rate by Education Level",
    x = "Education Level",
    y = "Match Rate (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),  # Y-axis font size
    axis.title = element_text(size = 14),  # Axis title font size
    plot.title = element_text(size = 16, hjust = 0.5),  # Title font size and alignment
    legend.title = element_text(size = 12),  # Legend title font size
    legend.text = element_text(size = 10),  # Legend text font size
    legend.position = "none",  # Remove the legend
    plot.margin = margin(1, 1, 1, 1, "cm")  # Adjust plot margins
  ) +
  scale_fill_brewer(palette = "Paired")  # Change palette to Paired (supports more colors)




# Remove rows with missing Age Range or Education Match Status
age_rate <- sample %>%
  filter(!is.na(`Age Range`) & !is.na(`Education Match Status`)) %>%
  group_by(`Age Range`) %>%
  summarise(Yes_Rate = mean(`Education Match Status` == "Yes", na.rm = TRUE) * 100)

# Create the bar plot with adjusted palette and handling missing data
ggplot(age_rate, aes(x = `Age Range`, y = Yes_Rate, fill = `Age Range`)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 50, color = "red", linetype = "dashed", size = 1) + 
  labs(
    title = "Education Match Rate by Age Range",
    x = "Age Range",
    y = "Match Rate (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),
    legend.position = "none", 
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_fill_brewer(palette = "Paired")  # Change palette to 'Paired' (supports more colors)





# Check frequency of unique values in Steps to Find Job

steps_frequency <- find_obstacle %>%
  count(`Steps to Find Job`) %>%
  arrange(desc(n))  # Sort by frequency

# Bar plot for Steps to Find a Job
ggplot(steps_frequency, aes(x = reorder(`Steps to Find Job`, n), y = n, fill = `Steps to Find Job`)) +
  geom_bar(stat = "identity") +
  labs(title = "Job Search Behavior: Steps to Find a Job",
       x = "Steps to Find a Job",
       y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.text = element_text(size = 10),  # Adjust text size
    axis.title = element_text(size = 12),  # Adjust axis title size
    plot.title = element_text(size = 16, hjust = 0.5)  # Adjust plot title size and position
  )





##CI1: CI for the proportion of people working in Yerevan who report a job matching their education.

# Filter for Yerevan and valid Education Match Status responses
df_yerevan <- sample %>%
  filter(`Region` == "Yerevan", `Education Match Status` %in% c("Yes", "No"))

# Count total and matches
n <- nrow(df_yerevan)
successes <- sum(df_yerevan$`Education Match Status` == "Yes")

# Proportion
p_hat <- successes / n

# Population size (Yerevan's total population)
N <- 1095000

# 99% confidence interval (normal approximation)
z <- qnorm(0.995)  # For 99% CI (two-tailed, so 0.995 for the upper tail)
# Calculate the standard error with finite population correction
se_corrected <- sqrt((p_hat * (1 - p_hat) / n) * ((N - n) / (N - 1)))

# Calculate the confidence interval
ci_lower <- p_hat - z * se_corrected
ci_upper <- p_hat + z * se_corrected

# Display results
cat("Sample size:", n, "\n")
cat("Proportion with matching education:", round(p_hat, 4), "\n")
cat("99% Confidence Interval (with finite population correction): [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")





##CI2: CI for the Mean Wage by Education Level

# Filter dataset to include only rows where 'Education Match Status' is either 'Yes' or 'No'
filtered_data <- sample %>%
  filter(`Education Match Status` %in% c("Yes", "No"))

ci_results <- filtered_data %>%
  group_by(`Education level`) %>% # Group by Education level
  summarise(
    n = n(), # Count the number of observations in each group
    mean_salary = mean(`Exact Salary`, na.rm = TRUE), # Calculate mean salary, ignoring NA values
    std_err = ((sd(`Exact Salary`, na.rm = TRUE) / sqrt(n))), # Standard error of the mean
    .groups = "drop" # Drop the grouping after summarisation
  ) %>%
  mutate(
    # Calculate the 95% confidence interval (CI) for the mean salary
    CI_lower = mean_salary - qt(1 - 0.05/2, n - 1) * std_err,
    CI_upper = mean_salary + qt(1 - 0.05/2, n - 1) * std_err
  )

# Print the results (confidence intervals for each education level)
print(ci_results)

# Create a readable statement for each Education Level with the confidence interval and print it
ci_results %>%
  rowwise() %>%
  mutate(statement = paste0(
    `Education level`, ": [", 
    round(CI_lower, 2), ", ", round(CI_upper, 2), "]"
  )) %>%
  pull(statement) %>% # Extract the 'statement' column
  cat(sep = "\n\n") # Print each statement on a new line with a space between them




##Hypothesis Testing 1: Test whether the mean wage of those whose job matches their qualification is higher than those whose job doesn’t. 

# Filter out missing salary values and create two groups: matched vs. not matched
df_clean <- df %>%
  filter(!is.na(`Exact Salary`), `Education Match Status` %in% c("Yes", "No"))

# Split the data into two groups: job matches education and job does not match
df_matched <- df_clean %>%
  filter(`Education Match Status` == "Yes")
df_not_matched <- df_clean %>%
  filter(`Education Match Status` == "No")

# Perform a t-test comparing the mean salaries of the two groups
t_test_result <- t.test(df_matched$`Exact Salary`, df_not_matched$`Exact Salary`, alternative = "greater")

# Extract p-value and t-statistic from the t-test result
p_value <- t_test_result$p.value
t_statistic <- t_test_result$statistic

# Display the t-test results
cat("T-Test Results: Comparing Mean Salaries Based on Job-Education Match\n")
cat("---------------------------------------------------------------\n")
cat("T-Statistic: ", round(t_statistic, 3), "\n")
cat("P-Value: ", format(p_value, scientific = FALSE), "\n")
cat("\n")

# Check whether the p-value is smaller than 0.05 and draw a conclusion
if (p_value < 0.05) {
  cat("Conclusion: We reject the null hypothesis. There is a statistically significant difference in mean salaries between the two groups (Matched vs. Non-Matched). The salary for matched jobs is significantly higher than for non-matched jobs.\n")
} else {
  cat("Conclusion: We fail to reject the null hypothesis. There is no statistically significant difference in mean salaries between the two groups (Matched vs. Non-Matched).\n")
}




##Hypothesis Testing 2: Test for independence between education level and job-education match status.

# Filter data for relevant columns and remove rows with NA values in 'Education level' or 'Education Match Status'
df_clean1 <- df %>%
  filter(!is.na(`Education level`), !is.na(`Education Match Status`))

# Create a contingency table for 'Education level' and 'Education Match Status'
contingency_table <- table(df_clean1$`Education level`, df_clean1$`Education Match Status`)

# Perform Chi-square test of independence
chi_square_result <- chisq.test(contingency_table)

# Print the Chi-square test result
cat("Chi-square Statistic: ", chi_square_result$statistic, "\n")
cat("Degrees of Freedom: ", chi_square_result$parameter, "\n")
cat("p-value: ", chi_square_result$p.value, "\n")

# Interpretation
if (chi_square_result$p.value < 0.05) {
  cat("Conclusion: There is a significant relationship between Education level and Job-Education match status.\n")
} else {
  cat("Conclusion: There is no significant relationship between Education level and Job-Education match status.\n")
}

