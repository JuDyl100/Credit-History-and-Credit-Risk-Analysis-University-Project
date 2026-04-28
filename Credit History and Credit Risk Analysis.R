# Libraries
library(DataExplorer)
library(dplyr)
library(naniar)
library(outliers)
library(mice)
library(ggplot2)
library(ggpubr)     
library(scales)    
library(stats)
library(writexl)
# Import Data
df <- read.csv("C:\\Users\\Julio Dylan\\Desktop\\R Programming\\5. credit_risk_classification (1).csv")

# Replace missing values
df <- df %>% mutate(across(where(is.character), ~replace(., . == '', NA)))

# Remove rows with more than 3 missing values
delete.na = function(data, n = 0) {
  data[rowSums(is.na(data)) <= n,]
}
df = delete.na(df, 3)

# Mean/Median Imputation for Numerical Columns
df$duration = replace(df$duration, is.na(df$duration), mean(df$duration, na.rm = TRUE))
df$credit_amount = replace(df$credit_amount, is.na(df$credit_amount), median(df$credit_amount, na.rm = TRUE))
df$installment_commitment = replace(df$installment_commitment, is.na(df$installment_commitment), mean(df$installment_commitment, na.rm = TRUE))
df$residence_since = replace(df$residence_since, is.na(df$residence_since), mean(df$residence_since, na.rm = TRUE))
df$age = replace(df$age, is.na(df$age), mean(df$age, na.rm = TRUE))
df$existing_credits = replace(df$existing_credits, is.na(df$existing_credits), median(df$existing_credits, na.rm = TRUE))
df$num_dependents = replace(df$num_dependents, is.na(df$num_dependents), mean(df$num_dependents, na.rm = TRUE))

# Mode Imputation for Categorical Columns
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

cols_to_impute <- c("checking_status", "credit_history", "purpose", "savings_status", 
                    "employment", "personal_status", "other_parties", "property_magnitude", 
                    "other_payment_plans", "housing", "job", "own_telephone", "foreign_worker")

for (col in cols_to_impute) {
  df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
}

# Data Label Manipulation
df <- df %>%
  mutate(
    own_telephone = recode(own_telephone, "none" = "no"),
    savings_status = recode(savings_status, "500<=X<10000" = "500<=X<1000")
  )

# Reset Row Indices
row.names(df) <- NULL

write_xlsx(df, "C:\\Users\\Julio Dylan\\Desktop\\R Programming\\5. credit_risk_classification (1).csv")

# Objective 3: Relation of Credit History and Credit Risk

# Analysis 1: How are customers distributed across different credit history categories, and what is the relation between these categories and credit class(good/bad)? (Descriptive)

credit_history_distribution = df %>%
  count(credit_history, class) %>%
  group_by(credit_history) %>%
  mutate(percentage = n / sum(n) * 100)
credit_history_distribution 
# Visualization Analysis 1
ggplot(credit_history_distribution, aes(x = credit_history, y = n, fill = class)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9), size = 4, color = "black") +
  labs(
    title = "Credit History Categories and Relationship with Credit Class",
    x = "Credit History",
    y = "Customer Count"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  ) + 
  scale_fill_manual(values = c("tomato", "steelblue"))

# Special Feature 1 = Chi Squared Test Analysis 1

credit_history_table <- table(df$credit_history, df$class)
chi_sq_test <- chisq.test(credit_history_table)
print(chi_sq_test)

# Analysis 2: Average Duration of Credit by Credit History and Credit Risk Class (Descriptive)
duration_summary <- df %>%
  group_by(credit_history, class) %>%
  summarise(
    avg_duration = mean(duration, na.rm = TRUE), 
    .groups = 'drop'
  )
duration_summary
# Visualization Analysis 2
ggplot(duration_summary, aes(x = credit_history, y = avg_duration, fill = class)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = round(avg_duration, 1)), vjust = -0.5, position = position_dodge(0.9), color = "black") +
  labs(
    title = "Average Loan Duration by Credit History and Credit Risk Class",
    x = "Credit History",
    y = "Average Duration (months)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("red", "steelblue"))

#Special Feature 2: ANOVA
anova_result <- aov(duration ~ credit_history * class, data = df)
summary(anova_result)

#Analysis 3: Base on the credit history which one can be classified as good or bad (Predictive)
df$class <- factor(df$class, levels = c("good", "bad"))
df$credit_history <- factor(df$credit_history)

#Visualization Analysis 3

ggplot(df, aes(x = credit_history, fill = class)) +
  geom_bar(position = "dodge", color = "black") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5, color = "black") +  
  labs(title = "Credit History vs Class", 
       x = "Credit History", 
       y = "Count") +
  scale_fill_manual(values = c("good" = "darkgreen", "bad" = "tomato")) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", size = 1, fill = NA),  
    plot.title = element_text(hjust = 0.5) 
    
  )

#Step 2 : Logistic Regression
model <- glm(class ~ credit_history, data = df, family = "binomial")
summary(model)

#Step 3 : Predict Probabilities
# Make predictions on the data (probabilities)
pred_prob <- predict(model, type = "response")

# Convert probabilities to class predictions (threshold = 0.5)
pred_class <- ifelse(pred_prob > 0.5, "good", "bad")

#Step 4 : Confusion Matrix

# Create confusion matrix
confusion_matrix <- table(pred_class, df$class)
print(confusion_matrix)

# Calculate accuracy
accuracy <- mean(pred_class == df$class)
print(paste("Accuracy:", accuracy))

