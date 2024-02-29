library(tidyverse)
# Import Data
df <- read_csv('https://www.dropbox.com/scl/fi/umsuyuq3c4gj0bju6arfw/11_train.csv?rlkey=ty34yltxl41eycqbccidrgjxz&dl=1')
df %>% glimpse()

table(df$Diabetes_binary)

summary(df$Diabetes_binary)

ggplot(df, aes(x = factor(Diabetes_binary))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Diabetes",
       x = "Presence",
       y = "Frequency")

ggplot(df, aes(x = "", fill = factor(Diabetes_binary))) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Diabetes")


# boxplots of every column in the data set 
df %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = value, fill = name)) + 
  geom_boxplot() + 
  facet_wrap(~name, scales = 'free_y')

# ***************various groupings**********************
# health related variables
health_cols <- c("GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "HighBP", "HighChol", "Stroke", "HeartDiseaseorAttack")
# Lifestyle grouping
life_cols <- c("Fruits", "Veggies", "HvyAlcoholConsump", "Smoker", "PhysActivity")
# Demographic Variables
demo_cols <- c("Age", "Sex", "Education", "Income")
# Binary Variables
binary_cols <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", 
                "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", 
                "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", 
                "NoDocbcCost", "DiffWalk")

# categorical and continuous variables
categorical_variables <- c(
  "Diabetes_binary",
  "HighBP",
  "HighChol",
  "CholCheck",
  "Smoker",
  "Stroke",
  "HeartDiseaseorAttack",
  "PhysActivity",
  "Fruits",
  "Veggies",
  "HvyAlcoholConsump",
  "AnyHealthcare",
  "NoDocbcCost",
  "Sex",
  "Education",
  "GenHlth",
  "DiffWalk",
  "Income"
)

continuous_variables <- c(
  "BMI",
  "MentHlth",
  "PhysHlth",
  "Age"
)

df %>% 
  select(all_of(categorical_variables))

# distribution of all continuous variables
continuous_distribution_plot <- df %>%
  select(all_of(continuous_variables)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "blue", bins = 20) +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Value", y = "Frequency", title = "Histograms of Continuous Variables")

# Select categorical columns and convert to long format
df_categorized <- df %>%
  select(all_of(categorical_variables)) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")

# distribution of all categorical variables
categorical_disribution_plot <- ggplot(df_categorized, aes(x = value, fill = variable)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Category", y = "Count", fill = "Variable",
       title = "Distribution of Categorical Variables")

# count of missing data in each column
missing_counts <- colSums(is.na(df))

# Calculate the 99th percentile of all continuous variables
age_cap <- quantile(df$Age, .99)
bmi_cap <- quantile(df$BMI, .99)
ment_cap <- quantile(df$MentHlth, .99)
phys_cap <- quantile(df$PhysHlth, .99)


# PART C 

# Load the required packages for data manipulation, visualization, and modeling
library(tidyverse)
library(broom)
library(stats)

# PART 1: DATA PREPARATION AND OVERVIEW
# --------------------------------------------------

# Load the dataset from a provided URL and take a glimpse to understand its structure
df <- read_csv('https://www.dropbox.com/scl/fi/umsuyuq3c4gj0bju6arfw/11_train.csv?rlkey=ty34yltxl41eycqbccidrgjxz&dl=1')
df %>% glimpse()

# Count the number of observations for each Diabetes status to understand the distribution
df %>% 
  count(Diabetes_binary)

# PART 2: CORRELATION ANALYSIS FOR CONTINUOUS VARIABLES
# NOTE: We counted Age as continuous in this part since it had more than 10 categories.
# --------------------------------------------------

# Calculate and print the correlation matrix for selected continuous variables and Diabetes status
# This helps in identifying potential relationships worth exploring further
correlations <- cor(df[c("Diabetes_binary", "BMI", "MentHlth", "PhysHlth", "Age", "Income")], use="complete.obs")
print(correlations)

# PART 3: VISUALIZING RELATIONSHIPS WITH CONTINUOUS VARIABLES
# --------------------------------------------------

# Visualize the relationship between continuous variables and Diabetes status using box plots
# Box plots are chosen to summarize the distribution and median of these variables across the two groups

#BMI Correlation: 0.2169
ggplot(df, aes(x=factor(Diabetes_binary), y=BMI)) +
  geom_boxplot() +
  labs(title="Box Plot of BMI by Diabetes Status", x="Diabetes Binary", y="BMI") +
  theme_minimal()

#Age Correlation: 0.1776
ggplot(df, aes(x=factor(Diabetes_binary), y=Age)) +
  geom_boxplot() +
  labs(title="Box Plot of Age by Diabetes Status", x="Diabetes Binary", y="Age") +
  theme_minimal()

#PhysHlth Correlation: 0.1699
ggplot(df, aes(x=factor(Diabetes_binary), y=PhysHlth)) +
  geom_boxplot() +
  labs(title="Box Plot of PhysHlth by Diabetes Status", x="Diabetes Binary", y="PhysHlth") +
  theme_minimal()

#Income Correlation: -0.1650
ggplot(df, aes(x=factor(Diabetes_binary), y=Income)) +
  geom_boxplot() +
  labs(title="Box Plot of Income by Diabetes Status", x="Diabetes Binary", y="Income") +
  theme_minimal()

#MntlHlth Correlation: 0.0668
ggplot(df, aes(x=factor(Diabetes_binary), y=MentHlth)) +
  geom_boxplot() +
  labs(title="Box Plot of MentHlth by Diabetes Status", x="Diabetes Binary", y="MentHlth") +
  theme_minimal()

#Faceted

# Pivot the data frame to long format
long_df <- df %>%
  pivot_longer(cols = c("BMI", "Age", "PhysHlth", "Income", "MentHlth"), 
               names_to = "Variable", values_to = "Value")

# Generate the faceted box plot
ggplot(long_df, aes(x=factor(Diabetes_binary), y=Value)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free", ncol = 3) + # Adjust 'ncol' as needed for layout
  scale_x_discrete(labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
  labs(y = "", fill = "Diabetes Status") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Continuous Variables by Diabetes Status")

# PART 4: EXPLORING RELATIONSHIPS WITH CATEGORICAL AND BINARY VARIABLES
# --------------------------------------------------

# Calculate and print point-biserial correlation coefficients for binary variables
# This provides insight into the strength and direction of associations with Diabetes status
binary_vars <- c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", 
                 "HeartDiseaseorAttack", "PhysActivity", "Fruits", 
                 "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
                 "DiffWalk")

# Initialize an empty data frame to store the results
correlations_df <- data.frame(Variable = character(), Correlation = numeric(), stringsAsFactors = FALSE)

# Loop through each binary variable to calculate its correlation with Diabetes_binary
for(var in binary_vars) {
  correlation <- cor(df$Diabetes_binary, df[[var]], method = "pearson")
  # Append the results to the data frame
  correlations_df <- rbind(correlations_df, data.frame(Variable = var, Correlation = correlation))
}

# Sort the correlations by the absolute value of their strength
sorted_correlations <- correlations_df %>%
  mutate(AbsCorrelation = abs(Correlation)) %>%
  arrange(desc(AbsCorrelation)) %>%
  select(-AbsCorrelation) # Remove the absolute value column after sorting

# Print the sorted correlations
print(sorted_correlations)

# Visualizing the proportion of Diabetes status across binary variables using faceted bar plots
# Faceted bar plots allow for easy comparison across multiple variables
# These bar charts only include variables with a correlation less than -0.1 or greater than 0.1
filtered_long_df <- df %>%
  pivot_longer(cols = c("HighBP", "DiffWalk", "HighChol", "HeartDiseaseorAttack", "PhysActivity", "Stroke"), 
               names_to = "Variable", values_to = "Value")

# Create the faceted bar plot for the filtered variables
ggplot(filtered_long_df, aes(x=factor(Value), fill=factor(Diabetes_binary))) +
  geom_bar(position="fill") +
  facet_wrap(~ Variable, scales="free_x", nrow=3) + # Adjusted the number of rows for better layout
  scale_fill_manual(values=c("0"="blue", "1"="red"), labels=c("No Diabetes"="0", "Diabetes"="1")) +
  labs(title="Proportion of Diabetes by Binary Variables with Stronger Correlations",
       x="", y="Proportion", fill="Diabetes Status") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

# PART 5: MODELING THE IMPACT OF CATEGORICAL VARIABLES ON DIABETES
# --------------------------------------------------

# Fit a logistic regression model to assess the impact of General Health, Income, and Education on Diabetes status
model <- glm(Diabetes_binary ~ GenHlth + Income + Education, family=binomial(link="logit"), data=df)

# Summarize the model to see coefficients and their significance
# This helps in understanding the influence of each predictor on the likelihood of Diabetes
summary(model)

# PART 6: VISUALIZING THE MODEL FINDINGS
# --------------------------------------------------

# Visualize the model's findings with coefficient plots to highlight the impact of each predictor
tidy_model <- tidy(model) %>% 
  filter(term != "(Intercept)")

# Coefficient plot excluding the intercept to focus on predictors
ggplot(tidy_model, aes(x=term, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error)) +
  geom_pointrange() +
  geom_hline(yintercept=0, linetype="dashed") +
  coord_flip() +
  xlab("Variables") +
  ylab("Estimate") +
  ggtitle("Effect of General Health, Income, and Education on Diabetes")

# Odds ratio plot to provide a more interpretable measure of each predictor's impact
tidy_model$odds_ratio <- exp(tidy_model$estimate)
tidy_model$conf.low <- exp(tidy_model$estimate - 1.96 * tidy_model$std.error)
tidy_model$conf.high <- exp(tidy_model$estimate + 1.96 * tidy_model$std.error)

ggplot(tidy_model, aes(x=term, y=odds_ratio, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept=1, linetype="dashed") +
  coord_flip() +
  xlab("Variables") +
  ylab("Odds Ratio") +
  ggtitle("Odds Ratios for General Health, Income, and Education")

# PART 7: PREDICTING DIABETES PROBABILITY BASED ON SIGNIFICANT VARIABLES
# --------------------------------------------------

# Generate and plot predicted probabilities for significant predictors to illustrate their effect on Diabetes likelihood
# This series of plots conveys how the probability of Diabetes changes with varying levels of each significant predictor
# Predicted probabilities for General Health
# Did not create charts for income and diabetes
new_data <- expand.grid(GenHlth = unique(df$GenHlth), Income = median(df$Income, na.rm = TRUE), Education = median(df$Education, na.rm = TRUE))
new_data$Diabetes_Prob <- predict(model, newdata = new_data, type = "response")

ggplot(new_data, aes(x=GenHlth, y=Diabetes_Prob)) +
  geom_line() + geom_point() +
  xlab("General Health") + ylab("Probability of Diabetes") +
  ggtitle("Predicted Probability of Diabetes by General Health")

# Repeating the process for Income and Education, altering `new_data` accordingly

# Final thoughts and narrative:
# This analysis journey from data loading to predictive modeling showcases a comprehensive exploration of factors associated with Diabetes. By systematically analyzing continuous, binary, and categorical variables, we've identified key predictors and visualized their impact. The logistic regression model further quantifies these relationships, offering insights into how variations in General Health, Income, and Education levels influence the likelihood of Diabetes. This narrative not only enhances our understanding of the dataset but also guides future data-driven decisions and interventions.
