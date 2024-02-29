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
                "NoDocbcCost")

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
