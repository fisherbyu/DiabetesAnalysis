library(tidyverse)
# Import Data
df <- read_csv('https://www.dropbox.com/scl/fi/umsuyuq3c4gj0bju6arfw/11_train.csv?rlkey=ty34yltxl41eycqbccidrgjxz&dl=1')
df

# boxplots of every column in the data set 
df %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = value, fill = name)) + 
  geom_boxplot() + 
  facet_wrap(~name, scales = 'free_y')

# various groupings
health_conditions <- c('Diabetes_binary', 'HighBP', 'HighChol', 'Stroke', 'HeartDiseaseorAttack')
lifestyle_factors <- c('Smoker', 'PhysActivity', 'Fruits')
health_monitoring

# categorical and continuous variables
categorical_variables <- c('Diabetes_binary','HighBP','HighChol','CholCheck','Smoker','Stroke','HeartDiseaseorAttack','PhysActivity','Fruits')
continous_variables <- 'BMI'

df %>% 
  select(all_of(categorical_variables))

bmi_cap <- quantile(df$BMI, .99) 

# distribution of BMI values
df %>% 
  select(BMI, Diabetes_binary) %>% 
  ggplot(aes(x = BMI)) + 
  geom_histogram(fill = "blue")
  

df %>% 
  glimpse()
