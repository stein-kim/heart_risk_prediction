# Install packages and load libraries
install.packages("readr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("haven")
install.packages("corrgram")
install.packages("caTools")
install.packages("MASS")
install.packages("ROCR")

library(haven)
library(tidyverse)
library(dplyr)
library(corrgram)
library(ggplot2)
library(RColorBrewer) # for visualizations
library(caTools)
library(MASS)
library(yardstick) # for confusion matrix

# Load data
brfss <- read_xpt('LLCP2022.XPT', .name_repair = 'unique')
head(brfss)

# Check column names
colnames(brfss)

# Create data frame using specific columns
df1 <- brfss[,c('SEXVAR', '_AGEG5YR', 'GENHLTH', 'WEIGHT2', 'WTKG3', 'HTM4', 
               'HTIN4', '_BMI5', '_SMOKER3', '_CURECI2', 'DRNKANY6', 
               'EXERANY2', 'SLEPTIM1', '_MICHD')]
View(df1)
head(df1)
summary(df1)

# Remove additional columns
df <- df1[,c('SEXVAR', '_AGEG5YR', 'GENHLTH', 'WEIGHT2', 'HTIN4', '_SMOKER3', 
               '_CURECI2', 'DRNKANY6', 'EXERANY2', 'SLEPTIM1', '_MICHD')]
summary(df)

# Remove NA values
df <- na.omit(df)
head(df)
summary(df)

# Rename columns
colnames(df) <- c("sex", "age", "gen_health", "weight", "height", "smoker", 
                  "ecig_use", "alc_last30", "ex_last30", "hrs_sleep", "mi_chd")
head(df)

# Check for outliers in data
boxplot(df)
boxplot(df$weight)
df |> dplyr::select(sex:gen_health, height:mi_chd) |> boxplot()

# Find and remove outliers in weight variable
summary(df$weight)
unique(df$weight)
df <- df |> filter(weight < 999)
summary(df$weight)
print(table(df$weight))
df <- df |> filter(weight > 79)
summary(df$weight)
boxplot(df$weight)

# Find and remove outliers in sleep column
summary(df$hrs_sleep)
unique(df$hrs_sleep)
df <- df |> filter(hrs_sleep < 25)
summary(df$hrs_sleep)
print(table(df$hrs_sleep))
df <- df |> filter(hrs_sleep < 17)
df <- df |> filter(hrs_sleep > 3)
summary(df$hrs_sleep)

# Check for outliers in height column
summary(df$height)
print(table(df$height))
df <- df |> filter(height < 85)
df <- df |> filter(height > 58)
summary(df$height)

# Check summary again
summary(df)

# Gather information on unknown data values for each variable
print(table(df$age))
print(table(df$gen_health))
print(table(df$smoker))
print(table(df$ecig_use))
print(table(df$alc_last30))
print(table(df$ex_last30))
print(table(df$sex))

# Remove unknown values (7, 9, 14)
df <- df |> filter(age < 14) |>
  filter(gen_health < 6) |>
  filter(smoker < 5) |>
  filter(ecig_use < 3) |>
  filter(alc_last30 < 3) |>
  filter(ex_last30 < 3)
summary(df)

# Create BMI calculation column
df$BMI <- df$weight*0.453/(df$height*0.0254)**2
summary(df)

# Change binary columns to 0 for no
df$sex[df$sex == 2] <- 0
df$male <- df$sex
df$ecig_use[df$ecig_use == 1] <- 0
df$ecig_use[df$ecig_use == 2] <- 1
df$alc_last30[df$alc_last30 == 2] <- 0
df$ex_last30[df$ex_last30 == 2] <- 0
df$mi_chd[df$mi_chd == 2] <- 0
summary(df)

# Remove height, weight and sex columns
df <- subset(df, select = -c(sex, height, weight))
summary(df)

# Save to CSV
write.csv(df, 'heart_pred_clean.csv')

# Create corrgram to compare variables
corrgram(df)

# Create visualizations
# Biological sex
hist(df$male, 
     col = c("orange", "blue"),
     xlab = "Male or Female",
     ylab = "Frequency", 
     main = "Number of Participants by Sex",
     breaks = 2)
legend("right", legend = c("Female","Male"), col = c("orange", "blue"), pt.cex = 2, pch = 15)

# E-cig use
hist(df$ecig_use, 
     col = c("orange", "blue"),
     xlab = "User or Non-user",
     ylab = "Frequency", 
     main = "Number of Participants by E-cigarette Use",
     breaks = 2)
legend("right", legend = c("Non-user","User"), col = c("orange", "blue"), pt.cex = 2, pch = 15)
table(df$ecig_use)

# Smoking status
table(df$smoker) |>
  barplot(
     col = c("orange", "darkorange", "blue", "lightblue"),
     xlab = "Smoking status",
     ylab = "Frequency", 
     main = "Number of Participants by Smoking Status",
     xlim = range(0:5))
legend("topleft", legend = c("Daily Smoker", "Smokes Some Days",
                             "Former smoker", "Never Smoked"), 
       col = c("orange", "darkorange", "blue", "lightblue"), pt.cex = 2, 
       pch = 15)
table(df$smoker)

# Alcohol use
hist(df$alc_last30, 
     col = c("orange", "blue"),
     xlab = "Drank or Not",
     ylab = "Frequency", 
     main = "Number of Participants who Drank in Last 30 Days",
     breaks = 2)
legend("right", legend = c("No","Yes"), col = c("orange", "blue"), pt.cex = 2, pch = 15)
table(df$alc_last30)

# General health
table(df$gen_health) |>
  barplot(
    col = c("orange", "darkorange", "darkblue", "blue", "lightblue"),
    xlab = "General Health Status",
    ylab = "Frequency", 
    main = "Number of Participants by General Health Status")
legend("topright", legend = c("Excellent", "Very Good","Good", "Fair", "Poor"), 
       col = c("orange", "darkorange", "darkblue", "blue", "lightblue"), pt.cex = 2, pch = 15)
table(df$gen_health)

# Age groups
# Set color
color <- brewer.pal(11, "RdBu")
table(df$age) |>
  barplot(
    col = color,
    xlab = "Age Group",
    ylab = "Number of Participants",
    main = "Number of Participants by Age Group")
table(df$age)

# Logistic Regression
# Reorder columns
df <- df |> dplyr::select(mi_chd, male, age, gen_health, BMI, smoker, ecig_use, 
                   alc_last30, ex_last30, hrs_sleep)

# Create train and test sets
set.seed(12) # To ensure reproducibility
index <- sample.split(Y = df$mi_chd, SplitRatio = 0.8)
train <- df[index,]
test <- df[!index,]
dim(train)
dim(test)

# Create regression model
initial_model <- glm(mi_chd ~ ., data = train, 
             family = binomial)
initial_model
summary(initial_model)

# Perform stepwise regression
step_model <- initial_model |> stepAIC()
summary(step_model)

forward_model <- initial_model |>
  stepAIC(direction = "forward")
summary(forward_model)

backward_model <- initial_model |>
  stepAIC(direction = "backward")
summary(backward_model)

# Create decreased models for comparison
model_2 <- glm(mi_chd ~ male + age + gen_health + BMI + smoker + alc_last30 + 
                 hrs_sleep, data = train, family = binomial)
summary(model_2)

model_3 <- glm(mi_chd ~ male + age + BMI:hrs_sleep, 
               data = train, family = binomial)
summary(model_3)

model_4 <- glm(mi_chd ~ male + BMI:hrs_sleep, 
               data = train, family = binomial)
summary(model_4)

# Test initial model
probabilities <- predict(initial_model, test, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)
# Check accuracy
observations <- test$mi_chd

# Create confusion matrix
outcomes <- table(predictions, observations)
outcomes
confusion <- conf_mat(outcomes)
autoplot(confusion)
summary(confusion, event_level = "second")

