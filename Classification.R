rm(list = ls())

# Packages Installation ---------------------------------------------------

packages <- c("ggthemes", "tidyverse", "glmnet","MASS","randomForest")

for (package_name in packages) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}

# Loading Packages --------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(MASS)
library(glmnet) # Lasso
library(randomForest) # RF

# Loading Data -------------------------------------------------------------------------

getwd()
# setwd("C:/Users/gabri/OneDrive/Desktop/Trapin Project/Classification")
smoke <- read.csv("smoking_data.csv", sep = ";")
head(smoke)
tail(smoke)

# FUNCTIONS ---------------------------------------------------------------
# 1) Accuracy Function -------------------------------------------------------

# This function, given a classification model and a test set, 
# return accuracy metrics from the confusion matrix. It was built
# to speed up the validation process of the models.

calculate_metrics <- function(model, test) {
  
  # Get predicted classes and probabilities
  predictions <- predict(model, test, type = "response")
  predicted_classes <- ifelse(predictions >= 0.5, 1, 0)
  
  # Calculate confusion matrix
  confusion_matrix <- table(test$smoking, predicted_classes)
  
  # true negative (TN), true positive (TP), false positive (FP), false negative (FN)
  TN <- confusion_matrix[1]
  FN <- confusion_matrix[2]
  FP <- confusion_matrix[3]
  TP <- confusion_matrix[4]
  
  # Calculate metrics
  PPV <- TP / (TP + FP)  # Positive Predictive Value / Precision
  TPR <- TP / (TP + FN)  # True Positive Rate / Sensitivity / Recall
  FPR <- FP / (TN + FP)  # False Positive Rate
  FNR <- FN / (FN + TP)  # False Negative Rate
  ACC <- (TN + TP) / nrow(test) 
  
  # Create a list to store the metrics
  metrics <- list(Confusion_Matrix = confusion_matrix,
                  Precision = PPV,
                  Sensitivity = TPR,
                  False_Positive_Rate = FPR,
                  False_Negative_Rate = FNR,
                  Accuracy = ACC)
  return(metrics)
}

# FIXING VARIABLES --------------------------------------------------------

# Age | Height | Weight were approximated with a range of 5
# (see original dataset).
# We decided to randomize them with adding to each value
# -+2 just to make them as similar as the original values
# as possible.

set.seed(1234)

# 1) Age ---------------------------------------------------------------------

a <- sort(unique(smoke$age))
for (j in a) {
  for (i in 1:55517) {
    if (smoke$age[i] == j) {
      smoke$age[i] <- sample((j-2):(j+2), 1)
    }
  }
}

# 2) Height ---------------------------------------------------------------

b <- sort(unique(smoke$height))
for (j in b) {
  for (i in 1:55517) {
    if (smoke$height[i] == j) {
      smoke$height[i] <- sample((j-2):(j+2), 1)
    }
  }
}

# 3) Weight ---------------------------------------------------------------

c <- sort(unique(smoke$weight))
for (j in c) {
  for (i in 1:55517) {
    if (smoke$weight[i] == j) {
      smoke$weight[i] <- sample((j-2):(j+2), 1)
    }
  }
}

# FEATURE ENGENEERING -----------------------------------------------------

# 1) Bmi ------------------------------------------------------------------

# height is divided by 100 because the original formula
# assumes that the measure is meters, while we have centimeters
smoke$bmi <- smoke$weight/((smoke$height/100)^2)

smoke <- smoke %>%
  mutate(bmi_status =
           case_when(
             bmi <  18.5 ~ "Underweight",
             bmi >= 18.5 & bmi < 25 ~ "Healthy_Weight",
             bmi >= 25 & bmi < 30 ~ "Overweight",
             bmi >= 30 ~ "Obese"))

smoke$bmi_status <- factor(smoke$bmi_status)

# FIXING VARIABLES CLASSES -------------------------------------------------

str(smoke)

smoke <- smoke[,-1] # remove id column

smoke$gender        <- as.factor(smoke$gender)
smoke$pressure_S    <- as.integer(smoke$pressure_S)
smoke$pressure_D    <- as.integer(smoke$pressure_D)
smoke$GTP           <- as.integer(smoke$GTP)
smoke$dental_caries <- as.factor(smoke$dental_caries)
smoke$tartar        <- as.factor(smoke$tartar)
smoke$smoking       <- as.factor(smoke$smoking)
smoke$hearing_L     <- ifelse(smoke$hearing_L == "normal",0,1)
smoke$hearing_L     <- as.factor(smoke$hearing_L)
smoke$hearing_R     <- ifelse(smoke$hearing_R == "normal",0,1)
smoke$hearing_R     <- as.factor(smoke$hearing_R)

str(smoke)

# DATA EXPLORATION -------------------------------------------------------------------------

names(smoke)
summary(smoke)

# 1) Smoking --------------------------------------------------------------

unique(smoke$smoking)
table(smoke$smoking, smoke$gender)

smoke %>% 
  ggplot(aes(x = smoking)) +
  geom_bar(fill = c("darkgreen","black"),
           alpha = 0.7, 
           width = 0.6) +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + #remove grids
  labs(title = "Sample distribution by smoker") +
  xlab("")

# 2) Gender ---------------------------------------------------------------

unique(smoke$gender)
table(smoke$gender)

smoke %>% 
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar(position = "dodge", 
           alpha = 0.8, 
           width = 0.5,
           show.legend = F) +
  scale_fill_manual(values=c("#FF6666", "Blue")) +
  scale_x_discrete(labels=c("Females", "Males"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + #remove grids
  labs(title = "Sample distribution by gender",
       x = "")

# 2.1) Gender & Smoking ---------------------------------------------------

table(smoke$smoking, smoke$gender)

smoke %>% 
  ggplot(aes(x = smoking, fill = gender)) +
  geom_bar(position = "dodge", 
           alpha = 0.8, 
           width = 0.5,
           show.legend = T) +
  scale_fill_manual(values=c("#FF6666", "blue")) +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + #remove grids
  labs(title = "NON Smokers & Smokers per gender",
       y = "# smokers",
       x = "")

# 3) Age ------------------------------------------------------------------

unique(smoke$age)

# Histogram
smoke %>% 
  ggplot(aes(x = age)) +
  geom_histogram(color = "black", fill = "white", bins = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Population Distribution by age") +
  ylab("") +
  xlab("Age")
  
# 3.1) Age & Smoking ------------------------------------------------------

# age by smoking variable
smoke %>% 
  ggplot(aes(x = smoking, y = age)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  labs(title = "Age by smoker") +
  xlab("") +
  ylab("Age")

# 4.1) Cholesterol & BMI ~ Gender -----------------------------------------

my_theme <- theme(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(), #remove grids
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  text = element_text(color = "black"),
  plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = ggplot2::margin(b = 10)),
  axis.title.x = element_text(margin = ggplot2::margin(t = 10), size = 10),
  axis.title.y = element_text(margin = ggplot2::margin(r = 10), size = 10)
)

smoke %>% 
  ggplot(aes(x=bmi, y=cholesterol))+
  geom_smooth(aes(linetype = gender, color=gender), se = F)+
  scale_color_manual(values=c("#ff6781", "blue"))+
  scale_linetype_manual(values=c("solid", "solid")) + #in that way also female are solid instead of dashed
  geom_vline(xintercept = 30, linetype = "dashed", color = "black")+ #adds Obese threshold
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fffdd0", alpha = 0.3)+
  labs(title = "Cholesterol levels with BMI per Gender",
       y = "Cholesterol",
       x = "BMI")+
  my_theme

# Smoking has been shown to decrease HDL cholesterol (the "good" cholesterol),
# that's why females have higher levels of cholesterol, 
# because they smoke way less in our dataset

# 4.2) Cholesterol & BMI ~ Smoker -----------------------------------------

smoke %>% 
  ggplot(aes(x=bmi, y=cholesterol))+
  geom_smooth(aes(linetype = smoking, color=smoking), se = F)+
  scale_color_manual(values=c("gray", "#E49633"))+
  scale_linetype_manual(values=c("solid", "solid")) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "black")+ #adds Obese threshold
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fffdd0", alpha = 0.3)+
  labs(title = "Cholesterol levels with BMI per Smoker",
       y = "Cholesterol",
       x = "BMI")+
  my_theme

# 5.1) HDL & BMI ~ Gender -------------------------------------------------

smoke %>% 
  ggplot(aes(x=bmi, y=HDL))+
  geom_smooth(aes(linetype = gender, color=gender), se = F)+
  scale_color_manual(values=c("#ff6781", "blue"))+
  scale_linetype_manual(values=c("solid", "solid")) + #in that way also female are solid instead of dashed
  geom_vline(xintercept = 30, linetype = "dashed", color = "black")+ #adds Obese threshold
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fffdd0", alpha = 0.3)+
  labs(title = "HDL levels with BMI per Gender",
       y = "HDL",
       x = "BMI")+
  my_theme

# HDL levels are higher for the females, because they smoke less,
# and they are inversely proportional with BMI

# 5.2) HDL & BMI ~ Smoker -------------------------------------------------

smoke %>% 
  ggplot(aes(x=bmi, y=HDL))+
  geom_smooth(aes(linetype = smoking, color=smoking), se = F)+
  scale_color_manual(values=c("gray", "#E49633"))+
  scale_linetype_manual(values=c("solid", "solid")) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "black")+ #adds Obese threshold
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fffdd0", alpha = 0.3)+
  labs(title = "HDL levels with BMI per Smoker",
       y = "HDL",
       x = "BMI")+
  my_theme

# Smoking can reduce the amount of HDL (high-density lipoprotein) cholesterol,
# or "good" cholesterol, which helps remove excess cholesterol from the bloodstream.

# 6) Triglycerides & BMI --------------------------------------------------

cor(smoke$triglycerides, smoke$bmi)

# 6.1) Triglycerides & BMI ~ Gender --------------------------------------------------

# triglycerides by smoking variable
smoke %>% 
  ggplot(aes(x = smoking, y = triglycerides)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  labs(title = "Triglycerides by smoker") +
  xlab("") +
  ylab("triglycerides")

smoke %>% 
  ggplot(aes(x=bmi, y=triglycerides))+
  geom_smooth(aes(linetype = gender, color=gender), se = F)+
  scale_color_manual(values=c("#ff6781", "blue"))+
  scale_linetype_manual(values=c("solid", "solid")) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "black")+ #adds Obese threshold
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fffdd0", alpha = 0.3)+
  labs(title = "Triglycerides levels with BMI per Gender",
       y = "Triglycerides",
       x = "BMI")+
  my_theme

# 6.2) Triglycerides & BMI ~ Smoker --------------------------------------------------

smoke %>% 
  ggplot(aes(x=bmi, y=triglycerides))+
  geom_smooth(aes(linetype = smoking, color=smoking), se = F)+
  scale_color_manual(values=c("gray", "#E49633"))+
  scale_linetype_manual(values=c("solid", "solid")) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "black")+ #adds Obese threshold
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fffdd0", alpha = 0.3)+
  labs(title = "Triglycerides levels with BMI per Smoker",
       y = "Triglycerides",
       x = "BMI")+
  my_theme

# As this graph shows, smoking can lead to changes in the body's metabolism, which can result in higher levels of triglycerides. 
# High levels of triglycerides are associated with an increased risk of heart disease, stroke, and other health problems. 
# Smoking has been shown to decrease HDL cholesterol (the "good" cholesterol) and increase triglycerides, which can contribute 
# to the development of cardiovascular disease. This is one of the reasons why smokers have an increased risk of heart attacks, 
# strokes, and other cardiovascular problems compared to non-smokers. Additionally, smoking can also lead to high blood pressure 
# and damage to the lining of the arteries, which can further increase the risk of cardiovascular disease.


# 7) Tartar ------------------------------------------------------------------

smoke %>% 
  ggplot(aes(x = smoking, fill = tartar)) +
  geom_bar(position = "dodge", 
           alpha = 0.8, 
           width = 0.5,
           show.legend = T) +
  scale_fill_manual(values=c("grey", "red")) +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + #remove grids
  labs(title = "NON Smokers & Smokers per tartar",
       y = "count",
       x = "")


# 8) Waist ----------------------------------------------------------------

smoke %>% 
  ggplot(aes(x = smoking, y = waist)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  labs(title = "waist by smoker") +
  xlab("") +
  ylab("waist")


# 9) Serum Creatinine & BMI -----------------------------------------------

smoke %>% 
  rename(Smoke=smoking) %>% 
  ggplot(aes(x=bmi, y=serum_creatinine))+
  geom_smooth(aes(linetype = Smoke, color=Smoke), se = F)+
  scale_color_manual(values=c("grey", "black"))+
  scale_linetype_manual(values=c("solid", "solid")) + #in that way also female are solid instead of dashed
  labs(title = "Serum Cr. & BMI by Smoker",
       y = "Serum Cr.",
       x = "BMI",
       fill="Smoke"
  )+
  theme_classic()


# 10) Hb ----------------------------------------------------------------------
smoke %>% 
  mutate(Smoke=smoking) %>% 
  ggplot(aes(x = Smoke, y = Hb)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  labs(title = "Smokers by Hemoglobin ") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size= 10),
        axis.title.y = element_text(size = 8),
        panel.border = element_rect(fill = NA, color = "black", linewidth =1)
  ) + 
  xlab("") +
  ylab("Hb")


# 10.1) Hb and BMI ----------------------------------------------------------

smoke %>% 
  rename(Smoke=smoking) %>% 
  ggplot(aes(x=bmi, y=Hb))+
  geom_smooth(aes(linetype = Smoke, color=Smoke), se = F)+
  scale_color_manual(values=c("gray", "black"))+
  scale_linetype_manual(values=c("solid", "solid")) +
  labs(title = "Hemobling & BMI by Smoker",
       y = "Hb",
       x = "BMI")+
  theme_classic()

# MODELLING ------------------------------------------------------

# Train\Test split --------------------------------------------------------
train_setup <- sample(nrow(smoke), round(0.7 * nrow(smoke)))

train <- smoke[train_setup,]
test <- smoke[-train_setup,]
nrow(train)
nrow(test)

table(train$smoking)
table(test$smoking)


# 1) GLM -------------------------------------------------------

# ~ gender -----------------------------------------------

fit1 <- glm(smoking ~ gender, data = train, family = "binomial")
summary(fit1)
calculate_metrics(fit1, test) # acc = 0.700

# ~ gender + age^2 ---------------------------------------------------------------

fit2 <- glm(smoking ~ gender + I(age^2),
            data = train, family = "binomial")
summary(fit2) 
calculate_metrics(fit2, test) # acc = 0.708

# ~ gender + age^2 + triglycerides ---------------------------------------------------------------

fit3 <- glm(smoking ~ gender + I(age^2) + triglycerides,
            data = train, family = "binomial")
summary(fit3) 
calculate_metrics(fit3, test) # acc = 0.721

# ~ gender + age^2 + triglycerides + serum_creatinine ---------------------------------------------------------------

fit4 <- glm(smoking ~ gender + I(age^2) + triglycerides +
              serum_creatinine,
            data = train, family = "binomial")
summary(fit4) 
calculate_metrics(fit4, test) # 0.725

# ~ gender + age^2 + triglycerides + serum_creatinine + GTP ---------------------------------------------------------------

fit5 <- glm(smoking ~ gender + I(age^2) + triglycerides +
              serum_creatinine + GTP,
            data = train, family = "binomial")
summary(fit5) 
calculate_metrics(fit5, test) # acc = 0.731

# ~ gender + age^2 + triglycerides + serum_creatinine + GTP ---------------------------------------------------------------
#   + tartar

fit6 <- glm(smoking ~ gender + I(age^2) + triglycerides +
              serum_creatinine + GTP + tartar ,
            data = train, family = "binomial")
summary(fit6) 
calculate_metrics(fit6, test) # acc = 0.738

# ~ gender + age^2 + triglycerides + serum_creatinine + GTP ---------------------------------------------------------------
#   + tartar + waist

fit6 <- glm(smoking ~ gender + I(age^2) + triglycerides +
              serum_creatinine + GTP + tartar + waist,
            data = train, family = "binomial")
summary(fit6) 
calculate_metrics(fit6, test) # acc = 0.739

# ~ gender + age^2 + triglycerides + serum_creatinine + GTP ---------------------------------------------------------------
#   + tartar + HDL:bmi_status

fit7 <- glm(smoking ~ gender + I(age^2) + triglycerides +
              serum_creatinine + GTP + tartar + waist + 
              dental_caries:age,
            data = train, family = "binomial")
summary(fit7) 
calculate_metrics(fit7, test) # acc = 0.742

# ~ gender + age^2 + triglycerides + serum_creatinine + GTP ---------------------------------------------------------------
#   + tartar + waist + dental_caries:age + pressure_S:age

fit8 <- glm(smoking ~ gender + I(age^2) + triglycerides +
              serum_creatinine + GTP + tartar + waist + 
              dental_caries:age + 
              pressure_S:age,
            data = train, family = "binomial")
summary(fit8) 
calculate_metrics(fit8, test) # acc = 0.744

# Final Model -------------------------------------------------------------

fit <- glm(smoking ~ 
             gender:age + pressure_S:age +
             I(age^2) + cholesterol + 
             triglycerides + Hb + HDL +
             serum_creatinine + tartar + GTP +
             dental_caries + bmi_status:Hb +
             serum_creatinine:bmi  
           , family = binomial, data = train)
summary(fit)
calculate_metrics(fit, test) # acc = 0.749

# 2) Lasso ----------------------------------------------------------------

# We have done variable selection with the lasso
# framework as measure of comparison with our logit models

# train and test preparation
x_train <- model.matrix(smoking ~ ., train)
x_test  <- model.matrix(smoking ~ ., test)
y_train <- as.integer(as.character(train$smoking))
y_test  <- as.integer(as.character(test$smoking))

# lasso model
lasso <- glmnet(x_train, y_train, family = "binomial")

# lasso cross validation
cv.lasso         <- cv.glmnet(x_train, y_train)
best_lambda      <- cv.lasso$lambda.min

# best model coefficients
coef_fit <- coef(lasso, s = best_lambda)
coef_fit

# plots
par(mfrow = c(1,2))
plot(lasso, xvar="lambda")
plot(lasso, xvar="dev")

# predictions
prediction_lasso <- predict(lasso,
                            s = best_lambda,
                            newx = x_test,
                            type = "response")

# assigning 0,1
# threeshold = 0.5
for (i in 1:length(prediction_lasso)) {
  if (prediction_lasso[i] >= 0.5)  {
    prediction_lasso[i] <- 1
  } else {
    prediction_lasso[i] <- 0
  }
}

# confusion matrix
confusion_matrix <- table(prediction_lasso, test$smoking)

# PERFORMANCE EVALUATION 
# Positive Predictive Value (Precision)
true_negative  <- confusion_matrix[1]
false_negative <- confusion_matrix[2]
false_positive <- confusion_matrix[3]
true_positive  <- confusion_matrix[4]

# ppv = positive predictive values
ppv <- true_positive / (true_positive + false_positive)

# tpr = true positive rate
tpr <- true_positive / (true_positive + false_negative)

# fpr = false positive rate
fpr <- false_positive / (true_negative + false_positive)

# fnr = false negative rate
fnr <- false_negative / (true_positive + false_negative)

# accuracy 
acc <- (true_negative + true_positive)/nrow(test)

# output
output <- paste("Precision:", ppv,
                "\nSensitivity:", tpr,
                "\nFalse positive rate:", fpr,
                "\nFalse_Negative_Rate:", fnr,
                "\nAccuracy:", acc)
cat(output) # Accuracy: 0.744


# 3) RF -------------------------------------------------------------------

# Split the data into predictors (X) and the target variable (y)
# Split the data into training and testing sets

set.seed(1234)

train_indices <- sample(1:nrow(smoke), 0.7 * nrow(smoke))  # 70% for training
train_data <- smoke[train_indices, ]
test_data <- smoke[-train_indices, ]

x_train <- train_data[,-24]
y_train <- train_data[,24]
x_test <- test_data[,-24]
y_test <- test_data[,24]

# Train a random forest model
rf_model <- randomForest(x = x_train, y = y_train)

# Print the details of the trained model
print(rf_model)

# Predictions
predictions <- predict(rf_model, newdata = x_test)

# Evaluate the model's performance
accuracy <- sum(predictions == y_test) / nrow(test)
print(paste("Accuracy:", accuracy)) # 0.82%

# Access variable importance measures
var_importance <- importance(rf_model)
print(var_importance)
print(sort(var_importance[,],decreasing = T))







