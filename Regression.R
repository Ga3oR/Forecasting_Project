rm(list = ls())


# Packages Installation ---------------------------------------------------

packages <- c("readxl", "tidyverse", "corrplot","MASS","GGally")

for (package_name in packages) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}

# Loading Packages --------------------------------------------------------

library(readxl)
library(tidyverse)
library(corrplot)
library(MASS)
library(GGally) 
library(quantreg)

# Loading Data ------------------------------------------------------------

getwd()
# setwd("C:/Users/gabri/OneDrive/Desktop/Trapin Project/Regression")
data <- read_xlsx("Regression.xlsx")
head(data)
tail(data)
names(data)

# DATA MANIPULATION ----------------------------------------------------------

str(data)

# 1) smoker ---------------------------------------------------------------

# Converting smoker variable from ("Yes","No") to (1,0)
# and converting class in "factor"

data$smoker[data$smoker == "yes"] <- 1
data$smoker[data$smoker == "no"] <- 0
data$smoker <- factor(data$smoker)
unique(data$smoker)

# 2) sex ------------------------------------------------------------------

# Converting sex variable in "factor"
data$sex <- factor(data$sex)
unique(data$sex)

str(data)

# FEATURE ENGENEERING -----------------------------------------------------

# 1) bmi_status variable --------------------------------------------------

# Splitting bmi variable in classes: 
# (Underweight, Healthy_Weight, Overweight, Obese)

data <- data %>%
  mutate(bmi_status =
           case_when(
             bmi <  18.5 ~ "Underweight",
             bmi >= 18.5 & bmi < 25 ~ "Normal_Weight",
             bmi >= 25 & bmi < 30 ~ "Overweight",
             bmi >= 30 ~ "Obese"))

# Converting bmi_status in factor

class(data$bmi_status)
data$bmi_status <- factor(data$bmi_status)
unique(data$bmi_status)
str(data)

# CORRELATION -------------------------------------------------------------

# 1) Correlation Plot -----------------------------------------------------

# Calculating correlations only on the numeric variables

correlation_matrix <- data %>%
  keep(is.numeric) %>%
  cor()

# Plotting correlations

corrplot(correlation_matrix)

data %>%
  ggpairs()

# Dependent Variable = Charges 
# Independent Variable = Smoker
table(data$smoker)

# Calculating pearson correlation between charges (Y) and qualitative
# variables (Xi) with the cor.test() function. 

cor.test(data$charges, as.numeric(factor(data$smoker)), method = "pearson")
cor.test(data$charges, as.numeric(factor(data$sex)), method = "pearson")
cor.test(data$charges, as.numeric(factor(data$children)), method = "pearson")

# cor(charges | smoker)   = 0.7872514
# cor(charges | sex)      = 0.0572920
# cor(charges | children) = 0.0679982

# DATA EXPLORATION --------------------------------------------------------

# To follow we'll plot visualization of each variable

# 1) Charges --------------------------------------------------------------

data %>% 
  ggplot(aes(x = charges, y = after_stat(density))) +
  geom_histogram(bins = 40, color = "white", fill = "gray") +
  geom_vline(aes(xintercept = mean(charges)), color = "#000000",
             linewidth = 0.5, linetype = "dashed")+
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Charges Distribution $") +
  xlab("") +
  ylab("Density")

# 1.1) Log-Charges --------------------------------------------------------

# We see that "Charges" variable is strongly right skewed.
min(data$charges)
max(data$charges)
mean(data$charges)
sd(data$charges)

# Could a log transformation fix it? Let's see:
data$log_charges <- log(data$charges)

data %>% 
  ggplot(aes(x = log_charges, y = after_stat(density))) +
  geom_histogram(bins = 40, color = "white", fill = "gray") +
  geom_vline(aes(xintercept = mean(log_charges)), color = "#000000",
             linewidth = 0.5, linetype = "dashed") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Log-Charges Distribution") +
  xlab("") +
  ylab("Density")

# It seems still far from a normal distribution.

# 2) Smoker ---------------------------------------------------------------

data %>% 
  ggplot(aes(x = smoker)) +
  geom_bar(fill = c("darkgreen","black"),
           alpha = 0.7, 
           width = 0.6) +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + #remove grids
  labs(title = "Sample distribution by smoker") +
  xlab("")

# 2.1) Smoker | Charges ---------------------------------------------------

data %>% 
  ggplot(aes(x = smoker, y = charges)) +
  geom_boxplot(outlier.colour = "red") +
  scale_x_discrete(labels=c("Non Smokers", "Smokers"))+
  labs(title = "Charges by smoker") +
  xlab("") +
  ylab("Charges")

# 2.2) Smoker | Charges | Age ---------------------------------------------

data %>% 
  ggplot(aes(x = age,
             y = charges,                
             color = smoker)) +
  geom_point(size = 1) +
  labs(title = "Charges of smokers by age")

# 2.3) Smoker | Charges | BMI ---------------------------------------------

data %>% 
  ggplot(aes(x=smoker,y=charges))+
  geom_boxplot()+
  facet_wrap(~data$bmi_status)+
  scale_x_discrete(labels=c("Non Smokers", "Smokers")) +
  xlab("")

# We notice strong relation between obese and smokers, 
# we will try to put it into the future model.

# 3) Bmi ------------------------------------------------------------------

data %>% 
  ggplot(aes(x = bmi, y = after_stat(density))) +
  geom_histogram(bins = 40, color = "white", fill = "gray") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Bmi Distribution") +
  xlab("") +
  ylab("Density")

# # 3.1) Bmi_status | Charges ---------------------------------------------

data %>% 
  mutate(bmi_status = fct_reorder(bmi_status, bmi)) %>%  # reorder from Underweight to Obese
  ggplot(aes(x = bmi_status, y = charges)) +
  geom_boxplot() +
  labs(title = "Charges by bmi status") +
  xlab("") +
  ylab("Charges $")

# outliers in obese class relate to the fact that bmi is a function 
# opened to the right [0, + infinite]

# 4) Age ------------------------------------------------------------------

data %>% 
  ggplot(aes(x = age, y = after_stat(density))) +
  geom_histogram(bins = 10, color = "white", fill = "gray") +
  geom_density(color = "red", linewidth = 0.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Population Distribution by age") +
  xlab("Age") +
  ylab("")


# 5) Children -------------------------------------------------------------

data %>%
  ggplot(aes(x = factor(children), y = charges)) +
  geom_boxplot(color = "black", fill = "white") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Distribution of Charges by Children Under Insurance",
       x = "# of children under insurance",
       y = "Charges") 


# 6) Sex ------------------------------------------------------------------

data %>% 
  ggplot(aes(x=sex,y=charges))+
  geom_boxplot()+
  labs(title = "Distribution of Charges by Sex",
       x = "",
       y = "Charges") +
  scale_x_discrete(labels=c("Females", "Males")) +


# MODELLING SECTION -------------------------------------------------------

# Splitting into train and test -------------------------------------------

# We arbitrarily decide to take 80% of observations for train
# and 20% for test

set.seed(69)
train_setup <- sample(nrow(data), round(0.8 * nrow(data)))

# Train
train <- data[train_setup,]
# Test
test <- data[-train_setup,]

nrow(train)
nrow(test)

# LINEAR MODEL: -----------------------------------------------------------------

# Let's start from adding variables one by one, choosing
# keeping in consideration correlations calculated above

# 1) smoker ---------------------------------------------------------------

fit1 <- lm(charges ~ smoker, data = train)
summary(fit1)
summary(fit1)$adj.r.squared
pred1 <- predict(fit1, test)
mean((test$charges - pred1)^2) #49031609
plot(density(residuals(fit1)))

# 2) smoker + bmi ---------------------------------------------------------

fit2 <- lm(charges ~ smoker + bmi, data = train)
summary(fit2)
summary(fit2)$adj.r.squared
pred2 <- predict(fit2, test)
mean((test$charges - pred2)^2) #42877029
plot(density(residuals(fit2)))

# 3) smoker + bmi + age ---------------------------------------------------

fit3 <- lm(charges ~ smoker + bmi + age, data = train)
summary(fit3)
summary(fit3)$adj.r.squared
pred3 <- predict(fit3, test)
mean((test$charges - pred3)^2) #31257949
plot(density(residuals(fit3)))

# 4) smoker + bmi + age + children ----------------------------------------

fit4 <- lm(charges ~ smoker + bmi + age + children, data = train)
summary(fit4)
summary(fit4)$adj.r.squared
pred4 <- predict(fit4, test)
mean((test$charges - pred4)^2) #31128455
plot(density(residuals(fit4)))

# 5) smoker + bmi + age + children + sex ----------------------------------

fit5 <- lm(charges ~ smoker + bmi + age + 
            children + sex, data = train)
summary(fit5)
summary(fit5)$adj.r.squared
pred5 <- predict(fit5, test)
mean((test$charges - pred5)^2) #31165024
plot(density(residuals(fit5)))

# MSE increase whit sex variable, so because it
# is both uncorrelated with charges and non significant,
# we will not add it anymore in the models
# (also no interaction effects between sex and other variables 
# tested in some trials don't seem to be effective)

# 6) smoker + bmi + age + children + bmi_status ---------------------------

fit6 <- lm(charges ~ smoker + bmi + age + 
            children + bmi_status, data = train)
summary(fit6)
summary(fit6)$adj.r.squared
pred6 <- predict(fit6, test)
mean((test$charges - pred6)^2) #29865205
plot(density(residuals(fit6)))

# It seems that the only bmi_status that has a significant impact
# for the predictions is the "Obese", so we can try to extract
# that category and put it in a new model. We also saw from
# the data exploration that the interaction between smokers 
# and obese seemed relevant) 

# 7) smoker + bmi + age + children + bmi_obese ----------------------------

# We create another variable only with the obese units.

data$bmi_obese <- ifelse(data$bmi_status == "Obese", 1, 0)

# We can allow to reset train and test because we set a seed above.

train <- data[train_setup,]
test <- data[-train_setup,]

fit7 <- lm(charges ~ smoker + bmi + age + 
            children + bmi_obese, data = train)
summary(fit7)
summary(fit7)$adj.r.squared
pred7 <- predict(fit7, test)
mean((test$charges - pred7)^2) #29790417
plot(density(residuals(fit7)))

# adding interactions: -------------------------------------------------

# 8) bmi_obese & smoker ---------------------------------------------------

# Reminder : when adding interaction with the ":", we can avoid 
# writing again the single variables each time because they are 
# already included in the model

fit8 <- lm(charges ~ smoker*bmi_obese +bmi + age + 
            children , data = train)
summary(fit8)
summary(fit8)$adj.r.squared
pred8 <- predict(fit8, test)
mean((test$charges - pred8)^2) #19393878
plot(density(residuals(fit8)))
plot(fit8, which = 2) 
#{or}
qqnorm(resid(fit8))
qqline(resid(fit8))

# We see that residuals have mean = 0 but the right tail 
# is really long, this is due to the skewness of charges

# 9) age^2 ----------------------------------------------------------------

# Let's try to add a square term for the age variable,
# so that we highlight people who are older.
# We suspect it may be good by the graphs above;
# (in fact it seems to be an increment in age)

fit9 <- lm(charges ~ smoker*bmi_obese + bmi + I(age^2)  + 
            children , data = train)
summary(fit9)
summary(fit9)$adj.r.squared    
pred9 <- predict(fit9, test)
mean((test$charges - pred9)^2) #19121561
plot(density(residuals(fit9)))

# This is the lowest mse that we managed to obtain 
# with a Gaussian linear regression trying probably
# every interaction that our data can afford.

# Outlier Analysis

# We then proceed to check for potential outliers
# that we believe they may not belong to the DGP. 

# Extract standardized residuals
std_resid <- rstandard(fit9)

# Create a boxplot of standardized residuals
boxplot(std_resid, 
        main = "Boxplot of Standardized Residuals")

# Identify potential outliers
outliers <- which(abs(std_resid) > 2.5)
if (length(outliers) > 0) {
  cat("Potential outliers at observation(s):",
      paste(outliers, collapse = ", "))
}

# By roughly checking the units highlighted above,
# it seems that the outliers are just plausible extreme values,
# so we decided not to remove any of them. 

# GENERALIZED LINEAR MODEL ------------------------------------------------

# After trying any possible combination of predictors with the
# linear model setting, we proceed to calculate new predictions
# with the family of Generalized Linear Model. This is due
# to the asymmetric distribution of charges, that we want
# to try to fit at best.

# 1) Inverse Gaussian --------------------------------------------------------

# The Inverse Gaussian function, also known as the Wald distribution,
# is commonly used to model non-negative positively skewed data, so
# it seems appropriate to fit our data.

fit_ig <- glm(charges ~ smoker*bmi_obese + bmi + I(age^2)  + 
             children, data = train, family = inverse.gaussian(link = "identity"))
summary(fit_ig)
pred_ig <- predict(fit_ig, test)
mean((test$charges - pred_ig)^2) # 19119742 < 19121561 (mse_fit9)
plot(density(residuals(fit_ig))) 
res <- residuals(fit_ig)

# There is a slight improvement in the mse, even if some parameters
# result now non-significant. We decide to keep them as they are
# because they reflect the variable that we put in our previous model,
# and also because otherwise the MSE would increase drastically.
# We have also tried other different families that are available
# in the glm() function but we didn't notice any improvement.


# QUANTILE REGRESSION ---------------------------------------------------

# Last but not least is the quantiles calculation.
# We approach this problem in two ways:

# - Using our predictions obtained from the linear regression,
#   making an assumption on the distribution of the data;
# - Resorting to the quantile regression.
#   (see professor Trapin slides)

# Location-scale distribution.
# A location-scale family is a family of distributions parametrized
# by a location parameter and a non-negative scale parameter.
# For any random variable X whose probability distribution function
# belongs to such a family, the distribution of Y said that
# Y = a + bX, also belong to the family
# If x is a continuous random variable, then the probability 
# function of Y results to be the following formula:
# 1/sigma * dt((x - mu)/sigma, df)

# The purpose formula is fitting the residuals of the model

my.dt <- function(x, a, b, df){
  out <- (1/b)*dt((x-a)/b, df = df)
  return(out)
}

# This formula is used to compute the quantile
# of a location scale t student

qt_ls <- function(prob, df, mu, sigma){
  out <- qt(prob, df)*sigma + mu
  return(out)
}

# Recall that our best model is: fit9 
fit9 <- lm(charges ~ smoker*bmi_obese + bmi + I(age^2)  + 
             children , data = train)

# A) Normal 
# Fitting a Normal distribution on the residuals

res <- residuals(fit9)
fit.e <- fitdistr(res, "normal")

plot(density(residuals(fit9)))
lines(seq(-20000,20000, by=1), 
      dnorm(seq(-20000,20000, by=1), 
            fit.e$estimate[1], fit.e$estimate[2]), col=2)
# bad fit

# B) Student t 
# Fitting a Student t distribution on the residuals

# estimate parameter with Maximum Likelihood 
est.t <- fitdistr(fit9$residuals, densfun = "t")
est.t

plot(density(residuals(fit9)))
#gradi di libertà stimati con ML / fitdistr
lines(seq(-20000,20000,by=1), my.dt(x = seq(-20000,20000,by=1), 
                                    a = est.t$estimate[1],
                                    b= est.t$estimate[2],
                                    df = est.t$estimate[3]), col=4, lwd=1)

# 1st approach
# 10th quantile estimates | Lower Quantile Calculation
# with location scale student t  
lb_qt_student <- pred9 + qt_ls(0.1, df = est.t$estimate[3],
                                mu = est.t$estimate[1],
                                sigma = est.t$estimate[2])


# 2nd approach
# 10th quantile | Lower Quantile Calculation
# with quantile regression
lb_qt_regr <- rq(charges ~ smoker*bmi_obese + bmi + I(age^2) + 
             children,
           data = train, 
           tau = 0.1)
summary(lb_qt_regr)
pre_qt_regr <- predict(lb_qt_regr, test)

# Let's see the test values of charges compared with
# the lower quantiles estimated both with scale location t
# and quantile regression

cbind(test$charges, pre_qt_regr, lb_qt_student)
# quantiles with qr seem better (more narrow)

# QUANTILES VALIDATION -------------------------------------------------------------------------

# QUANTILE REGRESSION
# Checking percentage of values under the 10th quantile
i.qrlb <- 1*(test$charges < pre_qt_regr)
mean(i.qrlb)
# 12% of values are lower than the quantiles estimated

# likelihood ratio test (qr)
pv.qrlb <- 1-pchisq(-2*(sum((i.qrlb)*log(0.10)+(1-i.qrlb)*log(0.90))
                        -sum((i.qrlb)*log(mean(i.qrlb))+(1-i.qrlb)*log(1-mean(i.qrlb)))), 1)
pv.qrlb
#we do not reject the H0:p = 0.10, so our estimation with the quantile reg is fine
#p value at 0.22

# USING THE STUDENT T
i.tstud <- 1*(test$charges < lb_qt_student)
mean(i.tstud) 
# ~ 1% values lower than quantiles estimated

# likelihood ratio test (t)
pv.tstud <- 1-pchisq(-2*(sum((i.tstud)*log(0.10)+(1-i.tstud)*log(0.90))
                         -sum((i.tstud)*log(mean(i.tstud))+(1-i.tstud)*log(1-mean(i.tstud)))), 1)
# p value close to zero so we reject the null

# Y TRANSFORMATIONS -------------------------------------------------------

# In the following lines we did a logarithm and a square root 
# transformation to try to understand if it could 
# lead to better results. Summarized, it didn't.

# 1) Logarithm ------------------------------------------------------------

fit <- lm(log(charges) ~ smoker*bmi_obese +bmi + age + 
            children , data = train)
summary(fit)
summary(fit)$adj.r.squared
pred <- predict(fit, test)
mean((test$charges - pred)^2) #279870233
plot(density(residuals(fit)))

# 2) Square root ----------------------------------------------------------

fit <- lm(sqrt(charges) ~ smoker*bmi_obese +bmi + age + 
            children , data = train)
summary(fit)
summary(fit)$adj.r.squared
pred <- predict(fit, test)
mean((test$charges - pred)^2) #276752724
plot(density(residuals(fit)))

