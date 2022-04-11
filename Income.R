###### 0. Importing the ALLBUS data ###### 
#install.packages("heaven")
#install.packages("ggolot2")
#install.packages("pscl")
#library(haven)
library(dplyr)
library(ggplot2)
library(pscl)
library(haven)
library(interactions)
data <- read_sav("~/Documents/IE MCSBT/1. Term/Math_Lab/Final_Project_Tombers/ZA5270_v2-0-0.sav")


summarize <- function(variable){
  print("Summary")
  print(summary(variable))
  print("Standarddeviation")
  print(sd(variable))
  print("Correlation with Income")
  print(cor(data$income, variable, use="complete.obs"))
}

###### 1. Wrangle Data###### 
#colnames(data)
### --- 1.1 Income
# Recode
data$income <- data$di01a

# Summarize
summarize(data$income)

# Visualize
ggplot(data,aes(x=income)) +
  geom_histogram(color="darkgreen", fill="lightgreen", bins = 250) + 
  labs(title="Distribution of the Personal Net Income") + 
  xlab("Monthly Net Income in Euro")+
  ylab("Frequency") 

ggplot(data,aes(x=log(income))) +
  geom_histogram(color="darkgreen", fill="lightgreen", bins = 100) + 
  labs(title="Distribution of the log Personal Net Income") + 
  xlab("Monthly Net Income in Euro (log)")+
  ylab("Frequency")

### --- 1.2 Gender
# Recode
data$gender <- data$sex
hist(data$gender)
data$gender[data$gender == 1] <- 0
data$gender[data$gender == 2] <- 1

# Summarize
summarize(data$gender)

# Visualize
ggplot(data = subset(data, !is.na(data$gender)), aes(x=factor(gender))) + 
  geom_bar(fill="lightgreen") + 
  labs(title ="Distribution of the Sexes") + 
  xlab("Gender")+
  ylab("Frequency")+
  scale_x_discrete(labels = c("Male","Female"))


### --- 1.3 Age
# Recode
data$age <- data$age

# Summarize
summarize(data$age)

# Visualize
ggplot(data,aes(x=age)) +
  geom_histogram(color="darkgreen", fill="lightgreen", bins = 77) + 
  labs(title="Distribution of the Age of Participants") +
  ylab("Frequency") +
  xlab("Age in Years")


### --- 1.4 Partnership
# Recode
data$married <- data$mstat
data$married[data$married == 4] <- 0
data$married[data$married == 5] <- 0
data$married[data$married == 9] <- 0
data$married[data$married == 1] <- 1
data$married[data$married == 2] <- 1
data$married[data$married == 6] <- 1
data$married[data$married == 7] <- 1
data$married[data$married == -9] <- NA
data$married[data$married == 3] <- NA
data$married[data$married == 8] <- NA

# Summarize
summarize(data$married)

# Visualize
ggplot(data = subset(data, !is.na(data$married)), aes(x=factor(married))) + 
  geom_bar(fill="lightgreen") + 
  labs(title ="Distribution of the Partnership Status", x="Status") + 
  xlab("")+
  ylab("Frequency")+
  scale_x_discrete(labels = c("Not Married","Married"))


### --- 1.5 Educational Level
# Recode
data$education <- data$educ
data$education[data$education == 1] <- 0
data$education[data$education == 2] <- 0
data$education[data$education == 3] <- 1
data$education[data$education == 4] <- 2
data$education[data$education == 5] <- 2
data$education[data$education == 6] <- NA
data$education[data$education == 7] <- NA
data$education[data$education == -9] <- NA
data$education.cat <- factor(data$education, levels = c(0, 1, 2))
plot(data$education.cat)

data$medium_educ <- data$education
data$medium_educ[data$medium_educ == 2]<- 0

data$high_educ <- data$education
data$high_educ[data$high_educ == 1] <- 0
data$high_educ[data$high_educ ==2] <- 1


# Summarize
summarize(data$education)

# Visualize
ggplot(data = subset(data, !is.na(data$education)), aes(x=factor(education))) + 
  geom_bar(fill="lightgreen") + 
  labs(title ="Distribution over the levels of education", x="Educational Level") + 
  scale_x_discrete(labels = c("Low","Medium","High")) +
  ylab("Frequency")


### --- 1.6 Place of Living
# Recode
data$west_to_east <- data$eastwest
data$west_to_east[data$west_to_east == 1] <- 0
data$west_to_east[data$west_to_east == 2] <- 1

# Summarize
summarize(data$west_to_east)

# Visualize
ggplot(data = subset(data, !is.na(data$west_to_east)), aes(x=factor(west_to_east))) + 
  geom_bar(fill="lightgreen") + 
  labs(title ="Distribution of Region - West or East", x="Region") + 
  scale_x_discrete(labels = c("Western German", "Eastern German")) +
  ylab("Frequency")


###### 2. Observe the Relationships ###### 
### --- 2.1 Correlation Between  Variables and Income
cor(data$income, data$gender, use="complete.obs") 
cor(data$income, data$age, use="complete.obs")
cor(data$income, data$education, use="complete.obs")
cor(data$income, data$west_to_east, use="complete.obs")
cor(data$income, data$married, use="complete.obs")

# Income per Gender
by(log(data$income), data$gender, summary)
ggplot(data, 
       aes(x = factor(gender), y = log(income), fill = factor(gender))) + 
  geom_boxplot()+
  scale_x_discrete(labels= c("Men", "Woman")) +
  theme(legend.position="none") +
  labs(title="Log Personal Income per Gender ",
       x="Gender", 
       y = "Monthly Log Net Income in Euro") + 
  scale_y_continuous()

# Income per Educational Level
by(data$income, data$education, summary)
ggplot(data = subset(data, !is.na(data$education)), 
       aes(x = factor(education), y = log(income), fill = factor(education))) + 
  geom_boxplot()+
  scale_x_discrete(labels= c("Low", "Middle", "High")) +
  theme(legend.position="none") +
  labs(title="Log Personal Income per Educational Level ",
       x="Education Level", 
       y = "Monthly Log Net Income in Euro")+
  scale_y_continuous(limits=c(4,9))

# Income per Region
by(data$income, data$west_to_east, summary)
ggplot(data = subset(data, !is.na(data$west_to_east)), 
       aes(x = factor(west_to_east), y = log(income), fill = factor(west_to_east))) + 
  geom_boxplot()+
  scale_x_discrete(labels= c("Western Germany", "Eastern Germany")) +
  theme(legend.position="none") +
  labs(title="Log Personal Income per Region",
       x="Region", 
       y = "Monthly og Net Income in Euro")+
  scale_y_continuous(limits=c(4,9))

# Income per Relationship Status
by(data$income, data$married, summary)
ggplot(data = subset(data, !is.na(data$married)), 
       aes(x = factor(married), y = log(income), fill = factor(married))) + 
  geom_boxplot()+
  scale_x_discrete(labels= c("Not Married", "Married")) +
  theme(legend.position="none") +
  labs(title="Log Personal Income per Relationship Status",
       x="Marital Status", 
       y = "Monthly Log Net Income in Euro")+
  scale_y_continuous(limits=c(4,9))

# Income and Age
ggplot(data, 
       aes(x = age, y = log(income))) + 
  geom_point() +
  scale_y_continuous()+
  labs(title="Log Personal Net Income and Age", 
       x="Age in Years", 
       y="Log Monthly Net Income")


### --- 2.2 Create Interactions graph for dummies and the categorial variables


###### 3. Regression###### 
## Data Frame with only the needed variables
data_new <- data.frame(income = as.numeric(data$income),
                       age=as.numeric(data$age), 
                       woman = as.factor(data$gender), 
                       married = as.factor(data$married),
                       education = as.factor(data$education.cat),
                       east_germany = as.factor(data$west_to_east),
                       medium_educ = as.factor(data$medium_educ),
                       high_educ = as.factor(data$high_educ)
                       )
### --- 3.1 All variables no interactions
reg1 <- lm(income ~ age + woman  + education + married +east_germany , data=data_new)
summary(reg1)
hist(reg1$residuals, breaks=100, main="Residuals Regression")
plot(reg1)
shapiro.test(reg1$residuals)

reg4 <- lm(income ~ age + woman + education + married + age:married + east_germany, data=data_new)
summary(reg4)
hist(reg4$residuals, breaks=100, main="Residuals Regression")
plot(reg4)
shapiro.test(reg4$residuals)
interact_plot(reg4, age, married)
anova(reg1,reg4)

reg2 <- lm(log(income) ~ age + woman + medium_educ + high_educ + married +east_germany , data=data_new)
summary(reg2)
hist(reg2$residuals, breaks=100, main="Residuals Regression")
plot(reg2)
anova(reg1,reg2)
shapiro.test(reg2$residuals)

reg3 <- lm(log(income) ~ age + woman + medium_educ + high_educ + married +east_germany +age:married , data=data_new)
summary(reg3)
hist(reg3$residuals, breaks=100, main="Residuals Regression")
plot(reg3)

anovav(reg3,reg2)
shapiro.test(reg2$residuals)

library(car)
vif(reg3)

