
#load and load thee relevent packages

install.packages("party")
install.packages("epitools")
install.packages("ggplot2")
install.packages("GGally")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("RColorBrewer")





library(party)
library(epitools)
library(ggplot2)
library(GGally)
library(tidyverse)
library(corrplot) 

library(RColorBrewer)




#Import the data set into R

setwd("~/assignment2")
diabetes<-read.csv("C:\\Users\\ASUS\\Documents\\assignment2\\diabetes.csv")

#loading  diabetes data set

data<-(diabetes)




#Remove all the null values

diabetes<-na.omit(as.data.frame(diabetes))
summary(diabetes)


dim(diabetes)
str(diabetes)
head(diabetes)
tail(diabetes)

#convert dependent variable (Class.Variable) to factor
diabetes$Class.variable<- as.factor(diabetes$Class.variable)
#build new data set with selected variables
diabetes <- diabetes [, c(1,2,3,4,5,6,7,8,9)]
head(diabetes)
View(diabetes)

#Explore all the variables in the research data set
#cor() function compute the variance of x and the covariance or correlation of x and y if these are vectors. If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y are computed.

str(diabetes)
diabetes_cor<-cor(diabetes[,-9])


#Visualize Correlation Matrix using Correlogram
#Correlogram is a graph of correlation matrix. Useful to highlight the most correlated variables in a data table. In this plot, correlation coefficients are colored according to the value. Correlation matrix can be also reordered according to the degree of association between variables




corrplot(diabetes_cor, type="upper", order="hclust", col=brewer.pal(n=4, 
                                                                name="RdYlBu"))

#The correlation coefficient is interpreted as:
#If ρXY=1, then X and Y are perfectly, positively, linearly correlated.
#If ρXY=−1, then X and Y are perfectly, negatively, linearly correlated.
#If ρXY=0, then X and Y are completely, un-linearly correlated. 
#If ρXY>0, then X and Y are positively, linearly correlated, but not perfectly so.
#If ρXY<0, then X and Y are negatively, linearly correlated, but not perfectly so.

#ggpairs() function is a special form of a function that produces a pairwise comparison of multivariate data. 
#By default, ggpairs() function provides two different comparisons of each pair of columns and displays either the density or count of the respective variable along the diagonal. 
#With different parameter settings, the diagonal can be replaced with the axis values and variable labels.

#Plot 2

ggpairs(data=diabetes, title="CHD data")

#Plot 03

ggpairs(data=diabetes,mapping=aes(color=Class.variable),title="CHD data")



#Plot 04

ggscatmat(data=diabetes, color="Class.variable",alpha=0.8)



#model 1 - Building a linear regression model with Insulin_2H_muU_ml  and TSF_thickness_mm   variables
lm1 <- lm(Insulin_2H_muU_ml ~  TSF_thickness_mm , data = diabetes)
summary(lm1)
ggplot(diabetes, aes(x = Insulin_2H_muU_ml, y = TSF_thickness_mm )) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red")


#model 2
#Building a linear regression model with TSF_thickness_mm vs  DPF
#Plot linear regression model

lm2 <- lm(TSF_thickness_mm    ~  DPF, data = diabetes)
summary(lm2)


ggplot(diabetes, aes(x = TSF_thickness_mm   , y = DPF )) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

#---------------Building a Logistic regression models---------------

#Logistic regression is a method for modeling binary outcome variables.
#Now we will build a logistic regression model for CHD data, to see whether we can predict a person has CHD for given variables.
#Now we will be specifying our train and validate(test) data from our data. 

#Now we will divide our sample into 70% Training and 30% Validation parts.

pd <- sample(2, nrow(diabetes),replace=TRUE, prob=c(0.7,0.30))
pd
train <- diabetes[pd==1,]
head(train)
validate <- diabetes[pd==2,]
head(validate)

#Lets build a logistic regression model to check whether we can predicta person has CHD for given variables.
#Creating a logistic regression model should look very similar to creating a linear regression model. 
#However, instead of lm() we use glm(). Also, note that we must specify family = "binomial" for a binary classification context.


#Model 3 - CHD and cholesterol

model_glm <- glm(  Class.variable ~ DPF  , data = diabetes, family = "binomial")

#Before making any predictions, let’s briefly examine the model with summary(). 
#Among other things, it is important to see what coefficient values have been estimated for our model.summary(model_glm)

#plot the logistic regression model
diabetes %>%
  mutate(chd = ifelse(Class.variable == "1", 1, 0)) %>%
  ggplot(aes(PlGluConc_2H, chd)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "glm",method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Cholesterol") +
  ylab("Probability of CHD")
#Logistic Regression Model Prediction
#First of all we should understand how to use the predict() function with glm(). 
#In order to return probabilities, we must specify type = "response". 
#As mentioned before, these predicted values are probabliliites, not classifications. 
#We must “manually” convert the probabilities to classifications. Traditionally, a midpoint value such as 0.5 is used to “categorize” the probabilities.

trn_pred <- ifelse(predict(model_glm, type = "response") >0.5, "1", "0")

#Logistic Regression Model Evaluation
#One of the best way evaluate a classification models is to compare the actual response values with the predicted ones using a cross-table, which is often called a confusion matrix. 
#This matrix can be generated with the base table() function.
#Making predictions on the train dataset.

trn_tab <- table(predicted = trn_pred, actual = train$Class.variable)
trn_tab

#Model Evaluation
sum(diag(trn_tab))/sum(trn_tab)
#Making predictions on the validate dataset.

tst_pred <- ifelse(predict(model_glm, newdata = validate, type = 
                             "response") > 0.5, "1", "0")
tst_tab <- table(predicted = tst_pred, actual = validate$Class.variable)

tst_tab
#Model Evaluation
sum(diag(tst_tab))/sum(tst_tab)
# Model 4 - Lets build a logistic regression model to check whether we can predict a person has class.variable for given all the independent variables.
model_glm <- glm(Class.variable ~ ., data = train, family = "binomial")
summary(model_glm)
#We must “manually” convert the probabilities to classifications.
trn_pred <- ifelse(predict(model_glm, type = "response") >0.5, "1", "0")
#Making predictions on the train set.
trn_tab <- table(predicted = trn_pred, actual = train$Class.variable)
trn_tab
#Model Evaluation
sum(diag(trn_tab))/sum(trn_tab)


