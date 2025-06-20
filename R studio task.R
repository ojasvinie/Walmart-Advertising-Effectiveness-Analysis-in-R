install.packages("dplyr")
library(dplyr)
install.packages("broom")
library(broom)
install.packages("caTools")
library(caTools)
install.packages("ggplot2")
library(ggplot2)

#load the data set
advertising

# display the head of the dataset

head(advertising)

#check the dimensions

dim(advertising)
summary(advertising)

#Data Visualization

plot(advertising$sales, advertising$TV, type='p', col = "red")
plot(advertising$sales, advertising$radio, type='p', col = "Blue")
plot(advertising$sales, advertising$newspaper, type='p', col = "Green")     

pairs(advertising)

# Correlation analysis
install.packages("corrplot")
library(corrplot)
num.cols<-sapply(advertising, is.numeric)
num.cols
cor.data<-cor(advertising[,num.cols])
cor.data
corrplot(cor.data, method = 'color')

#simple linear regression
model_simple<-lm(sales~TV, data=advertising)
summary(model_simple)


#Multiple linear regression Model

model_multiple<-lm(sales~TV + newspaper + radio, data= advertising)
summary(model_multiple)
tidy(model_multiple)
coef<-summary(model_multiple)$coefficients
coef

#train test split
set.seed(101)
sample<-sample.split(advertising$TV, SplitRatio = 0.70)
train = subset(advertising,sample == TRUE)
test = subset(advertising, sample == FALSE)

#Train model on Training set
model<-lm(sales~ .,train)
summary(model)

#Residual Analysis (actual sales - predicted sales)
res<-residuals(model)
res<-as.data.frame(res)
res

#Predict on Test set
sales.predictions<-predict(model, test)
sales.predictions

#Combine Predictions and actual sales
results <- cbind(sales.prediction, test$sales)
results
colnames(results) <-c('pred','real')
results<-as.data.frame(results)
results

