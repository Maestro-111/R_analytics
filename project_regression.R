library(ggplot2) # Data visualization
library(lubridate)
library(corrgram) # for cor table
library(corrplot) # for correlation plot


Covid_dataset = read.csv("C:/r_files/worldwide-aggregate.csv") # note that path could be different
#Covid_dataset = read.csv("worldwide-aggregate.csv") # note that path could be different

#set.seed(1)

View(Covid_dataset)


summary(Covid_dataset)#summary of data set

str(Covid_dataset)#structure of data set

head(Covid_dataset)#display the first 6 objects

dim(Covid_dataset) # dimension 


# Data Procession 
Covid_dataset = Covid_dataset[-c(1)]



table(is.na(Covid_dataset))

Covid_dataset = na.omit(Covid_dataset)
table(is.na(Covid_dataset))


for (i in 1:4){
  some_data = Covid_dataset[i]
  numeric_data = as.numeric(unlist(some_data))
  Q1 = quantile(numeric_data, .25)
  Q3 = quantile(numeric_data, .75)
  IQR = IQR(numeric_data)
  Covid_dataset = subset(Covid_dataset, numeric_data > (Q1 - 1.5*IQR) & numeric_data < (Q3 + 1.5*IQR)) # delete those rows that have values less or more than corresponding bounds
  print(i) 
  print(dim(Covid_dataset)) # keep track of dimension
  
}

rownames(Covid_dataset) = 1:nrow(Covid_dataset)


View(Covid_dataset)

# plotting variables

boxplot(Covid_dataset$Confirmed,Covid_dataset$Recovered,Covid_dataset$Deaths, names = c("Confirmed", "Recovered", "Deaths"), col = "orange")

plot(Covid_dataset$Confirmed, Covid_dataset$Increase.rate) # negative correlatiob for all
plot(Covid_dataset$Recovered, Covid_dataset$Increase.rate)
plot(Covid_dataset$Deaths, Covid_dataset$Increase.rate)



boxplot(Covid_dataset$Confirmed, xlab="Confirmed cases")
boxplot(Covid_dataset$Recovered, xlab="Recovered cases")
boxplot(Covid_dataset$Deaths, xlab="Death cases")
boxplot(Covid_dataset$Increase.rate, xlab="Increase Rate")


# additionally, histograms
hist(Covid_dataset$Confirmed)
hist(Covid_dataset$Recovered)
hist(Covid_dataset$Deaths)

# dividing into training and testing
indexes = sample(1:nrow(Covid_dataset), size = 0.2*nrow(Covid_dataset))
Test_data = Covid_dataset[indexes,] #Test data set 20%
Train_data = Covid_dataset[-indexes,] #Train data set 80%

# cor analysis
Attribute_Corr = cor(Train_data[1:4], method="pearson")
Attribute_Corr
corrplot(Attribute_Corr,type="upper",method="number",addCoef.col = "black", title = "Correlation between the attributes")

# modeling
model =  lm(log(Increase.rate) ~ Confirmed+Recovered+Deaths,data = Train_data) # all 3
summary(model)
hist(model$residuals) # close to normal
plot(model$fitted.values, model$residuals) # no relationship
print(mean(model$residuals)) # mean is zero


model =  lm(log(Increase.rate) ~ Confirmed+Deaths,data = Train_data)#, alternative due to high p value and corresponding t-test, F test will increase and R^2 is going to be decreased slightly(very)
summary(model)
hist(model$residuals)
plot(model$fitted.values, model$residuals) 
print(mean(model$residuals)) 



model =  lm(log(Increase.rate) ~ Recovered+Deaths,data = Train_data) # optimal
summary(model)# Multiple R-Squared : 0.88
hist(model$residuals)
plot(model$fitted.values, model$residuals) 
print(mean(model$residuals))



# predicting
pred = predict(model,Test_data)
result = (cbind("Confirmed"=Test_data$Confirmed,"Recovered"=Test_data$Recovered,"Deaths" = Test_data$Deaths,"Original_Rate" = Test_data$Increase.rate,"New_predicted_Rate"=exp(pred))) ##display the original price and predicted price
head(result)

res = as.data.frame(result)
#res data frame of predicted results

plot(Test_data$Increase.rate,res$New_predicted_Rate)
cor(Test_data$Increase.rate,res$New_predicted_Rate)


