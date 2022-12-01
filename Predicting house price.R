# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(lubridate)
library(corrgram)
library(corrplot)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")

# Any results you write to the current directory are saved as output.
HouseDataSet <- read.csv("C:/r_files/Houses sold at KIng County.csv")


#DIPLAY THE OUTLINE OF THE DATA
summary(HouseDataSet)#summary of dataset
str(HouseDataSet)#structure of dataset
class(HouseDataSet)#class of objects
head(HouseDataSet)#display the first 6 objects
tail(HouseDataSet)#display the last 6 objects

##DATA CLEANING
HouseDataSet$date <- as.Date(as.Date(as.character(HouseDataSet$date),"%Y%m%d"))



#sPLITTING THE DATA INTO TRAIN AND TEST DATA SETS
indexes = sample(1:nrow(HouseDataSet), size = 0.3*nrow(HouseDataSet))



Test_data = HouseDataSet[indexes,] #Test dataset 30%
dim(Test_data)# 6483 21
str(Test_data)
Train_data = HouseDataSet[-indexes,] #Train dataset 70%
dim(Train_data)#15130  21
str(Train_data)
table(is.na(Train_data)) # To check whether the data in train dataset is missing or not

#FINDING THE CORRELATION
Attribute_Corr <-cor(Train_data[3:21],method="pearson")

#PLOTTING THE CORRELATION

corrplot(Attribute_Corr,type="upper",method="color",addCoef.col = "black", title = "Correlation between the attributes", number.cex = 14/ncol(Train_data[3:21]), mar=c(1,1,1,1))

##Display the linearity between price and other independent variables

#Price ~ SqftLiving
scatter.smooth(x=Train_data$sqft_living, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ SqftLiving")
#Price ~ bedrooms
scatter.smooth(x=Train_data$bedrooms, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ bedrooms")
#Price ~ bathrooms
scatter.smooth(x=Train_data$bathrooms, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ bathrooms")
#Price ~ Sqf_lot
scatter.smooth(x=Train_data$sqft_lot, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ Sqf_lot")
#Price ~floors
scatter.smooth(x=Train_data$floors, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ floors")
#Price ~ waterfront
scatter.smooth(x=Train_data$waterfront, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ waterfront")
#Price ~ view
scatter.smooth(x=Train_data$view, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ view")
#Price ~ condition
scatter.smooth(x=Train_data$condition, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ condition")
#Price ~ grade
scatter.smooth(x=Train_data$grade, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ grade")
#Price ~ Sqft_above
scatter.smooth(x=Train_data$sqft_above, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ Sqft_above")
#Price ~ sqft_basement
scatter.smooth(x=Train_data$sqft_basement, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ sqft_basement")
#Price ~ yr_built
scatter.smooth(x=Train_data$yr_built, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ yr_built")
#Price ~ yr_renovated
scatter.smooth(x=Train_data$yr_renovated, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ yr_renovated")
#Price ~ zipcode
scatter.smooth(x=Train_data$zipcode, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ zipcode")
#Price ~ lat
scatter.smooth(x=Train_data$lat, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ lat")
#Price ~ long
scatter.smooth(x=Train_data$long, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ long")
#Price ~ SqftLiving15
scatter.smooth(x=Train_data$sqft_living15, y=Train_data$price,lpars =list(col = "red", lwd = 1, lty = 1), main = "Price ~ SqftLiving15")
#Price ~ Sqft_lot15
scatter.smooth(x=Train_data$sqft_lot15, y=Train_data$price,lpars=list(col = "red", lwd = 1, lty = 1), main = "Price ~ Sqft_lot15") 

#creating the Regression model
Housemodel_i1<- lm(log(price) ~ bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+lat+long+sqft_living15+ sqft_lot15, data = Train_data)
summary(Housemodel_i1)# Multiple R-Squared : 0.7684

pred <- predict(Housemodel_i1,Test_data)
summary(pred)
result <- (cbind("ID"=Test_data$id,"Orginal Price"=Test_data$price,"New predicted price"=exp(pred))) ##display the original price and predicted price
head(result)
write.csv(result, file = "OriginalPriceVsPredicted.csv", row.names=FALSE)