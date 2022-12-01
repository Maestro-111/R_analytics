#install.packages('caret')
library(class)
library(caret) # for confusion matrix
data = read.csv("C:/r_files/weatherAUS.csv")
View(data)

dim(data)

table(data[18]) # is not balanced...

data = data[-c(1,2,8,10,11)] # we don't need this columns

table(is.na(data))

data = na.omit(data)

table(is.na(data))



f = function(x){ #  to convert to binary format RainToday column
  ifelse(x == "Yes",1,0)
}

data['RainToday'] = lapply(data['RainToday'], f)

inds = c() # column indices 
for (i in 2:length(data)-1){
  print(i)
  inds = append(inds, i)
}

normalize = function(x) { # we have to normalize data too
  return ((x - min(x)) / (max(x) - min(x))) 
}


for (i in 1:16){ # up to the RainTOday
  some_data = data[i]
  numeric_data = as.numeric(unlist(some_data))
  Q1 = quantile(numeric_data, .25)
  Q3 = quantile(numeric_data, .75)
  IQR = IQR(numeric_data)
  data = subset(data, numeric_data > (Q1 - 1.5*IQR) & numeric_data < (Q3 + 1.5*IQR)) # delete those rows that have values less or more than corresponding bounds
  print(i) 
  print(dim(data)) # keep track of dimension

}

dim(data)


rownames(data) = 1:nrow(data)


data_norm = as.data.frame(lapply(data[,inds], normalize)) #apply to all column in inds, where inds is a column indexes list
View(data_norm)

set.seed(12)

indexes = sample(1:nrow(data), size = 0.8*nrow(data)) # 80% of training and 20% of testing 

tr = data_norm[indexes,]
ts = data_norm[-indexes,]
target_category = data[indexes,18] # we will separate target column to specify it as a parameter "cl" in algorithm
test_category = data[-indexes,18] # delete target column from test data set




accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100} # to calculate accuracy

neighbor = c()
ac = c()

for (probe in 5:20){ # we will chose the best k, but for now we have to check many variants of k
  pr = knn(tr,ts, cl=target_category,k=probe)
  tab = table(pr,test_category)
  neighbor = append(neighbor, probe)
  ac = append(ac, accuracy(tab))
  print(accuracy(tab))
}

#plot(ac, neighbor)
plot(neighbor, ac) # plot the results

pr = knn(tr,ts, cl=target_category,k=9) # optimal
tab = table(pr,test_category)
accuracy(tab)

confusionMatrix(table(pr,test_category)) # more detailed confusion matrix





