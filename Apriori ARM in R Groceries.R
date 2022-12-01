#Implementing Market Basket Analysis using Apriori Algorithm

#read transactions
df_groceries <- read.csv("C:/r_files/gros/GC.csv")
str(df_groceries)
#to see the data
View(df_groceries)


df_sorted <- df_groceries[order(df_groceries$Member_number),]
View(df_sorted)


#convert member number to numeric:
df_sorted$Member_number <- as.numeric(df_sorted$Member_number)


#convert item description to categorical format:

df_sorted$itemDescription <- as.factor(df_sorted$itemDescription)
#to see a summary of the dataset:
str(df_sorted)
View(df_sorted)
#convert dataframe to transaction format using ddply; 

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
#to change the default library folder:





#group all the items that were bought together; by the same customer on the same date
library(plyr)
df_itemList <- ddply(df_groceries, c("Member_number","Date"), 
                     function(df1)paste(df1$itemDescription,collapse = ","))

df_itemList

#remove member number and date
df_itemList$Member_number <- NULL
df_itemList$Date <- NULL



colnames(df_itemList) <- c("itemList")



#write to csv format
write.csv(df_itemList,"C:/r_files/ItemList.csv", quote = FALSE, row.names = TRUE)

#-------------------- association rule mining algorithm : apriori -------------------------#

#load package required
install.packages("arules")
library(arules)

#convert csv file to basket format
txn = read.transactions(file="C:/r_files/ItemList.csv", 
                        rm.duplicates= FALSE, format="basket",sep=",",cols=1);
View(txn)

#remove quotes from transactions
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)


#run apriori algorithm
basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.001, conf = 0.01, target="rules"))
#basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.00001, conf = 0.01, target="rules"),appearance = list(lhs = "CLEMENTINES")))

#check if tm is attched; if yes then detach
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}

#view rules
inspect(basket_rules)
typeof(basket_rules)

for (i in 1:length(basket_rules)){
  print(basket_rules['support'])
}

#convert to datframe and view; optional
df_basket <- as(basket_rules,"data.frame")
df_basket$confidence <- df_basket$confidence * 100
df_basket$support <- df_basket$support * 100
c10_final = c()
c1_support = c()
c2_confidence = c()
c3_lift = c()
c4_rules = c()
c6_random = c()
rul1_left = c()
rul2_right = c()

for (i in df_basket$support){
  if (i > 0.01){
    c1_support = append(c1_support, i)
  }
}

for (i in df_basket$confidence){
  if (i > 0.13){
    c2_confidence = append(c2_confidence,i)
  }
}


for (i in df_basket$lift){
  if (i > 1){
    c3_lift = append(c3_lift,i)
  }
}

for (i in 1:142){ # for all 142 trans
  some = df_basket$rules$lhs[i]
  rul1_left = append(rul1_left, some)
}

for (i in 1:142){
  some = df_basket$rules$rhs[i]
  rul2_right = append(rul2_right, some)
}

for (i in 1:length(rul1_left)){
  if (df_basket$support[i] %in% c1_support || df_basket$confidence[i] %in% c2_confidence || df_basket$lift[i] %in% c3_lift){
    c4_rules = append(c4_rules, paste(rul1_left[i], rul2_right[i]))
  }
  
}

c5_nums = c(runif(5, 1, length(c4_rules))) # 5 random nums

for (i in 1:length(c5_nums)){
  c5_nums[i] = round(c5_nums[i])
}

for (i in c5_nums){
  c6_random = append(c6_random, c4_rules[i])
  
}

for (x in c6_random){
  for (y in 1:length(rul1_left)){
    if (x == paste(rul1_left[y], rul2_right[y])){
      a = as.character(df_basket$support[y])
      b = as.character(df_basket$confidence[y])
      c = as.character(df_basket$lift[y])
      c10_final = append(c10_final, paste(x,a,b,c))
    }
    
  }
}

c18 = c(25,21, 28, 99, 41)
# Mining rules for recommendations:

# split lhs and rhs into two columns
install.packages('reshape2')
library(reshape2)
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[[:punct:]]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)

# convert to chracter
df_basket$rules$lhs <- as.character(df_basket$rules$lhs)
df_basket$rules$rhs <- as.character(df_basket$rules$rhs)

library(stringi)
install.packages("dplyr")
library(dplyr)
df_basket$rules %>% filter(stri_detect_fixed(lhs, "yogurt")) %>%  select(rhs)


#plot the rules
install.packages("arulesViz")
library(arulesViz)
plot(basket_rules)
install.packages("grid")
library(grid)
c5
set.seed(8000)
plot(basket_rules[c18,], method = "grouped", control = list(k = 5))

plot(basket_rules[c18,], method="graph", control=list(type="items"))
plot(basket_rules[c18,])
plot(basket_rules[c18,],method = "grouped")

plot(basket_rules[c18,], method="paracoord",  control=list(alpha=.5, reorder=TRUE))

itemFrequencyPlot(txn, topN = 5)

plot(basket_rules[c18,],measure=c("support","lift"),shading="confidence",interactive=F)
