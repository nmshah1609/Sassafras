## libraries
library(tibble)
library(dplyr)
library(ggplot2)
library(psych)
library(normtest)
library(olsrr)
library(randomForest)
library(VSURF)
library(caret)



## Read in data files
ytest = read.table("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/Ytest.txt", 
                   sep =' ', header =T, stringsAsFactors = FALSE)
ytrain = read.table("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/Ytrain.txt", 
                   sep =' ', header =T, stringsAsFactors = FALSE)
xtest = read.table("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/Xtest.txt", 
                   sep =' ', header =T, stringsAsFactors = FALSE)
xtrain = read.table("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/Xtrain.txt", 
                   sep =' ', header =T, stringsAsFactors = FALSE)


## Read in pre processed data 
xtrain_pp = read.csv("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/x_train_preprocessed.csv")
ytrain_pp = read.csv("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/y_train_preprocessed.csv")
xtest_pp = read.csv("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/x_test_preprocessed.csv")


############
## information about the data 
str(xtrain)
str(ytrain)
str(xtest)
summary(ytrain)
summary(xtrain)
summary(xtest)




# Imputation for missing values
xtrain2 = xtrain
ytrain2 = ytrain
xtest2 = xtest

for(i in 1:ncol(xtrain2)){
  xtrain2[is.na(xtrain2[,i]), i] = mean(xtrain2[,i], na.rm = TRUE)
}
for(i in 1:ncol(ytrain2)){
  ytrain2[is.na(ytrain2[,i]), i] = mean(ytrain2[,i], na.rm = TRUE)
}
for(i in 1:ncol(xtest2)){
  xtest2[is.na(xtest2[,i]), i] = mean(xtest2[,i], na.rm = TRUE)
}
####################




## RANDOM FORESTS ##

# optimal mtry
mtry_list = data.frame(mtry = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))

tr = trainControl(method = "cv", number = 5)

for(i in 3:dim(ytrain_pp)[2]){
  mtry = train(xtrain_pp[c(1:1000),-c(1,2)], ytrain_pp[c(1:1000),i], method = "rf", trControl = tr)
  mtry_list[i-2,] = mtry$bestTune
}

write.csv(mtry_list,"C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/mtry.csv", 
          row.names = FALSE)
mtry_list2 = read.csv("C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/mtry.csv")



# variable selection
# use prediction step to determine variables
# if prediction step is not reached, use interpretation step
var_list = list()
var_list2 = list()
for(i in c(7,15)) {
  surf = VSURF(xtrain_pp[c(1:3000), -c(1,2)], ytrain_pp[c(1:3000), 15], ntree = 100, mtry = mtry_list[13,])
  var_list[[i-2]] = surf$varselect.pred
  var_list2[[i-2]] = surf$varselect.interp
}

var_list = list(c(17,8,15,13,1,20,27,53),c(24,40,49,17,46,43,20,22,61),c(28,35,27,33,40,41,56),c(35,1,33,37,32),
                c(25,26,44,28,31,15,19),c(25,26,44,28,31,15,19), c(28,57,31,33,35,38,34,30,40,8),
                c(39,31,34,27,35,28,5),c(18,55,34,13,11,59,37,17,1,23,14),c(34,38,2,27,39,35,28,30),
                c(34,38,28,2,27),c(11,44,35,37),c(58,3),c(26,25,58,1,35,27,41,47,59,55,51,56,39,5))


#random sample of 50 000 observations
set.seed(1211)
x = sample(c(1:153287), 50000, replace = FALSE)


# RANDOM FOREST Models
xtrain_pp2 = xtrain_pp[x, -c(1,2)]
ytrain_pp2 = ytrain_pp[x, -c(1,2)]


rf_z01 = randomForest(xtrain_pp2[,var_list[[1]]], ytrain_pp2[,1], ntree = 100, mtry = mtry_list[1,],
                      do.trace = TRUE)
pred_z01 = predict(rf_z01, xtest_pp)

rf_z02 = randomForest(xtrain_pp2[,var_list[[2]]], as.factor(ytrain_pp2[,2]), ntree = 100, mtry = mtry_list[2,],
                      do.trace = TRUE)
pred_z02 = predict(rf_z02, xtest_pp)

rf_z03 = randomForest(xtrain_pp2[,var_list[[3]]], ytrain_pp2[,3], ntree = 100, mtry = mtry_list[3,],
                      do.trace = TRUE)
pred_z03 = predict(rf_z03, xtest_pp)

rf_z04 = randomForest(xtrain_pp2[,var_list[[4]]], ytrain_pp2[,4], ntree = 100, mtry = mtry_list[4,],
                      do.trace = TRUE)
pred_z04 = predict(rf_z04, xtest_pp)

rf_z05 = randomForest(xtrain_pp2[,var_list[[5]]], ytrain_pp2[,5], ntree = 100, mtry = mtry_list[5,],
                      do.trace = TRUE)
pred_z05 = predict(rf_z05, xtest_pp)

rf_z06 = randomForest(xtrain_pp2[,var_list[[6]]], ytrain_pp2[,6], ntree = 100, mtry = mtry_list[6,],
                      do.trace = TRUE)
pred_z06 = predict(rf_z06, xtest_pp)

rf_z07 = randomForest(xtrain_pp2[,var_list[[7]]], ytrain_pp2[,7], ntree = 100, mtry = mtry_list[7,],
                      do.trace = TRUE)
pred_z07 = predict(rf_z07, xtest_pp)

rf_z08 = randomForest(xtrain_pp2[,var_list[[8]]], ytrain_pp2[,8], ntree = 100, mtry = mtry_list[8,],
                      do.trace = TRUE)
pred_z08 = predict(rf_z08, xtest_pp)

rf_z09 = randomForest(xtrain_pp2[,var_list[[9]]], ytrain_pp2[,9], ntree = 100, mtry = mtry_list[9,],
                      do.trace = TRUE)
pred_z09 = predict(rf_z09, xtest_pp)

rf_z10 = randomForest(xtrain_pp2[,var_list[[10]]], ytrain_pp2[,10], ntree = 100, mtry = mtry_list[10,],
                      do.trace = TRUE)
pred_z10 = predict(rf_z10, xtest_pp)

rf_z11 = randomForest(xtrain_pp2[,var_list[[11]]], ytrain_pp2[,11], ntree = 100, mtry = mtry_list[11,],
                      do.trace = TRUE)
pred_z11 = predict(rf_z11, xtest_pp)

rf_z12 = randomForest(xtrain_pp2[,var_list[[12]]], ytrain_pp2[,12], ntree = 100, mtry = mtry_list[12,],
                      do.trace = TRUE)
pred_z12 = predict(rf_z12, xtest_pp)

rf_z13 = randomForest(xtrain_pp2[,var_list[[13]]], ytrain_pp2[,13], ntree = 100, mtry = mtry_list[13,],
                      do.trace = TRUE)
pred_z13 = predict(rf_z13, xtest_pp)

rf_z14 = randomForest(xtrain_pp2[,var_list[[14]]], ytrain_pp2[,14], ntree = 100, mtry = mtry_list[14,],
                      do.trace = TRUE)
pred_z14 = predict(rf_z14, xtest_pp)


pred_df = data.frame(pred_z01, pred_z02, pred_z03, pred_z04, pred_z05, pred_z06, pred_z07, pred_z08,
                     pred_z09, pred_z10, pred_z11, pred_z12, pred_z13, pred_z14)

write.csv(pred_df ,"C:/Users/Ryker/Desktop/SFU Class Files/2020/SFU Fall 2020/STAT 440/Modules/Module 2/prediction.csv", 
          row.names = FALSE)
