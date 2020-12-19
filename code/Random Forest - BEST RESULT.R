## Random forset
library(randomForest)
library(dplyr)

## Training set
X.train.rf = read.table("x_train_preprocessed.csv", sep = ",", header = TRUE)
Y.train.rf = read.table("y_train_preprocessed.csv", sep = ",", header = TRUE)

## Testing set
X.test.rf = read.table("x_test_preprocessed.csv", sep = ",", header = TRUE)
X.test.rf = X.test.rf[,3:64]  ## the first two columns are IDs

Y.test = read.table("Ytest.txt", sep = ",", header = TRUE)
Y.test$new_ID = sub(":.*", "", Y.test$Id)  ## slipt into two columns
Y.test$new_Y = sub(".*Z", "", Y.test$Id)   ## we only need Id in output 

set.seed(4099183,kind = "Mersenne-Twister")
sub.1 = cbind(X.train.rf[,3:64],Z01 =  Y.train.rf[,3])
sub.1 = sub.1 %>% sample_n(15000)
fit.rf.1 = randomForest(Z01 ~ ., data = sub.1,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)

plot(fit.rf.1)
pred.1 = predict(fit.rf.1,X.test.rf)
MAE.1 = mae(sub.1$Z01, pred.1[1:15000])

sub.2 = cbind(X.train.rf[,3:64],Z02 =  Y.train.rf[,4])
sub.2 = sub.2 %>% sample_n(15000)
fit.rf.2 = randomForest(Z02 ~ ., data = sub.2,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.2)
pred.2 = predict(fit.rf.2,X.test.rf)


sub.3 = cbind(X.train.rf[,3:64],Z03 =  Y.train.rf[,5])
sub.3 = sub.3 %>% sample_n(15000)
fit.rf.3 = randomForest(Z03 ~ ., data = sub.3,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.3)
pred.3 = predict(fit.rf.3,X.test.rf)


sub.4 = cbind(X.train.rf[,3:64],Z04 =  Y.train.rf[,6])
sub.4 = sub.4 %>% sample_n(15000)
fit.rf.4 = randomForest(Z04 ~ ., data = sub.4,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.4)
pred.4 = predict(fit.rf.4,X.test.rf)


sub.5 = cbind(X.train.rf[,3:64],Z05 =  Y.train.rf[,7])
sub.5 = sub.5 %>% sample_n(15000)
fit.rf.5 = randomForest(Z05 ~ ., data = sub.5,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.5)
pred.5 = predict(fit.rf.5,X.test.rf)


sub.6 = cbind(X.train.rf[,3:64],Z06 =  Y.train.rf[,8])
sub.6 = sub.6 %>% sample_n(15000)
fit.rf.6 = randomForest(Z06 ~ ., data = sub.6,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.6)
pred.6 = predict(fit.rf.6,X.test.rf)


sub.7 = cbind(X.train.rf[,3:64],Z07 =  Y.train.rf[,9])
sub.7 = sub.7 %>% sample_n(15000)
fit.rf.7 = randomForest(Z07 ~ ., data = sub.7,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.7)
pred.7 = predict(fit.rf.7,X.test.rf)


sub.8 = cbind(X.train.rf[,3:64],Z08 =  Y.train.rf[,10])
sub.8 = sub.8 %>% sample_n(15000)
fit.rf.8 = randomForest(Z08 ~ ., data = sub.8,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.8)
pred.8 = predict(fit.rf.8,X.test.rf)


sub.9 = cbind(X.train.rf[,3:64],Z09 =  Y.train.rf[,11])
sub.9 = sub.9 %>% sample_n(15000)
fit.rf.9 = randomForest(Z09 ~ ., data = sub.9,
                        importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.9)
pred.9 = predict(fit.rf.9,X.test.rf)


sub.10 = cbind(X.train.rf[,3:64],Z10 =  Y.train.rf[,12])
sub.10 = sub.10 %>% sample_n(15000)
fit.rf.10 = randomForest(Z10 ~ ., data = sub.10,
                         importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.10)
pred.10 = predict(fit.rf.10,X.test.rf)


sub.11 = cbind(X.train.rf[,3:64],Z11 =  Y.train.rf[,13])
sub.11 = sub.11 %>% sample_n(15000)
fit.rf.11 = randomForest(Z11 ~ ., data = sub.11,
                         importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.11)
pred.11 = predict(fit.rf.11,X.test.rf)


sub.12 = cbind(X.train.rf[,3:64],Z12 =  Y.train.rf[,14])
sub.12 = sub.12 %>% sample_n(15000)
fit.rf.12 = randomForest(Z12 ~ ., data = sub.12,
                         importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.12)
pred.12 = predict(fit.rf.12,X.test.rf)


sub.13 = cbind(X.train.rf[,3:64],Z13 =  Y.train.rf[,15])
sub.13 = sub.13 %>% sample_n(15000)
fit.rf.13 = randomForest(Z13 ~ ., data = sub.13,
                         importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.13)
pred.13 = predict(fit.rf.13,X.test.rf)


sub.14 = cbind(X.train.rf[,3:64],Z14 =  Y.train.rf[,16])
sub.14.2 = sub.14 %>% sample_n(15000)
fit.rf.14 = randomForest(Z14 ~ ., data = sub.14,
                         importance = F,mtry =10, nodesize = 2, ntree = 500)
plot(fit.rf.14)
pred.14 = predict(fit.rf.14,X.test.rf)


predition = cbind(Z01 = pred.1, Z02 = pred.2, Z03 = pred.3, Z04 = pred.4,
                  Z05 = pred.5, Z06 = pred.6, Z07 = pred.7, Z08 = pred.8, 
                  Z09 = pred.9, Z010 = pred.10, Z11 = pred.11, Z12 = pred.12,
                  Z13 = pred.13, Z14 = pred.14)

output = cbind(Id = Y.test$new_ID,Value = Y.test$Value)
write.table(output, file = 'pred_rf.csv', sep = ',', quote = F, row.name = FALSE)
