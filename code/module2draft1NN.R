##################################################################################
## Neural Net

## Code to produce csv. file
library(nnet)
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

## Training set
X.train.raw = read.table("x_train_preprocessed.csv", sep = ",", header = TRUE)
Y.train.raw = read.table("y_train_preprocessed.csv", sep = ",", header = TRUE)
full.data = cbind(X.train.raw, Y.train.raw)

X.train.raw = full.data[,3:64]
Y.train = full.data[,69:82]
X.train = rescale(X.train.raw, X.train.raw)

## Testing set
X.test.raw = read.table("x_test_preprocessed.csv", sep = ",", header = TRUE)
Y.test = read.table("Ytest.txt", sep = ",", header = TRUE)
Y.test$new_ID = sub(":.*", "", Y.test$Id)  ## slipt into two columns
Y.test$new_Y = sub(".*Z", "", Y.test$Id)   ## we only need Id in output 

X.test.raw = X.test.raw[,3:64]  ## the first two columns are IDs
X.test = rescale(X.test.raw, X.train.raw)


n.nnets = 20 ## repeat 20 times
all.MSEs = rep(0, times = 20) ## create an empty box to store MSE for each run 
all.nnets = list(1:20)
for(i in 1:n.nnets){
  nnet.1 = nnet(y = Y.train, x = X.train, linout = TRUE, size = 2,
                decay = 0.1, maxit = 500, trace = F)
  this.MSE = nnet.1$value/nrow(X.train)
  all.MSEs[i] = this.MSE
  all.nnets[[i]] = nnet.1
}
all.MSEs
(min_MSE.1 = min(all.MSEs))
ind.best = which.min(all.MSEs)
fit.nnet.best = all.nnets[[ind.best]] ## choose the run with the smallest MSE and use that fit as our model
pred.nnet = predict(fit.nnet.best, X.test)



## Fitting pred.nnet into Y.test
for (i in 1:length(Y.test$Value)){
  col = as.numeric(Y.test$new_Y[i]) ## new_Y: string after "Z"; change them to numeric so that the numbers align with their column orders
  Y.test$Value[i] <- pred.nnet[i,col]
}

output = cbind(Id = Y.test$new_ID,Value = Y.test$Value)
write.table(output, file = 'pred.csv', sep = ',', quote = F, row.name = FALSE)
#######################################################################################################END

## Tuning process


get.MSE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

get.folds = function(n, K) {
  n.fold = ceiling(n / K)
  fold.ids.raw = rep(1:K, times = n.fold)
  fold.ids = fold.ids.raw[1:n]
  folds.rand = fold.ids[sample.int(n)]
  return(folds.rand)
}

X.train = X.train %>% sample_n(15000)
Y.train = Y.train %>% sample_n(15000)
subset = cbind(X.train, Y.train)
M = 5
all.n.hidden = c(1,2,5,7,9)
all.shrink = c(0.05,0.5,1,2)
all.pars = expand.grid(n.hidden = all.n.hidden,shrink = all.shrink)
n.pars = nrow(all.pars)
K = 5
folds = get.folds(nrow(subset), K)
CV.MSEs = array(0, dim = c(K, n.pars))

for(i in 1:K){
  set.seed(4099183,kind = "Mersenne-Twister")
  print(paste0(i, " of ", fold_num))
  data.train = subset[folds != i,]
  X.train = data.train[,1:62]
  Y.train = data.train[,63:76]
  
  data.valid = subset[folds == i,]
  X.valid = data.valid[,1:62]
  Y.valid = data.valid[,63:76]
  
  for(j in 1:n.pars){
    this.n.hidden = all.pars[j,1]
    this.shrink = all.pars[j,2]
    all.nnets = list(1:M)
    all.SSEs = rep(0, times = M)
    for(l in 1:M){
      fit.nnet = nnet(X.train, Y.train, linout = TRUE, size = this.n.hidden,
                      decay = this.shrink, maxit = 500, trace = FALSE)
      SSE.nnet = fit.nnet$value
      all.nnets[[l]] = fit.nnet
      all.SSEs[l] = SSE.nnet
    }
    ind.best = which.min(all.SSEs)
    fit.nnet.best = all.nnets[[ind.best]]
    pred.nnet = predict(fit.nnet.best, X.valid)
    MSE.nnet = get.MSE(Y.valid[,1], pred.nnet)  ## get MSE for Z01
    #MAE.nnet = mae(Y.valid[,14], pred.nnet)
    CV.MSEs[i, j] = MSE.nnet 
  }
}
names.pars = paste0(all.pars$n.hidden,",",all.pars$shrink)
colnames(CV.MSEs) = names.pars
boxplot(CV.MSEs,main = "CV MSEs over 5 folds",las = 2)

CV.RMSEs = apply(CV.MSEs, 1, function(W) W/min(W))
CV.RMSEs = t(CV.RMSEs)
boxplot(CV.RMSEs, las = 2,ylim = c(1,1.005),main = "RMSE Boxplot")
