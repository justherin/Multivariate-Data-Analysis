library(MASS)
library(ROCR)
library(Matrix)
library(foreach)
library(glmnet)

#LOOCV for Linear Regreesion
datanew <- read.csv("Life_Expectancy.csv")
datamod <- na.omit(datanew)
datamod <- data.frame(datamod)
lm.fit <- lm(Life.expectancy ~ Alcohol+percentage.expenditure+Total.expenditure+GDP+Income.composition.of.resources+Schooling, data= datamod)
n <- nrow(datamod)
sum_sse <- 0
for (i in 1:n){
  test_ind <- i
  test_data <- datamod[test_ind,]
  train_data <- datamod[-test_ind,]
  
  lm.fit <- lm(Life.expectancy ~ Alcohol+percentage.expenditure+Total.expenditure+GDP+Income.composition.of.resources+Schooling, data= train_data)
  test_pred <- predict(lm.fit, test_data)
  SE <- (test_pred - test_data$Life.expectancy)^2
  
  sum_sse <- sum_sse + SE
}
mse <- sum_sse/n
print(mse)


#Bootstrapping
B <-  1000
MSE1 <- numeric(B)
set.seed(2)

for (j in 1:B) {
  BS_Data2 <- datamod[sample(1:n, replace = TRUE),]
  
  sum_sse2 <- 0
  
  for (i in 1:n){
    test_ind <- i
    test_data <- BS_Data2[test_ind,]
    train_data <- BS_Data2[-test_ind,]
    
    lm.fit2 <- lm(Life.expectancy ~ Alcohol+percentage.expenditure+Total.expenditure+GDP+Income.composition.of.resources+Schooling, data= train_data)
    test_pred <- predict(lm.fit2, test_data)
    SE2 <- (test_pred - test_data$Life.expectancy)^2
    
    sum_sse2 <- sum_sse2 + SE2
  }
  
  MSE1[j] <- sum_sse2/n
}

mean(MSE1) #0.2898434
sd(MSE1) #0.03683273




par(mfrow=c(3,3))
lm.fit <- lm(Life.expectancy ~ Alcohol+percentage.expenditure+Total.expenditure+GDP+Income.composition.of.resources+Schooling, data= datamod)
plot(resid(lm.fit)~datamod$Alcohol+datamod$percentage.expenditure+datamod$Total.expenditure+datamod$GDP+datamod$Income.composition.of.resources+datamod$Schooling)

library(glmnet)

Xtrain <- datamod[, c("Alcohol","percentage.expenditure","Total.expenditure","GDP","Income.composition.of.resources","Schooling")]
Ytrain <- datamod[["Life.expectancy"]]

mdl.lasso <- glmnet(x=as.matrix(Xtrain), y=Ytrain, family = "gaussian", alpha = 1)

cv.glmmod <- cv.glmnet(x=as.matrix(Xtrain), y=Ytrain, alpha=1)

print(best.lambda <- cv.glmmod$lambda.min) 
#0.02353942
#predict(fit,newx=nx,s=c(0.1,0.05))

# Log(lamnda) = 0.02353942

# Find Minimum MSE -
cv.glmmod$cvm[cv.glmmod$lambda==cv.glmmod$lambda.min]

#LOOCV to estimate MSE
MSE_lasso_cv_i <- numeric(n)
for(i in 1:n) {
  fit_lasso_cv_1 <- glmnet(X[-i, ], Y[-i], lambda = fit_lasso_cv$lambda,
                           family = "gaussian", alpha = 1)
  y_hat_lasso_cv_1 <- predict(fit_lasso_cv_1, newx = X[i, , drop = FALSE],
                              s = fit_lasso_cv$lambda.min)
  MSE_lasso_cv_i[i] <- (y_hat_lasso_cv_1 - Y[i])^2
}
MSE_lasso_cv <- mean(MSE_lasso_cv_i)


# 0.2944128 - This would be equal to min(cv.glmmod$cvm)


sum_sse3 <- 0
for (i in 1:n){
  test_ind <- i
  test_data <- datamod[test_ind,]
  train_data <- datamod[-test_ind,]
  
  Xtrain <- as.matrix(datamod[, c("Alcohol","percentage.expenditure","Total.expenditure","GDP","Income.composition.of.resources","Schooling")])
  Ytrain <- as.matrix(datamod[["Life.expectancy"]])
  
  Xtest <- as.matrix(test_data[, c("Alcohol","percentage.expenditure","Total.expenditure","GDP","Income.composition.of.resources","Schooling")])
  
  lasso.fit <- glmnet(x=Xtrain, y=Ytrain, lambda = 0.16626,  family = "gaussian")
  test_pred <- predict(lasso.fit, newx=Xtest)
  
  SE3 <- (test_pred - test_data$Life.expectancy)^2
  
  sum_sse3 <- sum_sse3 + SE3
}

MSE3 <- sum_sse3/n
print(MSE3)

B <-  1000
MSE4 <- numeric(B)
set.seed(2)

for (j in 1:B) {
  BS_Data2 <- datamod[sample(1:n, replace = TRUE),]
  
  sum_sse4 <- 0
  
  for (i in 1:n){
    test_ind <- i
    test_data <- datamod[test_ind,]
    train_data <- datamod[-test_ind,]
    
    Xtrain <- as.matrix(train_data[, c("Alcohol","percentage.expenditure","Total.expenditure","GDP","Income.composition.of.resources","Schooling")])
    Ytrain <- as.matrix(train_data[["Life.expectancy"]])
    
    Xtest <- as.matrix(test_data[, c("Alcohol","percentage.expenditure","Total.expenditure","GDP","Income.composition.of.resources","Schooling")])
    
    lasso.fit <- glmnet(x=Xtrain, y=Ytrain, lambda = 0.16626,  family = "gaussian")
    test_pred <- predict(lasso.fit, newx=Xtest)
    
    SE4 <- (test_pred - test_data$Life.expectancy)^2
    
    sum_sse4 <- sum_sse4 + SE4
  }
  
  
  MSE4[j] <- sum_sse4/n
}

mean(MSE4)
sd(MSE4)



Xtrain <- datamod[, c("Alcohol","percentage.expenditure","Total.expenditure","GDP","Income.composition.of.resources","Schooling")]
Ytrain <- datamod[["Life.expectancy"]]

mdl.lasso <- glmnet(x=as.matrix(Xtrain), y=Ytrain, family = "gaussian", alpha = 1)
lm.fit <- lm(Life.expectancy ~ Alcohol+percentage.expenditure+Total.expenditure+GDP+Income.composition.of.resources+Schooling, data= datamod)

coef(mdl.lasso, s= 0.1662)
coef(lm.fit)

datan <- read.csv("hof_data.csv")
datam <- na.omit(datan)
datam <- data.frame(datam)
set.seed(2)
datam$HOF2[datam$HOF=="N"] <- 0
datam$HOF2[datam$HOF=="Y"] <- 1
datam$HOF2=factor(datam$HOF2)
dataN <- datam[datam$HOF2==0,]
dataY <- datam[datam$HOF2==1,]
tN <- sample(1:dim(dataN)[1],round(dim(dataN)[1]*2/3) ,replace = FALSE)
tY <- sample(1:dim(dataY)[1],round(dim(dataY)[1]*2/3) ,replace = FALSE)
traindataN <- dataN[tN,]
traindataY <- dataY[tY,]
testdataN <- dataN[-tN,]
testdataY <- dataY[-tY,]
testdata <- data.frame(rbind(testdataN,testdataY))
traindata <- data.frame(rbind(traindataN,traindataY))
glm.fit <- glm(HOF2~H+HR+AVG,family="binomial",data=traindata)
predres <- predict(glm.fit,testdata,type = 'response')
d1<-(table(testdata$HOF2,predres>0.9))
print(d1)
d1[1,1]
perf <- performance(prediction(predres, testdata$HOF2), 'tpr', 'fpr')



n <- nrow(datam)
mclambda<- vector()
mcsens<- vector()
mcspes<- vector()
sens=0
spes=0
k=0
for(j in seq(0,1,by=0.1)){
k=k+1
sum_mcrate <- 0
sens_sum <- 0
spes_sum <- 0
for (i in 1:n){
  test_ind <- i
  test_data <- datam[test_ind,]
  train_data <- datam[-test_ind,]
  glm.fit <- glm(HOF2~H+HR+AVG,family="binomial",data=train_data)
  predres <- predict(glm.fit,test_data,type = 'response')
  d1<-(data.frame(test_data$HOF2,predres>j))
 if(d1[1,1]==as.numeric(d1[1,2]))
 {
   mcrate=1
   if(d1[1,1]==1)
   {
     sens=1
     
   }
   else
   {
     spes=1
   }
   
 }
  else
  {
    mcrate=0
    if(d1[1,1]==1)
    {
      sens=0
    }
    else
    {
      spes=0
    }
  }
  sum_mcrate<- sum_mcrate+mcrate
  sens_sum=sens+sens_sum
  spes_sum=spes+spes_sum
  sens <- 0
  spes <- 0
  
}
  mclambda[k]<- (sum_mcrate/n)
  mcsens[k]<- (sens_sum/n)
  mcspes[k]<- (spes_sum/n)
}
print(mclambda)
print(mcsens)
plot((1-mcspes),mcsens,type='l')
which.min(1-mclambda)
op<- data.frame(cbind(seq(0,1,by=0.1),1-mclambda))
op[which.min(op[,2]),1]
op2<- data.frame(cbind(seq(0,1,by=0.1),1-mclambda,mcsens,mcspes))
colnames(op2)<- (c("lambda","missclass","sens","spes"))
print(op2)

library(glmnet)

Xtrain <- traindata[, c("H","HR","AVG")]
Ytrain <- as.factor(traindata[,"HOF2"])

mdl.lasso <- glmnet(x=as.matrix(Xtrain), y=as.matrix(Ytrain), family = "gaussian", alpha = 1)

lasso.model <- glmnet(x=as.matrix(Xtrain), y=as.matrix(Ytrain), lambda = 0.0003562591,  family = "gaussian")
coef(mdl.lasso)
predres <- predict(lasso.model,as.matrix(testdata[, c("H","HR","AVG")]),type = 'response')
d1<-(table(testdata$HOF2,predres>0.8))
print(d1)

#Your own solution to optimize k


#If to find lambda
for(j in seq(0,100,by=0.1)){
  k=k+1
  sum_mcrate <- 0
  sens_sum <- 0
  spes_sum <- 0
  for (i in 1:n){
    test_ind <- i
    test_data <- datam[test_ind,]
    train_data <- datam[-test_ind,]
    Xtrain <- train_data[, c("H","HR","AVG")]
    Ytrain <- as.factor(train_data[,"HOF2"])
    
    #mdl.lasso <- glmnet(x=as.matrix(Xtrain), y=as.matrix(Ytrain), family = "gaussian", alpha = 1)
    
    lasso.model <- glmnet(x=as.matrix(Xtrain), y=as.matrix(Ytrain), lambda = j,  family = "gaussian")
    
    #lasso.model <- glmnet(x=as.matrix(Xtrain), y=as.matrix(Ytrain), lambda = j,  family = "gaussian")
    predres <- predict(glm.fit,test_data,type = 'response')
    d1<-(data.frame(test_data$HOF2,predres>0.8))
    if(d1[1,1]==as.numeric(d1[1,2]))
    {
      mcrate=1
      if(d1[1,1]==1)
      {
        sens=1
        
      }
      else
      {
        spes=1
      }
      
    }
    else
    {
      mcrate=0
      if(d1[1,1]==1)
      {
        sens=0
      }
      else
      {
        spes=0
      }
    }
    sum_mcrate<- sum_mcrate+mcrate
    sens_sum=sens+sens_sum
    spes_sum=spes+spes_sum
    sens <- 0
    spes <- 0
    
  }
  mclambda[k]<- (sum_mcrate/n)
  mcsens[k]<- (sens_sum/n)
  mcspes[k]<- (spes_sum/n)
}
print(1-mclambda)
print(mcspes)




##Method as solutio ROC curve for logistic regression

k <- seq(from = 0, to = 1, by = 0.01)
cv_right <- sens <- spec <- numeric(length(k))
glm.prob <- predict(glm.fits, dta_training, type = "response")
for(i in 1:length(k)) {
  sens[i] <- mean(glm.prob[which(dta_training[,1]==1)]>k[i])
  spec[i] <- mean(glm.prob[which(dta_training[,1]==0)]<k[i])
}
plot(1-spec, sens, xlab = "false positive rate", ylab = "true positive rate",
     main = "ROC curve", type = "l")

###Method as solution Finding optimal kappa logistic regression
k <- seq(from = 0, to = 1, by = 0.01)

for (i in 1:length(k)){
  cvright_tmp <- rep(0,1186)
  for (j in 1:1186){
    dta_loocvtrain <- dta_training[-j, ]
    glm.fits <- glm(HOF ~ H + HR + AVG, family = binomial, data = dta_loocvtrain)
    glm.prob <- predict(glm.fits, dta_training[j,], type = "response")
    if (glm.prob > k[i]){
      cvright_tmp[j] <- 1
    }
  }
  cv_right[i] <- mean(cvright_tmp==dta_training[,1])
}
which.max(cv_right)

##After k is found out
cvright_tmp <- rep(0,1186)
for (j in 1:1186){
  dta_loocvtrain <- dta_training[-j, ]
  glm.fits <- glm(HOF ~ H + HR + AVG, family = binomial, data = dta_loocvtrain)
  glm.prob <- predict(glm.fits, dta_training[j,], type = "response")
  if (glm.prob > k_opt){
    cvright_tmp[j] <- 1
  }
}
table(cvright_tmp,dta_training[,1])


###CV on lambda, cefficients nad then missclass rate
set.seed(1)
library(Matrix)
library(foreach)
library(glmnet)
x <- model.matrix(HOF ~ ., dta_training)[, -1]
y <- dta_training[,1]
cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "auc")
lambda_min <- cv_lasso$lambda.min
lambda_min
lambda_grid <- cv_lasso$lambda
fit_lasso <- glmnet(x, y, alpha = 1, family = "binomial", lambda = lambda_grid)
lasso.coed <- predict(fit_lasso, type = "coefficients", s = lambda_min)[1:4, ]
pred_lasso <- predict(fit_lasso, newx = x, s = lambda_min, type = "response")
lasso_pred <- rep(0, 1186)
lasso_pred[pred_lasso > 0.5] = 1
table(lasso_pred, dta_training[, 1])


#