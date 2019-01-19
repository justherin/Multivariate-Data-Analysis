datan <- na.omit(read.csv("Life_Expectancy.csv"))
#n <- 100
#x <- runif(n,0,1)
modelp<-lm(datan$Life.expectancy~poly(datan$Schooling,4,raw=TRUE))
summary(modelp)
modelp<-lm(datan$Life.expectancy~poly(datan$Schooling,3))
modelp<-lm(datan$Life.expectancy~poly(datan$Schooling,3,raw=TRUE))
modelp<-lm(datan$Life.expectancy~Schooling+I(Schooling^2)+I(Schooling^3),data=datan)
school_grid <- data.frame("Schooling"=seq(from = 1.53, to = 20.04))
v<-predict(modelp,school_grid)
##print(v)
plot(seq(from = 1.53, to = 20.04),v,type = 'l')
twostderror <- 2*sd(v)/sqrt(length(v))
segments(school_grid$Schooling, v - twostderror, school_grid$Schooling, v + twostderror,col="blue");

twostderror <- 2*sd(modelp$fitted.values)/sqrt(length(modelp$fitted.values))
plot(datan$Schooling,datan$Life.expectancy)
points(datan$Schooling,modelp$fitted.values,col="red")
#curve(fitted(modelp),col="green")
segments(datan$Schooling, modelp$fitted.values - twostderror, datan$Schooling, modelp$fitted.values + twostderror,col="blue");
plot(datan$Life.expectancy,datan$Schooling,type="l",col="green")

library(splines)
fitcube<-lm(Life.expectancy ~ bs(Schooling,knots = c(5,10,15)),data = datan )
summary(fitcube)
v2<-(predict(fitsmoth,school_grid))
p<- as.matrix(v2$y)
sd(p)
twostderror <- 2*sd(as.matrix(v2$y))/sqrt(19)
plot(as.matrix(v2$x),as.matrix(v2$y),type = 'l')
segments(school_grid$Schooling, as.matrix(v2$y) - twostderror, school_grid$Schooling, as.matrix(v2$y) + twostderror,col="blue");
fitnat <- lm(Life.expectancy ~ ns(Schooling, df = 4), data = datan)
v2<-list(predict(fitsmoth,school_grid))
summary(fitsmoth)
twostderror <- 2*sd(v2)/sqrt(length(v2))
plot(seq(from = 1.53, to = 20.04),v2,type = 'l')
segments(school_grid$Schooling, v2 - twostderror, school_grid$Schooling, v2 + twostderror,col="blue");
fitsmoth<-smooth.spline(x=datan$Schooling,y=datan$Life.expectancy,cv = TRUE)
v2<-matrix(predict(fitsmoth,school_grid))
print(v2)
fitsmoth
twostderror <- 2*sd(v2)/sqrt(length(v2))
plot(seq(from = 1.53, to = 20.04),v2,type = 'l')
segments(school_grid$Schooling, v2 - twostderror, school_grid$Schooling, v2 + twostderror,col="blue");


residual <- 2*resid(modelp)
barplot(residual)

library(randomForest)
data2 <- na.omit(read.csv("winequality_red.csv"))
data2$indicator[data2$quality>5]<-"good"
data2$indicator[data2$quality<=5]<-"bad"
data2$indicator<-factor(data2$indicator)

fit2<- randomForest(quality~.,mtry=7,data=traindata)
results2<- predict(fit2,testdata)
table(results2,testdata$quality)
varImpPlot(fit2)
#BSdata<- traindata[sample(1:1000,1000, replace = TRUE),]
B<-100
set.seed(1)
ac<-vector()
for(j in 1:B)
{
  BSdata<- traindata[sample(1:1000,replace = TRUE),]
  BSdata$quality<-factor(BSdata$quality)
  fit2<- randomForest(formula("quality~."),mtry=7,data=BSdata)
  results2<- predict(fit2,testdata)
  r<-table(results2,testdata$quality)
  ac[j]<- sum(diag(r))/599
}
#ac
sd(ac)


#dim(data2)
fit1<- tree(indicator~.,data2)
results1<- predict(fit1,data2,type = "class")
table(results1,data2$indicator)
plot()
set.seed(1)
train<-sample(1:1599,1000,replace = FALSE)
traindata<- data2[train,]
testdata<- data2[-train,]
fit<- tree(indicator~.-quality,traindata)
results<-predict(fit,testdata,type = "class")
plot(fit)
table(results,testdata$indicator)

library(ROCR)
k = seq(from = 0, to = 1, by = 0.01)
sens <- numeric(length(k))
spec <- numeric(length(k))
modelroc=tree(indicator~.-quality,traindata)


fui=predict(modelroc,testdata,type= "prob")
print(fui)
print(k)
for(i in 1:1599) {
  ind<- i
  test_data<-data2[ind,]
  train_data<-data[-ind,]
  modelroc=tree(indicator~.-quality,train_data)
  Pred.cart = predict(modelroc, newdata = testdata, type = "class")
  
  #Pred2 = prediction(Pred.cart, testdata$indicator) 
  
  
  #sens[i] <- sum(fui[which(testdata[,13]==1)]>k[i])
  #spec[i] <- sum(fui[which(testdata[,13]==0)]<k[i])
                 
}
print(spec)
plot(1-spec, sens, xlab = "false positive rate", ylab = "true positive rate",
     
     main = "ROC curve", type = "l")
modelroc=rpart(indicator~.-quality,traindata,control=rpart.control(minsplit=2, cp=0.01179834))
printcp(modelroc)
plot(modelroc, uniform=TRUE,margin=0.2)
text(modelroc, use.n=TRUE, all=TRUE, cex=.8)
set.seed(1)
cv_tree <- cv.tree(modelroc, FUN = prune.misclass)
best.cv <- cv_tree$size[which.min(cv_tree$dev)]
best.cv
p1<-prune(modelroc,cp=0.018509)
p1<-prune(modelroc,cp=0.012397)
p1<-prune(modelroc,cp=0.01101967)
p1<-prune(modelroc,cp=0.01179834)

Pred.cart = predict(modelroc, newdata = testdata, type = "prob")[,2]
print(Pred.cart)
Pred2 = prediction(Pred.cart, testdata$indicator)
print(Pred2)
plot(performance(Pred2, "tpr", "fpr"))
abline(0, 1, lty = 2)

par(mfrow=c(2,2))
