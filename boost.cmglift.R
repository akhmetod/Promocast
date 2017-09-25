## boosted trees
rm(list=ls())
library(gbm)

data <- read.csv("C:/Users/JayAllu/Desktop/gbt.csv",sep=",",header = T)
data <- select(data,-product)
str(data)
attach(data)

data$week.prev.promo <- as.factor(data$week.prev.promo)
data$week.prev2.promo <- as.factor(data$week.prev2.promo)

data$disc <- as.numeric(sub("%","",disc))
data$CMG.Lift <- as.numeric(sub("%","",CMG.Lift))


set.seed(1001)
ind <- sample(1:nrow(data),.75*nrow(data))
train_data <- data[ind,]
test_data <- data[-ind,]




set.seed(1001)
nt <- seq(1,13,1)
lr <- c(.1,.05,.01,.005,.001)
trees <- c(1000,1500,2000,2500,3000,4000,5000)

error.boost <- matrix(rep(0,1),nrow = 1)
#error.boost <- rep(0,4)
start.time <- Sys.time()

for(i in lr){
  for(j in nt){
    for(k in trees){
      boost.mod <- gbm(CMG.Lift ~.,data = train_data,distribution = 'gaussian',n.trees= k,interaction.depth = 4,shrinkage = i,cv.folds = 10)
      pred.boost <- predict.gbm(boost.mod,test_data,n.trees = 1000 )
      error.boost1 <- sum(abs(test_data$CMG.Lift - pred.boost))
      print(c(error.boost1,i,j,k))
      error.boost = rbind(error.boost1,error.boost)
    }
  }
}
end.time <- Sys.time()
time <- end.time - start.time

# fit the best parameters
set.seed(1001)

boost.mod <- gbm(CMG.Lift ~.,data = train_data,distribution = 'gaussian',n.trees= 2000,interaction.depth = 4,shrinkage = 0.05,cv.folds = 10)
pred.boost <- predict.gbm(boost.mod,test_data,n.trees = 2000 )
pred.boost[pred.boost< 0] <- 0
plot(pred.boost)
hist(pred.boost,col="green")
hist(test_data$CMG.Lift,col="red",add=T)

plot(pred.boost,type="l",col="green")
lines(test_data$CMG.Lift,col="red")
legend("topright",c("CMG.Lift","Pred.Lift"),col=c("red","green"),lty=1,cex=0.65)

## validating the model
set.seed(1001)
valid_data <- data[sample(1:nrow(data),.3*nrow(data)),]
pred1 <- predict.gbm(boost.mod,valid_data,n.trees = 2000 )

par(mfrow=c(1,1))
plot(pred.boost,type="l",col="green")
lines(valid_data$CMG.Lift,col="red")
legend("topright",c("CMG.Lift","Pred.Lift"),col=c("red","green"),lty=1,cex=0.65)

## notes:

# 1) the model tends to over estimate the lifts on average
# 2) bootstrapping will help determining the confidence for the estimation.
# 3) fitting accounts as a factor will give more accurate results
# 4) added measures like velocity(comparable across accounts) will inc. prediction 


## lift and discount distribution ##

pred1 <- predict.gbm(boost.mod,data,n.trees = 2000 )
pred1[pred1 < 0] <- 0
data$pred.Lift <- pred1
data.sort <- data[order(data$disc),]

Agg <- aggregate(data$CMG.Lift ~ data$disc,data,mean)
Agg.pred <- aggregate(data$pred ~ data$disc,data,mean)

smoothingSpline = smooth.spline(Agg$`data$disc`, Agg$`data$CMG.Lift`, spar=0.35)
plot(Agg$`data$disc`,Agg$`data$CMG.Lift`)
lines(smoothingSpline)

smoothingSpline1 = smooth.spline(Agg.pred$`data$disc`, Agg.pred$`data$pred`, spar=0.35)
plot(Agg.pred$`data$disc`,Agg.pred$`data$pred`)
lines(smoothingSpline1)

hist(data$disc,col="grey",xlab = "Discount in %",ylab="Avg.Lift",ylim=c(0,1000),main="Lift and discount distribution(pupperoni)")
lines(smoothingSpline,col="red")
lines(smoothingSpline1,col="green")
legend("bottomright",c("CMG.Lift","Pred.Lift"),col=c("red","green"),cex=0.65,lty=1,lwd=2,text.font=4,box.lty=0)

## As seen in the graphs, the discount is concentrated around 5-15% wher the slope of average lift is almost flat. 
## The predicted lifts are pretty close to the actual.
## The challenge is to forecast the dependent variables as the model performs fairly when given accurate inputs.


