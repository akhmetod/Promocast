rm(list=ls())
data_pup <- read.csv("C:/Rcode/pup.csv",header = T,sep=",")
library(dplyr)
data_use <- select(data_pup,MKT,COT,Account.use..1,Chart.Product.Name,SALES.UNITS,
                   Adj.price,In.Dist..1,X..ACV.Dist..Max.,CUM..ACV...STORE.SELLING.,Promo.1)
summary(data_use)
data_use <- data_use[!data_use$SALES.UNITS==0,]
data_use <- data_use[!data_use$Adj.price==0,]


data_use <- filter(data_use,as.numeric(Promo.1==0),Account.use..1==1,COT=="FOOD",
                   Chart.Product.Name=="PUPPRNI 5.6OZ",Adj.price>=2.79,Adj.price<=4.49,In.Dist..1==1)

data_use$Chart.Product.Name <- factor(data_use$Chart.Product.Name)
data_use$COT<- factor(data_use$COT)
data_use$Adj.price <- factor(data_use$Adj.price)

summary(data_use)
str(data_use)

attach(data_use)
model <- lm(log(SALES.UNITS)~ 1+factor(Adj.price)+log(X..ACV.Dist..Max.)+
              log(CUM..ACV...STORE.SELLING.)+MKT,data = data_use)

summary(model)


par(mfrow=c(2,2))
plot(model)

### different model for different price points
data_use1 <- data_use[Adj.price=="2.99"|Adj.price=="2.79",]
data_use1$Adj.price <- factor(data_use1$Adj.price)
data_use1$Adj.price  <- as.numeric(as.character(data_use1$Adj.price))
model1 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use1)
summary(model1)

#### 
data_use2 <- data_use[Adj.price=="2.99"|Adj.price=="3.29",]
data_use2$Adj.price <- factor(data_use2$Adj.price)
data_use2$Adj.price  <- as.numeric(as.character(data_use2$Adj.price))
model2 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use2)
summary(model2)

####
data_use3 <- data_use[Adj.price=="3.49"|Adj.price=="3.29",]
data_use3$Adj.price <- factor(data_use3$Adj.price)
data_use3$Adj.price  <- as.numeric(as.character(data_use3$Adj.price))
model3 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use3)
summary(model3)
####
data_use4 <- data_use[Adj.price=="3.49"|Adj.price=="3.79",]
data_use4$Adj.price <- factor(data_use4$Adj.price)
data_use4$Adj.price  <- as.numeric(as.character(data_use4$Adj.price))
model4 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use4)
summary(model4)
####
data_use5 <- data_use[Adj.price=="3.99"|Adj.price=="3.79",]
data_use5$Adj.price <- factor(data_use5$Adj.price)
data_use5$Adj.price  <- as.numeric(as.character(data_use5$Adj.price))
model5 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use5)
summary(model5)
####
data_use6 <- data_use[Adj.price=="3.99"|Adj.price=="4.29",]
data_use6$Adj.price <- factor(data_use6$Adj.price)
data_use6$Adj.price  <- as.numeric(as.character(data_use6$Adj.price))
model6 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use6)
summary(model6)
####
data_use7 <- data_use[Adj.price=="4.29"|Adj.price=="4.49",]
data_use7$Adj.price <- factor(data_use7$Adj.price)
data_use7$Adj.price  <- as.numeric(as.character(data_use7$Adj.price))
model7 <- lm(log(SALES.UNITS)~ log(Adj.price)+log(CUM..ACV...STORE.SELLING.)+log(X..ACV.Dist..Max.)+MKT,data=data_use7)
summary(model7)

####
aggregate(data_use7$SALES.UNITS ~ data_use7$Adj.price+data_use7$MKT,data = data_use7,FUN = mean)
