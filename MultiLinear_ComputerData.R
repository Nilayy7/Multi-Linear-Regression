#Predict Price of the computer
View(Computer_Data)

#We need to transform character to numeric
library(plyr)

Cdata <- Computer_Data
Cdata$cd <- as.numeric(revalue(Cdata$cd,c("yes"=1,"no"=0)))
Cdata$multi <- as.numeric(revalue(Cdata$multi,c("yes"=1,"no"=0)))
Cdata$premium <-as.numeric(revalue(Cdata$premium,c("yes"=1,"no"=0)))
View(Cdata)
class(Cdata)
attach(Cdata)
summary(Cdata)

#Plot Scatter Plots
plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)

#Correlation
cor(Cdata)

#Build Linear Model
model <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(model)
#Multiple R-squared:  0.7756,	Adjusted R-squared:  0.7752
plot(model)


library(mvinfluence)
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)


predict(model,interval = 'predict')

# Final Model
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3))
summary(FinalModel)

#Prediction
Profpred <- predict(FinalModel)
View(Profpred)

finalplot <-Cdata[-c(1441,1701),]
View(finalplot)

Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profpred)
View(Final)
