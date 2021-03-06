#Prepare a prediction model for predicting Price.

View(ToyotaCorolla)
Corolla <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
summary(Corolla)
attach(Corolla)

Corolla <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla <- as.data.frame(Corolla)

View(Corolla)
attach(Corolla)

#Scatter Plots
plot(Age_08_04,Price)
plot(KM,Price)
plot(HP,Price)
plot(cc,Price)
plot(Doors,Price)
plot(Gears,Price)
plot(Quarterly_Tax,Price)
plot(Weight,Price)

#Linear Model
corolla_model <- lm(Price ~ Age_08_04 + KM + HP + cc + Doors + Gears + Quarterly_Tax + Weight) 
plot(corolla_model)
summary(corolla_model)
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(corolla_model,id.n=2,id.cex=0.7)

#New model
corolla_model1 <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight) 
summary(corolla_model1)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(corolla_model1,id.n=2,id.cex=0.7)

#Influence Plot
library(mvinfluence)
library(car)
influencePlot(corolla_model, id.n=3) 
#81st observation is influential

#After removing 81st observation
corolla_model2 <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight,data = Corolla[-81,])
summary(corolla_model2)
#Multiple R-squared:  0.8632,	Adjusted R-squared:  0.8626 

#final model
fmodel <- lm(Price~Age_08_04 + KM + HP + cc + Doors + Gears + Quarterly_Tax + Weight) 
summary(fmodel)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(fmodel,id.n=2,id.cex=0.7)

#Prediction
PricePredict <- predict(corolla_model,interval = 'predict')

Finalpred <- predict(corolla_model)
f <- cbind(Price,Finalpred,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
View(f)

#Evaluate model
plot(fmodel)
qqPlot(fmodel,id.n =4)

#Correlation
cor(Corolla)
cor(Corolla$Price,Corolla$Age_08_04)
cor(Corolla$Price,Corolla$KM)
cor(Corolla$Price,Corolla$HP)
cor(Corolla$Price,Corolla$cc)
cor(Corolla$Price,Corolla$Doors)
cor(Corolla$Price,Corolla$Gears)


#Barplot
#Barplot
barplot(height=Corolla$Price, names=Corolla$Price)
barplot(height=Corolla$KM, names=Corolla$KM)
