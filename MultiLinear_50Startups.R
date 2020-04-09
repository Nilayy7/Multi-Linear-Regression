# Prepare a prediction model for profit of 50_startups data
View(`50_Startups`)

Startups <- `50_Startups`
View(Startups)
class(Startups)

# To Transform the data from Character to Numeric
library(plyr)
Startups$State <- revalue(Startups$State,
                          c("New York"="0", "California"="1", "Florida"="2"))
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)


Startups <- as.data.frame(Startups)
attach(Startups)
summary(Startups)
View(Startups)

#Scatter Plot
plot(R.D.Spend,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)
plot(Administration,Profit)

#Correlation
cor(Startups)

#Linear Model
model <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(model)
#Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9464 
plot(model)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model,id.n=2,id.cex=0.7)

#boxplot
boxplot(Profit~RD_Spend+Administration+Marketing_Spend+State , data=Startups, main = "Output result",las = 2, pch=16, cex = 1,
        col = "lightblue", xlab = "R",ylab = "SNP values",ylim =c(-0.4,1.2), 
        border ="blue", boxwex = 0.3)

model1 <- lm(Profit~RD_Spend+log(Administration))
summary(model1)
#Multiple R-squared:  0.9474,	Adjusted R-squared:  0.9451 
plot(model1)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1,id.n=2,id.cex=0.7)


install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups))

install.packages("mvinfluence")
library(mvinfluence)
library(car)
influence.measures(model)

#Index Plot of influence measure
influenceIndexPlot(model,id = 3)
influencePlot(model,id=3)

#Correlation and Barplot
barplot(height=Startups$Profit, names=Startups$Profit)

cor(Startups$Profit,Startups$RD_Spend)
cor(Startups$Profit,Startups$Administration)
cor(Startups$Profit,Startups$State)


#logarthimic Transformation
model2 <- lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=Startups[-c(49,50),])
plot(model2)
summary(model2)
#Multiple R-squared:  0.9625,	Adjusted R-squared:  0.9591
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model2,id.n=2,id.cex=0.7)


#Exponential Transformation
model3 <- lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=Startups[-c(49,50),])
plot(model3)
summary(model3)
#Multiple R-squared:  0.9252,	Adjusted R-squared:  0.9182
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model3,id.n=2,id.cex=0.7)


#Function to predict the model
p <- predict(model,interval = 'predict')
p1 <- predict(model1,interval = 'predict')
p2 <- predict(model2,interval = 'predict')
p3 <- predict(model3,interval = 'predict')

rmse <- sqrt(mean(p2-Profit)^2)
rmse

rmse1 <- sqrt(mean(p3-Profit)^2)
rmse1






