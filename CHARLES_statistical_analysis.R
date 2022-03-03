library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

getwd()

# iq <- read.csv("./out.csv")
iq <- read.csv("./out_windowed_mean.csv")

# Run linear regression
m1 = lm(TF.IDF.Total ~ Popularity, data=iq)
iq$fitted_values <- m1$fitted.values
iq$residuals <- m1$residuals

# Histogram of residuals, check normality assumption of residuals
hist(iq$residuals)

# Check constant variance assumption of the regression model.
# Check linearity assumption of the regression model
# Check normality assumption of the linear regression model
plot(m1)

# Check summary of regression
summary(m1)

# MODEL 3 (THIS IS OUR BEST MODEL THAT WE CHOSE)

# Run polynomial regression 
m3 = lm(Popularity ~ TF.IDF.Total + I(TF.IDF.Total^2), data=iq)

# Check constant variance assumption of the regression model.
# Check linearity assumption of the regression model
# Check normality assumption of the linear regression model
plot(m3)

# Check summary of regression
# probability of F that large under the null hypothesis of no effect is small, reject null hypothesis
summary(m3)

# Anova table to see if 
anova(m3)


#plot x vs. y
plot(iq$TF.IDF.Total, iq$Popularity, pch=16, cex=1.5) 
#use model to get predicted values
pred <- predict(m3)
ix <- sort(iq$TF.IDF.Total, index.return=T)$ix
#add polynomial curve to plot
lines(iq$TF.IDF.Total[ix], pred[ix], col='red', lwd=2)

# MODEL 4
iq$LOG_Popularity <- log10(iq$Popularity)

# Run polynomial regression on the log of popularity
m4 = lm(LOG_Popularity ~ TF.IDF.Total + I(TF.IDF.Total^2), data=iq)

# Check constant variance assumption of the regression model.
# Check linearity assumption of the regression model
# Check normality assumption of the linear regression model
plot(m4)

# Check summary of regression
# probability of F that large under the null hypothesis of no effect is small, reject null hypothesis
summary(m4)

# Anova table to see if 
anova(m4)


#plot x vs. y
plot(iq$TF.IDF.Total, iq$LOG_Popularity, pch=16, cex=1.5) 
#use model to get predicted values
pred <- predict(m4)
ix <- sort(iq$TF.IDF.Total, index.return=T)$ix
#add polynomial curve to plot
lines(iq$TF.IDF.Total[ix], pred[ix], col='red', lwd=2)






