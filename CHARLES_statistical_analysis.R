library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

getwd()

iq <- read.csv("./out.csv")

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

# Log transform TF.IDF.Total to solve for non-constant variance
iq$LOG.TF.IDF.Total <- log10(iq$TF.IDF.Total + 0.0000001)

# Run linear regression with log transformed TF.IDF.Total
m2 = lm(LOG.TF.IDF.Total ~ Popularity, data=iq)

# Check constant variance assumption of the regression model.
# Check linearity assumption of the regression model
# Check normality assumption of the linear regression model
plot(m2)

# Check summary of regression
summary(m2)

