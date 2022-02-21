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