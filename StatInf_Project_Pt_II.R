####################################################
# Statistical Inference Project Part II
####################################################
####################################################

## Part 2 Basic Inferential Data Analysis
#
# Now in the second portion of the project, we're going to analyze the ToothGrowth data in the R datasets package.
# 
# Load the ToothGrowth data and perform some basic exploratory data analyses
# Provide a basic summary of the data.
# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
# State your conclusions and the assumptions needed for your conclusions.

#load libraries
library(dplyr)
library(ggplot2)

#Define Variables for simulation
set.seed(100) # set the seed value for reproducibility

#Load Data
data("ToothGrowth")

#plot data preparation
plotdata <- ggplot(ToothGrowth, aes(as.factor(dose),len,fill = supp))
plotdata <- plotdata + geom_bar(stat = "identity", position = "dodge") + 
    labs(title = "Tooth Growth per dose and supplement", 
         x = "Dose", y = "Growth Length")


#Preview of data
head(ToothGrowth,15)

#Detail of data
str(ToothGrowth)

#Data Summary
summary(ToothGrowth)

#plot data
plotdata

#Compare tooth growth by supplement using a t-test.
t_gr_sup <- t.test(len~supp,data=ToothGrowth)
cat("Conf Int:",t_gr_sup$conf.int,"p-value",t_gr_sup$p.value)

#Compare tooth growth by supplement using a t-test.
# doses 1 and 2
t_gr_dose_1_2 <- t.test(len~dose,data=subset(ToothGrowth,dose %in% c(1, 2)))
cat("Conf Int:",t_gr_dose_1_2$conf.int,"p-value",t_gr_dose_1_2$p.value)

#Compare tooth growth by supplement using a t-test.
# doses .5 and 1
t_gr_dose_5_1 <- t.test(len~dose,data=subset(ToothGrowth,dose %in% c(.5, 1)))
cat("Conf Int:",t_gr_dose_5_1$conf.int,"p-value",t_gr_dose_5_1$p.value)

#Compare tooth growth by supplement using a t-test.
# doses .5 and 2
t_gr_dose_5_2 <- t.test(len~dose,data=subset(ToothGrowth,dose %in% c(0.5, 2)))
cat("Conf Int:",t_gr_dose_5_2$conf.int,"p-value",t_gr_dose_5_2$p.value)

