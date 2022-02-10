#-----Packages-----#
library(lavaan)
library(semPlot)
library(tidyverse)
library(ltm)
library(semTools)

#----Data set-----# 
GRIT <- read.csv("D:/SEM_Assignment/archive/data.csv", head = T, sep = ",")

#---Survey_Response_Inspection---# 
Q1_Survey_time <- quantile(GRIT$surveyelapse, .25)
Q3_Survey_time <- quantile(GRIT$surveyelapse, .75)
IQR_Survey_time <- IQR(GRIT$surveyelapse)

GRIT <- subset(GRIT, GRIT$surveyelapse > (Q1_Survey_time - 1.5*IQR_Survey_time) & GRIT$surveyelapse < (Q3_Survey_time + 1.5*IQR_Survey_time))

boxplot(GRIT$surveyelapse)

mean(GRIT$surveyelapse)
median(GRIT$surveyelapse)

#---Removing_English_Failed_Responses---# 
GRIT <- filter(GRIT, VCL6 < 1, VCL9 < 1, VCL12 < 1, gender < 3, gender > 0)

table(GRIT$country)

#---Reverse-Scoring-GRIT---#
GRIT[,c(4, 5, 7, 9, 10, 13)] <- 6 - GRIT[,c(4, 5, 7, 9, 10, 13)]

#---Cronbach-Alpha---#
cronbach.alpha(GRIT[,c(3:14)], CI = T, probs = c(0.025, 0.975), B = 1000) # alpha = 0.831 (0.822, 0.840)

#---Omega_reliability---#
omega(GRIT[,c(3:14)]) # Omega_total = 0.87 

#---CFA---# 
Two_fac_GRIT <- 'Consistency =~ GS2 + GS3 + GS5 + GS7 + GS8 + GS11
                 Perseverance =~ GS1 + GS4 + GS6 + GS9 + GS10 + GS12
                 Consistency ~~ Perseverance'
Two_fac_GRIT_FIT <- cfa(Two_fac_GRIT, std.lv = T, data = GRIT, estimator = "WLSMV")

summary(Two_fac_GRIT_FIT, standardized = T, fit.measures = T) #Accpetable_FIT_overall
#-----Saving-results-----# 
setwd("D:/SEM_Assignment/archive")
saveFile(Two_fac_GRIT_FIT, file = "GRITCFA.txt")

#---Measurement_Invariance---# 
fit.Configural.GRIT <- cfa(Two_fac_GRIT, data = GRIT, meanstructure = T, group = "gender")
summary(fit.Configural.GRIT, standardized = T, fit.measures = T)

fit.Metric.GRIT <- cfa(Two_fac_GRIT, data = GRIT, meanstructure = T, group = "gender", 
                       group.equal = "loadings")
summary(fit.Configural.GRIT, standardized = T, fit.measures = T)

fit.Scalar.GRIT <- cfa(Two_fac_GRIT, data = GRIT, meanstructure = T, group = "gender", 
                       group.equal = c("loadings", "intercepts"))
summary(fit.Configural.GRIT, standardized = T, fit.measures = T)

fit.Residual.GRIT <- cfa(Two_fac_GRIT, data = GRIT, meanstructure = T, group = "gender", 
                         group.equal = c("loadings", "intercepts", "residuals"))
summary(fit.Configural.GRIT, standardized = T, fit.measures = T)

lavTestLRT(fit.Configural.GRIT, fit.Metric.GRIT, fit.Scalar.GRIT, fit.Residual.GRIT)

#---Comprehensive---Test---for---MI---#
library(semTools)
measurementInvariance(model = Two_fac_GRIT, data = GRIT, group = "gender")

#---Misfit-items---#
#-Group-1-#
lavInspect(fit.Scalar.GRIT, "mu")[[1]] - lavInspect(fit.Scalar.GRIT, "sampstat")[[1]]$mean

#-Group-2-#
lavInspect(fit.Scalar.GRIT, "mu")[[2]] - lavInspect(fit.Scalar.GRIT, "sampstat")[[2]]$mean

fit.Scalar.GRIT.2 <- cfa(Two_fac_GRIT, data = GRIT, 
                         group = "gender", 
                         group.equal = c("loadings", "intercepts"), 
                         group.partial = c("GS4 ~ 1", "GS6 ~ 1"))
lavTestLRT(fit.Configural.GRIT, fit.Metric.GRIT, 
      fit.Scalar.GRIT, fit.Scalar.GRIT.2)

#--Plotting-SEM---#
pdf("GRIT.pdf", height = 12, width = 12, paper = "USr")

semPaths(Two_fac_GRIT_FIT, whatLabels = "std", edge.label.cex = 0.5, layout = "tree",
         sizeMan = 5, style = "mx")
dev.off()
# Scalar in variance failed to achieve and so groups are not comparable 

boxplot(GRIT$testelapse)

#---Survey_Response_Inspection_BIG-Five---# 
Q1_Test_time <- quantile(GRIT$testelapse, .25)
Q3_Test_time <- quantile(GRIT$testelapse, .75)
IQR_Test_time <- IQR(GRIT$testelapse)

GRIT <- subset(GRIT, GRIT$testelapse > (Q1_Test_time - 1.5*IQR_Test_time) & GRIT$testelapse < (Q3_Test_time + 1.5*IQR_Test_time))

five_percentile_big_five <- quantile(GRIT$testelapse, 0.05)
GRIT <- subset(GRIT, GRIT$testelapse > five_percentile_big_five)

boxplot(GRIT$testelapse)


mean(GRIT$testelapse)
median(GRIT$testelapse)

#----Reverse-Scoring-Big-Five----# 

GRIT[,c(44, 46, 48, 50, 52, 54, 56, 63, 65, 67, 69, 74, 
        76, 78, 80, 84, 86, 88)
        ] <- 6 - GRIT[,c(44, 46, 48, 50, 52, 54, 56, 63, 65, 67, 69, 74, 
                         76, 78, 80, 84, 86, 88)]
#----------Cronbach.alpha---------------# 

cronbach.alpha(GRIT[,c(43:52)]) # .891 #Extraversion
cronbach.alpha(GRIT[,c(53:62)]) # .865 #Neuroticism
cronbach.alpha(GRIT[,c(63:72)]) # .820 #Agreeableness
cronbach.alpha(GRIT[,c(73:82)]) # .818 #Conscientiousness 
cronbach.alpha(GRIT[,c(83:91)]) # .755 #Intellect

#---------Omega----------#
omega(GRIT[,c(43:52)]) # .91 
omega(GRIT[,c(53:62)]) # .86
omega(GRIT[,c(63:72)]) # .87
omega(GRIT[,c(73:82)]) # .85
omega(GRIT[,c(83:92)]) # .84

#---CFA---# 
Five_fac_big_five <- 'Extraversion =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
                      Neuroticism =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
                      Agreeablness =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
                      Conscientiousness =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
                      Intellect =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10 ' 

Five_fac_big_five_fit <- cfa(Five_fac_big_five, data = GRIT, estimator = "WLSMV", std.lv = T)
summary(Five_fac_big_five_fit, standardized = T, fit.measures = T)

#-----Measurement_Invariance-----#
semPaths(Five_fac_big_five_fit, whatLabels = "std", edge.label.cex = 1, layout = "tree")

cfa(Five_fac_big_five_fit, data = GRIT, meanstructure = T, group = "gender")
