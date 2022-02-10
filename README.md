
# Psychometric properties of GRIT-O scale

The current project assess the psychometric properties of a widely used 
measure in psychology, GRIT-O (Duckworkth & Quinn, 2009). This measure have been widely used in school settings in order to model the achievement motivation and drive of students. 
This project assess how well the theorized factor structure of GRIT-O scale fits the data retrived from Openpsychometrics. 
To get accurate description of scale outliers in reponse time have been removed. The model shows 
poor to moderate fit, and Scalar invariance failed to hold between genders (Meredith, 1993). The results suggests that GRIT-O scale isn't comparable between groups and 
there is significant bias in the scores for groups. 



## Demo Code 
packages used - 
library(lavaan)

library(semPlot)

library(tidyverse)

library(ltm)

library(semTools)

#---CFA---# 

Two_fac_GRIT <- 'Consistency =~ GS2 + GS3 + GS5 + GS7 + GS8 + GS11
                 Perseverance =~ GS1 + GS4 + GS6 + GS9 + GS10 + GS12
                 Consistency ~~ Perseverance'

Two_fac_GRIT_FIT <- cfa(Two_fac_GRIT, std.lv = T, data = GRIT, estimator = "WLSMV")

- Picture 

lavTestLRT(fit.Configural.GRIT, fit.Metric.GRIT, fit.Scalar.GRIT, fit.Residual.GRIT)

- Picture 

lavTestLRT(fit.Configural.GRIT, fit.Metric.GRIT, fit.Scalar.GRIT, fit.Scalar.GRIT.2)

#------Plotting------#
semPaths(Two_fac_GRIT_FIT, whatLabels = "std", edge.label.cex = 0.5, layout = "tree",
         sizeMan = 5, style = "mx")
## References

 Duckworth, A. L., & Quinn, P. D. (2009). Development and validation of the Short Grit Scale (GRIT–S). Journal of personality assessment, 91(2), 166-174.

 Meredith, W. (1993). Measurement invariance, factor analysis and factorial invariance. Psychometrika, 58(4), 525-543.