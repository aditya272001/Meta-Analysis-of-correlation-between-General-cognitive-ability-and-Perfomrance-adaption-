# Meta Analysis of correlation between General cognitive ability and Perfomrance adaption 

In this project, I have used a pre-existining uploaded dataset from r-package "psymetadata"(Rodriguez & Willams, 2021)
This dataset is retrived from (Stasielowicz, 2020), it contains pooled studies on correlation between cognitive 
ability and performance adaption. For this project, I have used 42 studies that looks onto general constructs to measure cognitive 
ability and perfomance adaption. I have also ran a meta-regression with type of tests as moderator to explain the 
existing heterogenity in the model. 
This meta-analysis used random mixed effects model as there is substanital heteroenity in the observed correlations. 

## Demo code

-------packages------

library(metafor)
library(psychmetadata)
library(tidyverse)
library(metaviz)


#-----------FITTING-META---------------#

FIT.meta <- rma(yi, vi, data = DATA1, method = "REML")
FIT.meta
predict(FIT.meta, digits = 3, transf = transf.ztor)
confint(FIT.meta)
reporter(FIT.meta)

#--------------------------------------#

#---------------FOREST-PLOT-----------------------#
viz_forest(FIT.meta, 
           summary_label = "Summary effect", 
           study_labels = DATA1$author, 
           method = "REML", 
           xlab = "Correlation Coefficient", 
           variant = "classic", 
           annotate_CI = T, 
           table_headers = "Correlation [95% CI]")

#-----------------------------------------------------#

#####-------Cumulative_Correlations_Fishers_Z_transformation_--------######

tmp.fit <- cumul(FIT.meta, order = DATA1$pub_year)
forest(tmp.fit, atransf = exp, header = "Author(s) and Year")

#####______Meta-Regression______######
FIT.reg <- rma(yi, vi, 
               mods = ~ cog_abil_measure, 
               data = DATA1, digits = 3)
FIT.reg

######_______Other_Plots_______#########
