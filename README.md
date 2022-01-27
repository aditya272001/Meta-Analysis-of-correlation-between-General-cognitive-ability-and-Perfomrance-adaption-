# Meta Analysis of correlation between General cognitive ability and Perfomrance adaption 

In this project, I have used a pre-existining uploaded dataset from r-package "psymetadata"(Rodriguez & Willams, 2021)
This dataset is retrived from (Stasielowicz, 2020), it contains pooled studies on correlation between cognitive 
ability and performance adaption. For this project, I have used 42 studies that looks onto general constructs to measure cognitive 
ability and perfomance adaption. I have also ran a meta-regression with type of tests as moderator to explain the 
existing heterogenity in the model. For further robustness to check for bias I have used p-curving method (Simonsohn, Nelson & Simmons, 2014). 
This meta-analysis used random mixed effects model as there is substanital heteroenity in the observed correlations. 
![Screenshot 2022-01-23 225910](https://user-images.githubusercontent.com/96023170/150692971-94472522-a163-4984-87c2-362320e0df76.png)
![Screenshot 2022-01-23 225935](https://user-images.githubusercontent.com/96023170/150692974-d8e5f9e0-ccd8-4bfa-924c-3c3a7f45426a.png)
![Screenshot 2022-01-23 225959](https://user-images.githubusercontent.com/96023170/150692982-f056f32e-423c-494f-821d-9309699d57fb.png)


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
![Screenshot 2022-01-24 002723](https://user-images.githubusercontent.com/96023170/150693653-7a160f63-5e24-4957-80c7-3e55ca8ff48d.png)


#####______Meta-Regression______######
FIT.reg <- rma(yi, vi, 
               mods = ~ cog_abil_measure, 
               data = DATA1, digits = 3)
FIT.reg
![Screenshot 2022-01-24 002757](https://user-images.githubusercontent.com/96023170/150693651-d599aa26-37cf-427c-b929-da97b73cbcd0.png)


######_______Other_Plots_______#########
![Screenshot 2022-01-24 002817](https://user-images.githubusercontent.com/96023170/150693644-29d041f5-703c-4f04-830f-37ea57d07cae.png)

#-------P-curve---------#
library(metacor) 
library(dmetar) 
XP <- metacor( cor = META_DATA$yi, n = n, 
            labels = META_DATA$author, data = META_DATA, 
            fixed = F, random = T, method.tau = "REML", 
            hakn = T, title = "META-FOR-P-CURVING")

pdf("pcurve_meta.pdf", height = 12, width = 16, paper = "USr")
p.curve <- pcurve(XP)
dev.off()
![Screenshot 2022-01-27 221321](https://user-images.githubusercontent.com/96023170/151405070-d172bb9d-3471-4775-8328-ac3ab96e809f.png)


## References
Josue E. Rodriguez [aut, cre], Donald Williams [aut]https://cran.r-project.org/web/packages/psymetadata/index.html

Stasielowicz, L. (2020). How important is cognitive ability when adapting to changes? A meta-analysis of the performance adaptation literature. Personality and Individual Differences, 166, 110178. 

Simonsohn, U., Nelson, L. D., & Simmons, J. P. (2014). P-curve: a key to the file-drawer. Journal of experimental psychology: General, 143(2), 534.


