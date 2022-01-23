install.packages("psymetadata", dependencies = T)
library(metafor)
library(psymetadata)
library(tidyverse)
install.packages("metaviz", dependencies = T)
library(metaviz)

data("lowe2020")
data("stasielowicz2020")

table(stasielowicz2020$ca_measure)

DATA <- stasielowicz2020 %>% filter(ca_measure == "General")

#-----------DATA-PREP-------------#
setwd("D:/Meta-analysis")
write.csv(DATA, "Modified_According_to_Group.csv")
META_DATA <- read.csv("D:/Meta-analysis/DATA_Meta_Group_Sort.csv")

DATA1 <- escalc(measure = "ZCOR", ri = yi, ni = n, 
                slab = paste(author, pub_year, sep = ","), data = META_DATA)

DATA1
#---------------------------------#

#-----------FITTING-META---------------#
FIT.meta <- rma(yi, vi, data = DATA1, method = "REML")
FIT.meta
predict(FIT.meta, digits = 3, transf = transf.ztor)
confint(FIT.meta)
reporter(FIT.meta)
#--------------------------------------#

#---------------FOREST-PLOT-----------------------#
pdf("Meta_sem.pdf", paper = "USr", height = 12, width = 12)
viz_forest(FIT.meta, 
           summary_label = "Summary effect", 
           study_labels = DATA1$author, 
           method = "REML", 
           xlab = "Correlation Coefficient", 
           variant = "classic", 
           annotate_CI = T, 
           table_headers = "Correlation [95% CI]")
dev.off()

#-----------Other-Plots----------------------------#
pdf("baujat.pdf", paper = "USr", height = 12, width = 12)
baujat(FIT.meta, symbol = "slab")
dev.off()
#-Funnel-Plot-#
pdf("viz_funnel.pdf", paper = "USr", height = 12, width = 12)
viz_funnel(x = FIT.meta, 
           contours = T, 
           egger = T)
dev.off()
pdf("metafor_funnel.pdf", paper = "USr", height = 12, width = 12)
funnel(DATA1$yi, DATA1$vi, yaxis = "seinv", xlim = c(-3,2), ylim = c(0.0001, 8), 
       xaxs = "i", yaxs = "i", las = 1, level = c(.10, 0.05, 0.01), shade = c("white", "gray55", "gray75"), 
       legend = T, back = "gray90", ylab = "Precision (1/se)", lty = 0)
dev.off()
#----------Meta-Regression-----------------#
FIT.reg <- rma(yi, vi, 
               mods = ~ cog_abil_measure, 
               data = DATA1, digits = 3)
FIT.reg

tribble(~Group, ~r,
        "ASVAB", FIT.reg$b[1], 
        "GMA", FIT.reg$b[1] + FIT.reg$b[2], 
        "IST-2000-R", FIT.reg$b[1] + FIT.reg$b[3], 
        "Wonderlic Personnel Test", FIT.reg$b[1] + FIT.reg$b[4], 
        "Working Memory", FIT.reg$b[1] + FIT.reg$b[5])

pdf("subgroup_meta.pdf", height = 16, width = 12, paper = "USr")
viz_forest(FIT.reg, 
           study_labels = DATA1$author, 
           method = "REML", 
           group = DATA1$cog_abil_measure, 
           summary_label = c("AFQT","ASVAB","GMA","IST-2000-R","Wonderlic Personnel Test", 
                             "Working Memory"), 
           xlab = "Correlation", 
           variant = "classic", 
           text_size = 2, 
           annotate_CI = T)
dev.off()
#------Radial-Plot---------#
pdf("radial_plot.pdf", height = 12, width = 12, paper = "USr")
radial(FIT.meta)
dev.off()

#--------Cumumlative-Plot--------------#
tmp.fit <- cumul(FIT.meta, order = DATA1$pub_year)

pdf("cumulative_plot.pdf", height = 12, width = 16, paper = "USr")
forest(tmp.fit, atransf = exp, header = "Author(s) and Year")
dev.off()
