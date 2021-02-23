# first run t-tests

# Welch's t-tests to examine effects of note taking condition in current study
ttest_laplong_objZ <- t.test(objectiveZ ~ laplong, mydata)
ttest_laplong_openZ <- t.test(openZ ~ laplong, mydata)
ttest_laplong_wc <- t.test(wordcount ~ laplong, mydata)
ttest_laplong_vo <- t.test(threegramspercent ~ laplong, mydata)

# Welch's t-tests to examine effects of note taking condition in M&O Study 1
ttest_laplong_objZ_MO <- t.test(ZFindexA ~ condition, MO_Study1)
ttest_laplong_openZ_MO <- t.test(ZCindexA ~ condition, MO_Study1)
ttest_laplong_wc_MO <- t.test(Wcount ~ condition, MO_Study1)
ttest_laplong_vo_MO <- t.test(threeGR ~ condition, MO_Study1)


# now compute replication Bayes Factor

###############################################################################
### Compute Bayes factors after a replication study (t-test) 
### to find out whether the original findings are replicated
### Last revised: 19-7-2013
### Author: Josine Verhagen, A.J.Verhagen@uva.nl
### www.josineverhagen.com
### INPUT:  t original, n original, t replication, n replication
### OUTPUT: Replication Bayes factor, JZS Bayes factors one and two sided,
### Bayes factor Bayarri and Mayoral,  Meta-analysis Bayes factor, 
### prior and posterior distributions for effect size
###############################################################################

# clears workspace:  
#rm(list=ls(all=TRUE)) 

# assumes current working directory is where the 'Repfunctionspack6.R' script is stored
dir <- getwd()
#WB <- "C:/Users/user01/WinBUGS14"


setwd(dir)

# Load and if necessary install the required libraries
#library(R2WinBUGS)  # Only necessary for posterior plots for JZS prior
#library(MCMCpack)   # this library was called in the Rmd
 
# Load the functions necessary to compute the replication prior (file saved in working directory)
# HLU downloaded Repfunctionspack6.R on 19 July 2017
source(paste(dir,'/Repfunctionspack6.R', sep = "")) 

############################
#for a two sample t test: 
############################

## factual-recall performance ##

#Replication study info:
trep <- ttest_laplong_objZ$statistic # t statistic from replication study
n2 <- desc_objZ$"-0.5"$n        # number of subjects in group 1 in the replication study      
m2 <- desc_objZ$"0.5"$n        # number of subjects in group 2 in the replication study

#Original study info: 
desc_objZ_MO <- describeBy(MO_Study1$ZFindexA, 
                        group = MO_Study1$condition)

tobs <- ttest_laplong_objZ_MO$statistic  # t observed original study  
n1 <- desc_objZ_MO$"longhand"$n   # number of subjects in group 1 in the original study
m1 <- desc_objZ_MO$"laptop"$n   # number of subjects in group 2 in the original study
sample  <- 2   # this is a two-sample test so set sample to 2

# Independent JZS Bayes factors in Original and Replication studies, one and two sided: 
# Replication Bayes factors: Is the effect the same as in the original study or zero? 
# Bayarri and Mayoral Bayes Factor: is the replicated effect size equal to the original or different
# Meta-analysis Bayes factor: combining all effect sizes  

BF_objZ <- BFSALL(tobs, trep, n1, n2, m1=m1,m2=m2, sample=sample, Type = "ALL")
#BF_objZ

### Plots for one and two-sided independent JZS Bayes factors: normalized ###
### Savage Dickey Bayes factor output ###
# To make these plots, you need WinBUGS. You can download it: 
# http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/contents.shtml
# Check where the program folder is and insert in as WB= 
# The default is: WB ="c:/Program Files/WinBUGS14"

#WB ="C:/Users/user01/WinBUGS14"

# Original two-sided
#PlotJZS(tobs,n1,m1=m1, sample=sample, WB=WB)
#Replication two-sided 
#PlotJZS(trep, n2, m1 = m2, sample=sample, WB=WB)
#Replication one-sided Upper
#PlotJZS(trep, n2, m1 = m2, sample=sample, side=2, WB=WB)
#Replication one-sided 
#PlotJZS(trep, n2, m2, sample=sample, side=2, WB=WB)

# Replication Bayes Factor + a plot of prior and posterior 
# bayes <- ReplicationBayesfactorNCT(tobs, trep, n1, n2, m1=m1,m2=m2, sample=sample, plot=1, post=1)

# save plot (you can do this for every plot) 
#title <-  paste ("Replication Bayes factor")
#dev.copy(device=png, file= paste(dir,title,".png", sep=""))
#dev.off()




## conceptual-application performance ##

#Replication study info:
trep <- ttest_laplong_openZ$statistic # t statistic from replication study
n2 <- desc_openZ$"-0.5"$n        # number of subjects in group 1 in the replication study      
m2 <- desc_openZ$"0.5"$n        # number of subjects in group 2 in the replication study

#Original study info: 
desc_openZ_MO <- describeBy(MO_Study1$ZCindexA, 
                            group = MO_Study1$condition)

tobs <- ttest_laplong_openZ_MO$statistic  # t observed original study  
n1 <- desc_openZ_MO$"longhand"$n   # number of subjects in group 1 in the original study
m1 <- desc_openZ_MO$"laptop"$n   # number of subjects in group 2 in the original study
sample  <- 2   # this is a two-sample test so set sample to 2

# Independent JZS Bayes factors in Original and Replication studies, one and two sided: 
# Replication Bayes factors: Is the effect the same as in the original study or zero? 
# Bayarri and Mayoral Bayes Factor: is the replicated effect size equal to the original or different
# Meta-analysis Bayes factor: combining all effect sizes  

BF_openZ <- BFSALL(tobs, trep, n1, n2, m1=m1,m2=m2, sample=sample, Type = "ALL")
#BF_openZ

### Plots for one and two-sided independent JZS Bayes factors: normalized ###
### Savage Dickey Bayes factor output ###
# To make these plots, you need WinBUGS. You can download it: 
# http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/contents.shtml
# Check where the program folder is and insert in as WB= 
# The default is: WB ="c:/Program Files/WinBUGS14"

#WB ="C:/Users/user01/WinBUGS14"

# Original two-sided
#PlotJZS(tobs,n1,m1=m1, sample=sample, WB=WB)
#Replication two-sided 
#PlotJZS(trep, n2, m1 = m2, sample=sample, WB=WB)
#Replication one-sided Upper
#PlotJZS(trep, n2, m1 = m2, sample=sample, side=2, WB=WB)
#Replication one-sided 
#PlotJZS(trep, n2, m2, sample=sample, side=2, WB=WB)

# Replication Bayes Factor + a plot of prior and posterior 
# bayes <- ReplicationBayesfactorNCT(tobs, trep, n1, n2, m1=m1,m2=m2, sample=sample, plot=1, post=1)

# save plot (you can do this for every plot) 
#title <-  paste ("Replication Bayes factor")
#dev.copy(device=png, file= paste(dir,title,".png", sep=""))
#dev.off()






## word count ##

#Replication study info:
trep <- ttest_laplong_wc$statistic # t statistic from replication study
n2 <- desc_wc$"-0.5"$n        # number of subjects in group 1 in the replication study      
m2 <- desc_wc$"0.5"$n        # number of subjects in group 2 in the replication study

#Original study info: 
desc_wc_MO <- describeBy(MO_Study1$Wcount, 
                            group = MO_Study1$condition)

tobs <- ttest_laplong_wc_MO$statistic  # t observed original study  
n1 <- desc_wc_MO$"longhand"$n   # number of subjects in group 1 in the original study
m1 <- desc_wc_MO$"laptop"$n   # number of subjects in group 2 in the original study
sample  <- 2   # this is a two-sample test so set sample to 2

# Independent JZS Bayes factors in Original and Replication studies, one and two sided: 
# Replication Bayes factors: Is the effect the same as in the original study or zero? 
# Bayarri and Mayoral Bayes Factor: is the replicated effect size equal to the original or different
# Meta-analysis Bayes factor: combining all effect sizes  

BF_wc <- BFSALL(tobs, trep, n1, n2, m1=m1,m2=m2, sample=sample, Type = "ALL")
#BF_wc


## verbatim overlap ##

#Replication study info:
trep <- ttest_laplong_vo$statistic # t statistic from replication study
n2 <- desc_vo$"-0.5"$n        # number of subjects in group 1 in the replication study      
m2 <- desc_vo$"0.5"$n        # number of subjects in group 2 in the replication study

#Original study info: 
desc_vo_MO <- describeBy(MO_Study1$threeGR, 
                         group = MO_Study1$condition)

tobs <- ttest_laplong_vo_MO$statistic  # t observed original study  
n1 <- desc_vo_MO$"longhand"$n   # number of subjects in group 1 in the original study
m1 <- desc_vo_MO$"laptop"$n   # number of subjects in group 2 in the original study
sample  <- 2   # this is a two-sample test so set sample to 2

# Independent JZS Bayes factors in Original and Replication studies, one and two sided: 
# Replication Bayes factors: Is the effect the same as in the original study or zero? 
# Bayarri and Mayoral Bayes Factor: is the replicated effect size equal to the original or different
# Meta-analysis Bayes factor: combining all effect sizes  

BF_vo <- BFSALL(tobs, trep, n1, n2, m1=m1,m2=m2, sample=sample, Type = "ALL")
#BF_vo





