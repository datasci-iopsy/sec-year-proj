## Second Year Project Analysis
## LPA vs Cluster and Justice Perceptions

rm(list = ls()) #clear all content

# set working directory - ctrl + shift + h

library("lessR")
library("tidyverse")
library("psych")

#read in file (skip the second line)
all_content = readLines("./data/proj-data_cln.csv")
     skip_second = all_content[-2]
     dat1 = read.csv(textConnection(skip_second), header=T, stringsAsFactors=T)

#get rid of unnecessary columns
dat1$Att_Chk1 <- dat1$Att_Chk2 <- dat1$Employed <- NULL
rm(all_content, skip_second) #keep environment neat

#Demographic dataset
demo <- as.data.frame(dat1[, c(2:6)], header=T)



#create function to calculate %s for demo variables
percent_fun <- function(x, y) {
     x <- table(x) #provides table of the specific column of dataset
     y <- nrow(demo) #number of obs in dataset
     z <- round(as.matrix(x/y),2)
     return(z)
}

demo.percents <- sapply(demo, percent_fun) #applys function to demo dataset
#**read out csv for each category

#create dataset with scale items only
dat2 <- subset(dat1, 
                  select = -c(Age, Sex, Race, Race_Other, Edu, Industry, header = T))

#this function transforms all factors (responses) into numerics
unfactorise <- function(x) {
     case_when(x %in% c("Strongly disagree", "Very Slightly", 
                        "To a very small extent", "Never", "Very dissatisfied") ~ 1,
               x %in% c("Somewhat disagree", "A Little", "To a small extent", 
                        "Sometimes", "Somewhat dissatisfied") ~ 2,
               x %in% c("Neither agree nor disagree", "Moderately", 
                        "To a moderate extent", "About half the time", 
                        "Neither satisfied nor dissatisfied") ~ 3,
               x %in% c("Somewhat agree", "Quite a Bit", "To a large extent", 
                        "Most of the time", "Somewhat satisfied") ~ 4,
               x %in% c("Strongly agree", "Extremely", "To a very large extent", 
                        "Always", "Very satisfied") ~ 5)
}
dat2 <- as.data.frame(sapply(dat2, unfactorise))

#recode all reverse items; learn what 'grep' does to shorten this
fulldat <- Recode(
     c(BFI_E2_R, BFI_E5_R, BFI_E7_R, BFI_A1_R, BFI_A3_R, BFI_A6_R, BFI_A8_R, 
       BFI_C2_R, BFI_C4_R, BFI_C5_R, BFI_C9_R, BFI_N2_R, BFI_N5_R, BFI_N7_R,
       BFI_O7_R, BFI_O9_R, HOS_R6_R, HOS_S9_R, HOS_S10_R, EQ1_R:EQ7_R, EQ10_R), 
     old = 1:5, new = 5:1, data = dat2)

rm(dat2) #no longer necessary
fulldat <- fulldat[1:118] #only necessary items (i.e., no CWB, satisfaction, etc)

##Confirmatory Factor Analyses
library("lavaan")
library("GPArotation")
#subset variables that will be used in analysis
d_bfi <- subset(fulldat, select = c(BFI_E1:BFI_O10))
d_hos <- subset(fulldat, select = c(HOS_R1:HOS_S10_R))
d_eq <- subset(fulldat, select = c(EQ1_R:EQ16))
d_pan <- subset(fulldat, select = c(PA1:NA10))
d_jus <- subset(fulldat, select = c(JUS_P1:JUS_INF5))

#CFA of only agreeableness and neuroticism; most relevant to study
d_bfi_model <-   'agree =~ BFI_A1_R + BFI_A2 + BFI_A3_R + BFI_A4 + BFI_A5 +
                            BFI_A6_R + BFI_A7 + BFI_A8_R
                   neuro =~ BFI_N1 + BFI_N2_R + BFI_N3 + BFI_N4 + BFI_N5_R + 
                            BFI_N6 + BFI_N7_R + BFI_N8'

d_bfi_fit <- cfa(d_bfi_model, data = d_bfi)
summary(d_bfi_fit, fit.measures=T, standardized=T)
#surprisingly okay and justifable 2 factors

# #CFA on the BFI personality scale (5 factor model)
# d_bfi_model2 <-  'extra =~ BFI_E1 + BFI_E2_R + BFI_E3 + BFI_E4 + BFI_E5_R +
#                   BFI_E6 + BFI_E7_R + BFI_E8
#                   agree =~ BFI_A1_R + BFI_A2 + BFI_A3_R + BFI_A4 + BFI_A5 + 
#                   BFI_A6_R + BFI_A7 + BFI_A8_R + BFI_A9
#                   consc =~ BFI_C1 + BFI_C2_R + BFI_C3 + BFI_C4_R + BFI_C5_R +
#                   BFI_C6 + BFI_C7 + BFI_C8 + BFI_C9_R
#                   neuro =~ BFI_N1 + BFI_N2_R + BFI_N3 + BFI_N4 + BFI_N5_R +
#                   BFI_N6 + BFI_N7_R + BFI_N8
#                   open =~  BFI_O1 + BFI_O2 + BFI_O3 + BFI_O4 + BFI_O5 + BFI_O6 +
#                   BFI_O7_R + BFI_O8 + BFI_O9_R + BFI_O10 '
# 
# d_bfi_fit2 <- cfa(d_bfi_model2, data = d_bfi)
# summary(d_bfi_fit2, fit.measures=T, standardized=T)
# #consider dropping E3, E6, A3_R, A5, 07_R, 09_R* - they have the lowest loadings

d_hos_model <- ' resent =~ HOS_R1 + HOS_R2 + HOS_R3 + HOS_R4 + HOS_R5 + HOS_R6_R 
                         + HOS_R7 + HOS_R8 
                 suspi  =~ HOS_S1 + HOS_S2 + HOS_S3 + HOS_S4 + HOS_S5 + 
                           HOS_S6 + HOS_S7 + HOS_S8 + HOS_S9_R + HOS_S10_R'

d_hos_fit <- cfa(d_hos_model, data = d_hos)
summary(d_hos_fit, fit.measures=T, standardized=T)

d.hos.model2 <- ' resent =~ HOS_R1 + HOS_R2 + HOS_R3 + HOS_R4 + HOS_R5 + HOS_R7 +
                  HOS_R8 
                  suspi  =~ HOS_S1 + HOS_S2 + HOS_S3 + HOS_S4 + HOS_S5 + 
                  HOS_S6 + HOS_S7 + HOS_S8 '

d.hos.fit2 <- cfa(d.hos.model2, data = d_hos)
summary(d.hos.fit2, fit.measures=T, standardized=T)

mEQ1 <- (d.eq$EQ7_R + d.eq$EQ6_R + d.eq$EQ16 + d.eq$EQ13) / 4
mEQ2 <- (d.eq$EQ11 + d.eq$EQ4_R + d.eq$EQ8) / 3
mEQ3 <- (d.eq$EQ14 + d.eq$EQ2_R + d.eq$EQ12) / 3
mEQ4 <- (d.eq$EQ15 + d.eq$EQ10_R + d.eq$EQ9) / 3
mEQ5 <- (d.eq$EQ1_R + d.eq$EQ3_R + d.eq$EQ5_R) / 3
EQpar <- as.matrix(cbind(mEQ1, mEQ2, mEQ3, mEQ4, mEQ5))

#EQ parceled model
d.eq.model2 <-     ' equity =~ mEQ1 + mEQ2 + mEQ3 + mEQ4 + mEQ5 '

d.eq.fit2 <- cfa(d.eq.model2, data = EQpar)
summary(d.eq.fit2, fit.measures=T, standardized=T)

#Parceling; randomly assign items
mPA1 <- (d.pan$PA5 + d.pan$PA4) / 2
mPA2 <- (d.pan$PA1 + d.pan$PA6) / 2
mPA3 <- (d.pan$PA8 + d.pan$PA10) / 2
mPA4 <- (d.pan$PA9 + d.pan$PA7) / 2
mPA5 <- (d.pan$PA2 + d.pan$PA3) / 2
mNA1 <- (d.pan$NA5 + d.pan$NA7) / 2
mNA2 <- (d.pan$NA2 + d.pan$NA4) / 2
mNA3 <- (d.pan$NA3 + d.pan$NA6) / 2
mNA4 <- (d.pan$NA8 + d.pan$NA10) / 2
mNA5 <- (d.pan$NA9 + d.pan$NA1) / 2
PANASpar <- as.matrix(cbind(mPA1, mPA2, mPA3, mPA4, mPA5,
                            mNA1, mNA2, mNA3, mNA4, mNA5))

#PANAS parceled model
d.pan.model2 <- '   pos =~ mPA1 + mPA2 + mPA3 + mPA4 + mPA5
neg =~ mNA1 + mNA2 + mNA3 + mNA4 + mNA5 '

d.pan.fit2 <- cfa(d.pan.model2, data = PANASpar)
summary(d.pan.fit2, fit.measures=T, standardized=T)

d.jus.model <-    ' proc =~ JUS_P1 + JUS_P2 + JUS_P3 + JUS_P4 + JUS_P5 + JUS_P6
                    + JUS_P7
dist =~ JUS_D1 + JUS_D2 + JUS_D3 + JUS_D4
inter =~ JUS_INT1 + JUS_INT2 + JUS_INT3 + JUS_INT4
infor =~ JUS_INF1 + JUS_INF2 + JUS_INF3 + JUS_INF4 + JUS_INF5'

d.jus.fit <- cfa(d.jus.model, data = d.jus)
summary(d.jus.fit, fit.measures=T, standardized=T)
#fits far better than any other scale


##scale scores
SS.agree <- rowMeans(d_bfi[, c(2,7,12,17,22,27,32,42)])
SS.neuro <- rowMeans(d_bfi[, c(4,9,14,19,24,29,34,39)])
SS.open <- rowMeans(d_bfi[, c(5,10,15,20,25,30,40,44)])
SS.extra <- rowMeans(d_bfi[, c(1,6,16,21,31,36)])
SS.consc <- rowMeans(d_bfi[, c(3,8,13,18,23,28,33,38,43)])

#resentment and suspicion
SS.resent <- rowMeans(d.hos[, -c(6, 9:18)])
SS.suspic <- rowMeans(d.hos[, -c(1:8, 17:18)])

#equity sensitivity
SS.eq <- rowMeans(d.eq)

#positive and negative affect
SS.pos <- rowMeans(d.pan[, c(1,3,5,9,10,12,14,16,18,19)])
SS.neg <- rowMeans(d.pan[, c(2,4,6,7,8,11,13,15,17,20)])

#scale scores of justice perceptions
SS.proc <- rowMeans(d.jus[, 1:7])
SS.dist <- rowMeans(d.jus[, 8:11])
SS.inter <- rowMeans(d.jus[, 12:15])
SS.info <- rowMeans(d.jus[, 16:19])

SS.dat <- as.data.frame(cbind(
     SS.agree, SS.neuro, SS.resent, SS.suspic, SS.eq, SS.pos, SS.neg, 
     SS.proc, SS.dist, SS.inter, SS.info))

##LPA##

library(mclust)

#subset predictors - agreeableness, neuroticism, hostility, ES, & PANAS
clus.vars <- SS.dat[, c(1:7)]

#subset outcomes - PJ, DJ, IntJ, and InfJ
out.vars <- SS.dat[, c(8:11)]
class <- out.vars
X <- clus.vars
#all variables in mclust model
clPairs(clus.vars, colors = "red")


mod = Mclust(clus.vars)
#plot all of the BIC based on components
plot(mod, what = "BIC", ylim = range(mod$BIC[,-(1:2)], na.rm = T),
     legendArgs = list(x = "right"))
#plot(mod, what = "classification")

summary(mod, parameters=T) #see info surrounding model

#adjustedRandIndex(): this is used to compare partitions between classifications

# #Try placing the geometric model properties into 2D space
# drmod <- MclustDR(mod, lambda = 1)
#      summary(drmod)
#      plot(drmod, what = "boundaries")

#this is where we can get the ICL
clus.ICL <- mclustICL(clus.vars)
summary(clus.ICL) #shows the top 3 models according to ICL
# VVE(2) is the best ICL but VVE(3) is not far behind
plot(clus.ICL)

#1 component
mod1 = Mclust(clus.vars, G=1)
summary(mod1, parameters = T)
#plot(mod1, what = "density")

#2 components; best according to ICL
mod2 = Mclust(clus.vars, G=2)
summary(mod2, parameters = T)

#3 components (this is the best chosen from mod)
#the best model of the data, according to BIC - BARELY but go for it
#write out the means for the classes you want
clus.means <- as.matrix(mod$parameters$mean)
write.csv(clus.means, 
          "~/Dropbox/Graduate Courses/Org Psych II/Org Psych II Project/Data Files/Class_3 Means.csv")

#4 components     
mod4 = Mclust(clus.vars, G=4)
summary(mod4, parameters = T)

#provides likelihood ratio tests between # of components; VVE model!
clus.LRT = mclustBootstrapLRT(clus.vars, modelName = "VVE")
summary(clus.LRT) #see the output
plot(clus.LRT, G=1)
plot(clus.LRT, G=2)
plot(clus.LRT, G=3)
plot(clus.LRT, G=4)


boot1 = MclustBootstrap(mod, nboot = 999, type = "bs")
summary(boot1, what = "se")

boot2 = MclustBootstrap(mod2, nboot = 999, type = "bs")
summary(boot2, what = "se")

#weighted liklihood bootstrap
wlboot <- MclustBootstrap(mod, nboot = 999, type = "wlbs")
summary(wlboot, what = "se")

wlboot2 <- MclustBootstrap(mod, nboot = 999, type = "wlbs")
summary(wlboot2, what = "se")

# mod.den <- densityMclust(clus.vars1)
#      summary(mod.den)
#      plot(mod.den, what = "BIC")
#      plot(mod.den, what = "density")


#Create a dataset with cluster classification per participant
Class_3 <- factor(mod$classification, labels = c("Antag", "Accom", "Indif")) #create a column as factor for 3 classes!
out.vars_3 <- cbind(Class_3, out.vars) #adds cluster classification
out.vars_3 #viola!

## Play with some graphs
std_means <- as.matrix(scale(mod$parameters$mean))
colnames(std_means) <- c("Profile 1", "Profile 2", "Profile 3")

colors <- c("red", "yellow", "blue")
traits <- c("Agree", "Neuro", "Resent", "Suspic", "Eq", "Pos")
profiles <- c("Profile 1", "Profile 2", "Proifle 3")

barplot(std_means, main = "Standardized Means for Profiles", names.arg = profiles, 
        xlab = "Profile", ylab = "Mean", col = colors)

#Class_2 <- as.factor(mod2$classification) #create column as factor for 2 classes
#out.vars_2 <- cbind(out.vars, Class_2)

######CLASSSES########
# Class 1: Antagonistic?
#    lowest on agreeableness
#    highest on neuroticism
#    highest on resentment
#    highest on suspicion
#    lowest on equity sensitivity
#    lowest on positive affect
#    highest on negative affect

# Class 2: Accommodating
#    highest on agreeableness (very little compared to 3)
#    lowest on neuroticism
#    lowest on resentment
#    lowest on suspicion
#    middle of equity sensitivity (kind more little than 3)
#    middle of positive affect (kind of more little than 3)
#    lowest on negative affect

#Class 3: Indifferent
#    middle of agreeablness (very little difference to 2)
#    middle of neuroticism
#    middle of resentment
#    middle of suspsicion (little difference to 2)
#    highest of equity sensitivity (little difference to 2)
#    highest on positive affect (little difference to 2)
#    middle on negative affect 
#    
###Outcome Analysis###
library(car)
#library(coefplot)

#Three Classes
leveneTest(SS.proc ~ Class_3) #.4578; does not violate equal variance
proc.aov_3 <- aov(SS.proc ~ Class_3, data = out.vars_3)
summary(proc.aov_3) # p < .001**
#plot(proc.aov_3, 2)
TukeyHSD(proc.aov_3) #1-2, 3-1 significant; 3-2 nonsignificant


#Three classes
leveneTest(SS.dist ~ Class_3) #.87; does not violate equal variance   
dist.aov_3 <- aov(SS.dist ~ Class_3, data = out.vars_3)
summary(dist.aov_3) #p < .001**
plot(dist.aov, 2)
TukeyHSD(dist.aov_3) #1-2, 3-1; 3-2 nonsignificant


#Three classes     
leveneTest(SS.inter ~ Class_3) #.06554; does not violate homoscedasticity
inter.aov_3 <- aov(SS.inter ~ Class_3, data = out.vars_3)
summary(inter.aov_3) #p < .001** 
plot(inter.welch, 2)
TukeyHSD(inter.aov_3) #1-2, 3-1 significant; 3-2 nonsignificant at .0608     

#Three classes
leveneTest(SS.info ~ Class_3) #.07683; does not violate equal variance   
info.aov_3 <- aov(SS.info ~ Class_3, data = out.vars_3)
summary(info.aov_3) # p < .001*
plot(info.aov, 2)
TukeyHSD(info.aov_3) #2-1, 3-1 significant; 3-2 nonsignificant

#outcome means per cluster
#Procedural Justice   
mean(out.vars_3$SS.proc[out.vars_3$Class_3 == 1]) #2.69
mean(out.vars_3$SS.proc[out.vars_3$Class_3 == 2]) #3.40
mean(out.vars_3$SS.proc[out.vars_3$Class_3 == 3]) #3.06

#Distributive Justice
mean(out.vars_3$SS.dist[out.vars_3$Class_3 == 1]) #2.80
mean(out.vars_3$SS.dist[out.vars_3$Class_3 == 2]) #3.55
mean(out.vars_3$SS.dist[out.vars_3$Class_3 == 3]) #3.10

#Interactional Justice
mean(out.vars_3$SS.inter[out.vars_3$Class_3 == 1]) #3.50
mean(out.vars_3$SS.inter[out.vars_3$Class_3 == 2]) #4.27
mean(out.vars_3$SS.inter[out.vars_3$Class_3 == 3]) #4.00

#Informational Justice
mean(out.vars_3$SS.info[out.vars_3$Class_3 == 1]) #3.12
mean(out.vars_3$SS.info[out.vars_3$Class_3 == 2]) #3.95
mean(out.vars_3$SS.info[out.vars_3$Class_3 == 3]) #3.48






