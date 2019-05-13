################################################
##### Social Ecology of Gods' Minds: R Code ####
################################################

## Script prepared by Benjamin Grant Purzycki
## email: benjamin_purzycki@eva.mpg.de
## Last updated May 13, 2019

# Install and load packages

libs <- c("AnthroTools" ,"plyr", "psych", "lme4", "brms", "tidyr", "GPArotation", "extrafont")
lapply(libs, require, character.only = TRUE)

# Set working directory

setwd("")

data_CLEAN <- read.csv("data_CLEAN.csv", na = "NA") # Call up and rename data

exponly <- subset(data_CLEAN, !TG_DV%in%(0)) # Remove people who kept the initial endowment

m <- data.frame(exponly)

###################################
## Recoding Conditions ############
###################################

# original: 1 = defected on, 2 = fair, 3 = no answer until FL done, 4 = no Trust Game

# Note that the data output requires that we recode the condition values based on values in the "defect"
# variable where 1 = defected on and 0 = fair split

# Recode with strings

m$CondText <- NA
m$CondText[m$Condition==1 & m$defect==1] <- 'defect' # when condition = 1 or 2 and defect = 1, then players lost money
m$CondText[m$Condition==1 & m$defect==0] <- 'split' # when condition = 1 or 2 and defect = 0, then players got even split
m$CondText[m$Condition==2 & m$defect==1] <- 'defect' 
m$CondText[m$Condition==2 & m$defect==0] <- 'split'
m$CondText[m$Condition==3] <- 'delay' # note that defect does vary here as well
m$CondText[m$Condition==4] <- 'nogame'

# Example for creating new dummies from data rather than text
# Dummy 1: 0 = no treatment; 1 = split; 2 = defected on; NA = no game

# m$CondDummy1 <- NA # create an empty vector
# m$CondDummy1[m$Condition==1 & m$defect==1] <- 2 # when condition = 1 or 2 and defect = 1, then players lost money
# m$CondDummy1[m$Condition==1 & m$defect==0] <- 1 
# m$CondDummy1[m$Condition==2 & m$defect==1] <- 2 # when condition = 1 or 2 and defect = 0, then players got even split
# m$CondDummy1[m$Condition==2 & m$defect==0] <- 1
# m$CondDummy1[m$Condition>=3] <- 0 # when condition = 3, then free-lists were not asked after outcome

m$bindummy1 <- NA # create a dummy vector for playing a game at all
m$bindummy1[m$CondText=='defect'] <- 1 
m$bindummy1[m$CondText=='delay'] <- 1 
m$bindummy1[m$CondText=='split'] <- 1 
m$bindummy1[m$CondText=='nogame'] <- 0

m$bindummy2 <- NA # create a dummy vector for just the greedy and control conditions
m$bindummy2[m$CondText=='defect'] <- 1 
m$bindummy2[m$CondText=='nogame'] <- 0

##############################
### Transforming Variables ###
##############################

m$Age.C <- m$age - mean(m$age) # Center age

m$FISCAL <- m$FC - 4 # Center fiscal conservativism
m$SOCIAL <- m$SC - 4 # Center social conservativism
plot(m$SOCIAL~jitter(m$FISCAL,2))
abline(lm(m$SOCIAL~m$FISCAL))
cor.test(m$FC, m$SC)
mcon <- lm(FISCAL ~ SOCIAL, data = m)
mcon
confint(mcon)
m$CONSERVATIVE <- (m$FISCAL + m$SOCIAL)/2 # average convervativism
m$FISCON <- (m$FC + m$SC)/2

m$Sex <- NA # Recode sex
m$Sex[m$gen==2] <- 0
m$Sex[m$gen==1] <- 1

rel <- data.frame(cbind(m$relig_service,m$pray,m$relig_text,m$god_belife,m$relig_tradition)) # Check religiosity scale
psych::alpha(rel)
omega(rel)
m$RELIGIOSITY <- (m$relig_service + m$pray + m$relig_text + m$god_belife + m$relig_tradition)/5 # New religiosity var
mean(m$RELIGIOSITY)
sd(m$RELIGIOSITY)

plot(m$FISCON~jitter(m$RELIGIOSITY,2))
abline(lm(m$FISCON~m$RELIGIOSITY))
mrelcon <- lm(FISCON ~ RELIGIOSITY, data = m)
mrelcon
confint(mrelcon)
cor.test(m$FISCON, m$RELIGIOSITY)

m$PASS1 <- 0 # comprehension check dummy coding
m$PASS1[m$Condition==4] <- NA
m$PASS1[m$CompFail1==1 & m$CompFail2==1 & m$CompFail3==3 & m$CompFail4==2] <- 1
m$PASS1[m$CompFail1B==1 & m$CompFail2B==1 & m$CompFail3B==3 & m$CompFail4B==2] <- .5

m$Real.d <- NA # median split for belief that other player was real
m$Real.d[m$Real<=5] <- 1
m$Real.d[m$Real>5] <- 0

write.csv(m, "data.pre.FL.csv") # Save file if you want to

##################################
##### Process free-list data #####
##################################

m <- read.csv("data.pre.FL.csv")

FL <- reshape(m, varying = c("LIST1", "LIST2", "LIST3", "LIST4", "LIST5", "LIST6", "LIST7",
              "LIST8", "LIST9", "LIST10"), timevar = "Order", idvar = "PARTID", 
              direction = "long", sep = "")
rownames(FL) <- NULL
#View(FL)

FL.t <- table(FL$LIST)
#write.csv(FL.t, "FL.t.csv")

# Global Sample
FL.S <- CalculateSalience(FL, Order = "Order", Subj = "PARTID", CODE = "LIST", GROUPING = NA, Salience = "Salience")
SAL <- SalienceByCode(FL.S, CODE = "LIST", Salience = "Salience", Subj = "PARTID", dealWithDoubles = "MAX", GROUPING = NA)
FLSORT <- SAL[order(-SAL$SmithsS),] 
FLSORT$n <- FLSORT$SumSalience/FLSORT$MeanSalience
# FLSORT[which(FLSORT.g$SmithsS > 0.09),] # Table S1

# By Condition

FL.S.g <- CalculateSalience(FL, Order = "Order", Subj = "PARTID", CODE = "LIST", GROUPING = "CondText", Salience = "Salience")
SAL.g <- SalienceByCode(FL.S.g, CODE = "LIST", Salience = "Salience", Subj = "PARTID", dealWithDoubles = "MAX", GROUPING = "CondText")
FLSORT.g <- SAL.g[order(SAL.g$GROUPING, -SAL.g$SmithsS),] 
FLSORT.g$n <- FLSORT.g$SumSalience/FLSORT.g$MeanSalience
# FLSORT.g[which(FLSORT.g$SmithsS > 0.09),] # Table S2

#write.csv(FL.S.g, "FL.S.G.csv")

####################################################################
########## Assessing presence/absence of target items in lists #####
####################################################################

# Dishonesty, Greed, Theft, Selfishness, Love of Money

# Create a matrix of binary presence/absence data

FL.BIN <- FreeListTable(FL.S, CODE = "LIST", Salience = "Salience", Subj = "PARTID", tableType = "PRESENCE", GROUPING = NA)
FL.BIN$Subject <- rownames(FL.BIN)
rownames(FL.BIN) <- NULL
names(FL.BIN)[names(FL.BIN)=="Subject"] <- "PARTID"
#View(FL.BIN)

#write.csv(FL.BIN, "FL.BIN.csv")

# Merge target variables with main data set
#m <- read.csv("data.pre.FL.csv")
#FL.BIN <- read.csv("FL.BIN.csv")

dishonesty <- merge(x = m, y = FL.BIN[ , c("PARTID", "Dishonesty")], by = "PARTID", all.x = TRUE)
greed <- merge(x = dishonesty, y = FL.BIN[ , c("PARTID", "Greed")], by = "PARTID", all.x = TRUE)
theft <- merge(x = greed, y = FL.BIN[ , c("PARTID", "Theft")], by = "PARTID", all.x = TRUE)
selfishness <- merge(x = theft, y = FL.BIN[ , c("PARTID", "Selfishness")], by = "PARTID", all.x = TRUE)
lovemoney <- merge(x = selfishness, y = FL.BIN[ , c("PARTID", "Love of Money")], by = "PARTID", all.x = TRUE)
materialism <- merge(x = lovemoney, y = FL.BIN[ , c("PARTID", "Materialism")], by = "PARTID", all.x = TRUE)
unfairness <- merge(x = materialism, y = FL.BIN[ , c("PARTID", "Unfairness")], by = "PARTID", all.x = TRUE)
untrusting <- merge(x = unfairness, y = FL.BIN[ , c("PARTID", "Untrusting")], by = "PARTID", all.x = TRUE)
cheating <- merge(x = untrusting, y = FL.BIN[ , c("PARTID", "Cheating")], by = "PARTID", all.x = TRUE)
exploitation <- merge(x = cheating, y = FL.BIN[ , c("PARTID", "Exploitation")], by = "PARTID", all.x = TRUE)
merged <- exploitation
merged$X <- NULL

write.csv(merged, "GVPdata.csv") # final working dataset (unstacked)
GVPdata <- merged

####################################
## In-text descriptive statistics ##
####################################

mean(GVPdata$Real, na.rm = T)
sd(GVPdata$Real, na.rm = T)
hist(GVPdata$Real)
table(GVPdata$CondText, GVPdata$Real)

table(GVPdata$CondText)
table(GVPdata$CondText, GVPdata$Sex) # 0's are females
table(GVPdata$CondText, GVPdata$relig) # 1 = Christian; 2 = Muslim; 3 = Jewish; 4 = Buddhist

describeBy(GVPdata$age, GVPdata$CondText)
describeBy(GVPdata$RELIGIOSITY, GVPdata$CondText)
describeBy(GVPdata$edu, GVPdata$CondText)
describeBy(GVPdata$income, GVPdata$CondText)

table(GVPdata$CondText, GVPdata$Greed)

####################################
###### Flower Plot for FL Data #####
####################################

loadfonts()

# Circle function

circle <- function(xorig, yorig, radius, add, ...){ 
  x <- seq(-radius, radius, length.out = 1000)
  y <- sapply(x, function(z) sqrt(radius^2 - z^2))
  if(add == TRUE){
    lines(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
          type = "l", ...)
  } else {
    plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
         type = "l", ...)
  }
}

FLSORT[1:8,] #View the data for plot

### Empty plot space (for larger monitors)

plot(c(-110, 110), c(-110, 110), type = "n", xlab = "", ylab = "", axes=FALSE, asp=1, family = "Calibri") 

### Circles!
### lwd = Smith's S*10

rad <- 30 # predefined radius

circle(0, 0, rad, add = TRUE, col = "black", lwd = 5.7) # domain circle

circle(0, 80, rad, add = TRUE, col = "black", lwd = 5.7) # top circle
circle(60, 60, rad, add = TRUE, col = "black", lwd = 4.6) # top-right
circle(80, 0, rad, add = TRUE, col = "black", lwd = 4.4) # right circle
circle(60, -60, rad, add = TRUE, col = "black", lwd = 2.4) # lower right
circle(0, -80, rad, add = TRUE, col = "black", lwd = 2.2) # bottom circle
circle(-60, -60, rad, add = TRUE, col = "black", lwd = 2.0) # lower left
circle(-80, 0, rad, add = TRUE, col = "black", lwd = 2.0) # left circle
circle(-60, 60, rad, add = TRUE, col = "black",  lty = 1.8) # top-left

### Connections!
notch <- 50 # length for vertical and horizontal lines
nitch <- 21.5 # length for diagonals
natch <- 38.5 # length for diagonals

segments(0, rad, 0, notch, lwd = 5.7) # top
segments(nitch, nitch, natch, natch, lwd = 4.6) # upper right
segments(rad, 0, notch, 0, lwd = 4.4) # right 
segments(nitch, -nitch, natch, -natch, lwd = 2.4) # lower right
segments(0, -rad, 0, -notch, lwd = 2.2) # bottom
segments(-nitch, -nitch, -natch, -natch, lwd = 2) # lower left
segments(-rad, 0, -notch, 0, lwd = 2) # left
segments(-nitch, nitch, -natch, natch, lty = 1.8) # upper left

### Labels!
text(0, 0, labels = "Angers God", font = 2) # center
text(0, 80, labels = "Dishonesty", font = 2) # top
text(60, 60, labels = "Murder", font = 2) #2 o'clock
text(80, 0, labels = "Theft", font = 2) # right
text(60, -60, labels = "Cheating", font = 2) # 4 o'clock
text(0, -80, labels = "Greed", font = 2) # bottom
text(-60, -60, labels = "Hatred", font = 2) # 8 o'clock
text(-80, 0, labels = "Adultery", font = 2) # 9 o'clock
text(-60, 60, labels = "Violence", font = 2) # 10 o'clock

text(10, rad+10, labels = "0.57", font = 2) #top
text(35, 25, labels = "0.46", font = 2) # 2
text(rad+10, -5, labels = "0.44", font = 2) # left
text(25, -35, labels = "0.24", font = 2) # 4
text(-10, -rad-10, labels = "0.22", font = 2) # bottom
text(-35, -25, labels = "0.20", font = 2) # 7
text(-rad-10, 5, labels = "0.20", font = 2) # right
text(-25, 35, labels = "0.18", font = 2) # 10

####################################
## Reliability check of FL scales ##
####################################

GVPdata <- read.csv("GVPdata.csv")

greedscale <- data.frame(cbind(GVPdata$Dishonesty,GVPdata$Greed,GVPdata$Theft,GVPdata$Selfishness,
                               GVPdata$`Love of Money`,GVPdata$Materialism,GVPdata$Unfairness,
                               GVPdata$Untrusting,GVPdata$Cheating,GVPdata$Exploitation))

myvars <- c("Dishonesty", "Greed", "Theft", "Selfishness", "Love.of.Money", "Materialism", "Unfairness",
            "Untrusting", "Cheating", "Exploitation")
greedscale <- GVPdata[myvars]
psych::alpha(greedscale)
omega(greedscale, title = "Omega Estimates for Free-List Data", rotate = "oblimin", nfactors = 3)
fit <- factanal(greedscale, 3, rotation = "varimax")
print(fit, digits = 2, cutoff = .3, sort = T)
load <- fit$loadings[,1:2]
plot(load, type = "n")
text(load, labels = names(greedscale), cex = .7)

##########################################################
####### Regression Models for Frequency ##################
##########################################################

# Stack data set
GVPdata <- read.csv("GVPdata.csv")
newdata <- gather(GVPdata, item, listed, Dishonesty, Greed, Theft, Selfishness, Love.of.Money, 
                  Materialism, Unfairness, Untrusting, Cheating, Exploitation, factor_key = T)
newdata$X <- NULL
write.csv(newdata, "GVPdata.stacked.csv")

GVPdata <- read.csv("GVPdata.csv", na = "NA")
GVPdata2 <- GVPdata[!is.na(GVPdata$bindummy2), ]
stacked <- read.csv("GVPdata.stacked.csv", na = "NA")
stacked2 <- stacked[!is.na(stacked$bindummy2), ]

#####################################
######## Main-Text Analyses #########
#####################################

set.seed(7) # set seed here and in model (brms appears to ignore the seed values though, 
            # so results will likely be slightly different from reported)

priors <- c(set_prior("normal(0,1)", class = "b"),
            set_prior("cauchy(0,2)", class = "sd"))

priors2 <- set_prior("normal(0,1)", class = "b")

### In main text ###

# Main model

GVPdata$CondRecode <- NA # recode data for model
GVPdata$CondRecode[GVPdata$CondText=='nogame'] <- 0 
GVPdata$CondRecode[GVPdata$CondText=='defect'] <- 1 
GVPdata$CondRecode[GVPdata$CondText=='split'] <- 2 
GVPdata$CondRecode[GVPdata$CondText=='delay'] <- 3 
GVPdata$CondRecode <- as.factor(GVPdata$CondRecode)
GVPdata$Sex <- as.factor(GVPdata$Sex)  
GVPdata$ReligFraim <- as.factor(GVPdata$ReligFraim)  

bfull6 <- brm(formula = Greed ~ CondRecode +  RELIGIOSITY + Sex + Age.C + mo(income) + ReligFraim, 
              data = GVPdata, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

# Stacked model

stacked$CondRecode <- NA # recode data for model
stacked$CondRecode[stacked$CondText=='nogame'] <- 0 
stacked$CondRecode[stacked$CondText=='defect'] <- 1 
stacked$CondRecode[stacked$CondText=='split'] <- 2 
stacked$CondRecode[stacked$CondText=='delay'] <- 3 
stacked$CondRecode <- as.factor(stacked$CondRecode)
stacked$Sex <- as.factor(stacked$Sex)  
stacked$ReligFraim <- as.factor(stacked$ReligFraim) 

lbfull6 <- brm(formula = listed ~ CondRecode +  RELIGIOSITY + Sex + Age.C + mo(income) + ReligFraim + (1 | PARTID), 
               data = stacked, family = binomial(link = logit),
               prior = priors2,
               warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

# Diagnostics

summary(bfull6)
exp(fixef(bfull6))
fixef(bfull6)[1]
c(logistic(fixef(bfull6)[1]), logistic(fixef(bfull6)[1,3]), logistic(fixef(bfull6)[1,4])) # intercept
c(logistic(fixef(bfull6)[2]), logistic(fixef(bfull6)[2,3]), logistic(fixef(bfull6)[2,4])) # Defect condition
c(logistic(fixef(bfull6)[3]), logistic(fixef(bfull6)[3,3]), logistic(fixef(bfull6)[3,4])) # Split condition
c(logistic(fixef(bfull6)[4]), logistic(fixef(bfull6)[4,3]), logistic(fixef(bfull6)[4,4])) # Delay condition

fixef(lbfull6)
exp(fixef(lbfull6))
fixef(lbfull6)[1]
c(logistic(fixef(lbfull6)[1]), logistic(fixef(lbfull6)[1,3]), logistic(fixef(lbfull6)[1,4])) # intercept
c(logistic(fixef(lbfull6)[2]), logistic(fixef(lbfull6)[2,3]), logistic(fixef(lbfull6)[2,4])) # Defect condition
c(logistic(fixef(lbfull6)[3]), logistic(fixef(lbfull6)[3,3]), logistic(fixef(lbfull6)[3,4])) # Split condition
c(logistic(fixef(lbfull6)[4]), logistic(fixef(lbfull6)[4,3]), logistic(fixef(lbfull6)[4,4])) # Delay condition

stanplot(bfull6, type = "hist")
stanplot(lbfull6, type = "hist")

#####################################
###### Supplementary Analyses #######
#####################################

### Robustness checks of greed model

bfull1 <- brm(formula = Greed ~ CondRecode, 
              data = GVPdata, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull2 <- brm(formula = Greed ~ CondRecode +  RELIGIOSITY, 
              data = GVPdata, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull3 <- brm(formula = Greed ~ CondRecode +  RELIGIOSITY + as.factor(Sex), 
              data = GVPdata, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull4 <- brm(formula = Greed ~ CondRecode +  RELIGIOSITY + as.factor(Sex) + Age.C, 
              data = GVPdata, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull5 <- brm(formula = Greed ~ CondRecode +  RELIGIOSITY + as.factor(Sex) + Age.C + mo(income), 
              data = GVPdata, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

# Table S3
exp(fixef(bfull5))
exp(fixef(bfull4))
exp(fixef(bfull3))
exp(fixef(bfull2))
exp(fixef(bfull1))

############################################
### Robustness checks of greed set model ###
############################################
              
bfull1s <- brm(formula = listed ~ CondRecode, 
              data = stacked, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull2s <- brm(formula = listed ~ CondRecode +  RELIGIOSITY, 
              data = stacked, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull3s <- brm(formula = listed ~ CondRecode +  RELIGIOSITY + as.factor(Sex), 
              data = stacked, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull4s <- brm(formula = listed ~ CondRecode +  RELIGIOSITY + as.factor(Sex) + Age.C, 
              data = stacked, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

bfull5s <- brm(formula = listed ~ CondRecode +  RELIGIOSITY + as.factor(Sex) + Age.C + mo(income), 
              data = stacked, family = binomial(link = logit),
              prior = priors2,
              warmup = 1000, iter = 2000, chains = 4, control = list(adapt_delta = .99), seed = 7)

#Table S4
exp(fixef(bfull5s))
exp(fixef(bfull4s))
exp(fixef(bfull3s))
exp(fixef(bfull2s))
exp(fixef(bfull1s))
              
#####################################
##### Supplementary Statistics ######
#####################################

# How real were Players B?
mean(GVPdata$Real, na.rm = T)
sd(GVPdata$Real, na.rm = T)
hist(GVPdata$Real)
table(GVPdata$CondText, GVPdata$Real)

# Relationship between fiscal and social conservativism and religiosity
summary(lm(GVPdata$FC~GVPdata$SC))
confint(lm(GVPdata$FC~GVPdata$SC))
summary(lm(GVPdata$CONSERVATIVE~GVPdata$RELIGIOSITY))
confint(lm(GVPdata$CONSERVATIVE~GVPdata$RELIGIOSITY))
