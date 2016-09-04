# setwd("/scratch1/svmille")
setwd("~/Dropbox/projects/etst/analysis")

library(foreign)
library(car)
library(sqldf)
library(mirt)
library(sbgcop)
library(plyr)
library(broom)
library(DataCombine)

# WVS <- read.dta("WVS_Longitudinal_1981_2014_stata_v2015_04_18.dta", convert.factors = FALSE)
WVS <- read.dta("~/Dropbox/data/wvs/WVS_Longitudinal_1981_2014_stata_v2015_04_18.dta", convert.factors = FALSE)

colnames(WVS) <- tolower(names(WVS))
ncolwvs <- dim(WVS)[2]+1

# Drop the first two waves to speed up computation.
WVS <- subset(WVS, s002 > 2)

# Basic information
WVS$wave <- WVS$s002
WVS$wvsccode <- WVS$s003
WVS$year <- WVS$s020

# Some case exclusions:
# https://www.washingtonpost.com/news/monkey-cage/wp/2014/09/02/world-values-lost-in-translation/
WVS <- WVS[!(WVS$wvsccode == 704 & WVS$wave == 4),] # Vietnam, wave 4
WVS <- WVS[!(WVS$wvsccode == 364 & WVS$wave == 4),] # Iran, wave 4
WVS <- WVS[!(WVS$wvsccode == 8 & WVS$wave == 3),] # Albania, wave 3
WVS <- WVS[!(WVS$wvsccode == 360 & WVS$wave == 4),] # Indonesia, wave 4
# I'm also dropping Iraq from Wave 4 and 5.
WVS <- WVS[!(WVS$wvsccode == 368 & WVS$wave == 4),] # Iraq, wave 4
WVS <- WVS[!(WVS$wvsccode == 368 & WVS$wave == 5),] # Iraq, wave 5

WVS <- subset(WVS, wave <= 5)

# Generate a unique ID for each respondent (i.e. row)
WVS$uid <- seq(1, nrow(WVS))

# Let's isolate the DVs now.
WVS$strongleader <- with(WVS, recode(e114, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"))
WVS$expertdecision <- with(WVS, recode(e115, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"))
WVS$armyrule <- with(WVS, recode(e116, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"))
WVS$havedem <- with(WVS, recode(e117, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"))

# Get the IVs now. Here's what I had prior to review.
WVS$age <- with(WVS, recode(x003, "-5:-1=NA"))
WVS$female <- with(WVS, recode(x001, "-5:-1=NA; 1=0; 2=1"))
WVS$collegeed <- with(WVS, recode(x025, "-5:-1=NA; 1:7=0; 8=1"))
WVS$unemployed <- with(WVS, recode(x028, "-5:-1=NA; 1:6=0; 7=1; 8=0"))
WVS$satisfin <- with(WVS, ifelse(c006 < 0, NA, c006-1))
WVS$ideo <- with(WVS, ifelse(e033 < 0, NA, e033-1))
WVS$socialclass <- with(WVS, recode(x045, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1; 5=0"))
WVS$incscale <- with(WVS, ifelse(x047 < 0, NA, x047-1))
WVS$interestp <- with(WVS, recode(e023, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"))
WVS$postma <- with(WVS, recode(y002, "-5:-1=NA"))

# These will comprise the traditional values variable.
Tradvalues <- with(WVS, data.frame(uid, a040, a042, a029, a039, f120, f063, e018, g006))

Tradvalues$cai <- with(Tradvalues, ifelse(a040 >=0 & a042 >=0 & a029 >= 0 & a039 >= 0,
                            (a029 + a039)-(a040 + a042), NA))
Tradvalues$cai <- Tradvalues$cai * -1
Tradvalues$aj <- with(Tradvalues, ifelse(f120 < 0, NA, (f120 * -1) + 10)) # invert it.
Tradvalues$godimp <- with(Tradvalues, ifelse(f063 < 0, NA, f063-1))
Tradvalues$respauth <- with(Tradvalues, recode(e018, "-5:-1=NA; 1=1; 2=0; 3=-1"))
Tradvalues$natpride <- with(Tradvalues, recode(g006, "-5:-1=NA; 1=3; 2=2; 3=1; 4=0"))

Tradvalues <- with(Tradvalues, data.frame(uid, cai, aj, godimp, respauth, natpride))

Tradvalues$removeme <- with(Tradvalues, ifelse(is.na(cai) & is.na(aj) & is.na(godimp) & is.na(respauth) & is.na(natpride), 1, 0))
Tradvalues <- subset(Tradvalues, removeme == 0)
Tradvalues$removeme <- NULL # Removes just 13 observations

TradM <- mirt(Tradvalues[ ,  2:ncol(Tradvalues)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

tradscores <- fscores(TradM, full.scores = TRUE, full.scores.SE = TRUE)
Tradvalues <- cbind(Tradvalues, tradscores)
Tradvalues <- rename(Tradvalues, c("F1" = "ltradv", "SE_F1" = "se_ltradv"))
Tradvalues <- with(Tradvalues, data.frame(uid, ltradv))

WVS <- join(WVS, Tradvalues, by=c("uid"), type="left", match="first")
rm(Tradvalues, TradM, tradscores)

# R3 wants these variables for some reason.
WVS$agency <- with(WVS, ifelse(a173 < 0, NA, a173-1))

WVS$emancvalues <- WVS$y020
WVS$autonomy <- with(WVS, recode(y021, "-5:-1=NA"))
WVS$equality <- with(WVS, recode(y022, "-5:-1=NA"))
WVS$choice <- with(WVS, recode(y023, "-5:-1=NA"))
WVS$voice <- with(WVS, recode(y024, "-5:-1=NA"))

# duplicate autonomy
Autonomy <- with(WVS, data.frame(uid, autonomy, a029, a034, a042))

Autonomy[,3:ncol(Autonomy)] <- sapply(Autonomy[,3:ncol(Autonomy)],
                                      function(x)ifelse(x<=-1,NA,x))

Autonomy$removeme <- with(Autonomy, ifelse(is.na(a029) & is.na(a034) & is.na(a042), 1, 0))
Autonomy <- subset(Autonomy, removeme == 0)
Autonomy$removeme <- NULL

colnames(Autonomy) <- c("uid", "autonomy", "kid_ind", "kid_imag", "kid_obed")
Autonomy$kid_obed <- with(Autonomy, recode(kid_obed, "1=0;0=1"))

AutM <- mirt(Autonomy[ ,  3:ncol(Autonomy)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

autscores <- fscores(AutM, full.scores = TRUE, full.scores.SE = TRUE)
Autonomy <- cbind(Autonomy, autscores)
Autonomy <- rename(Autonomy, c("F1" = "laut", "SE_F1" = "se_laut"))
with(Autonomy, cor(laut, autonomy, use="complete.obs"))
Autonomy <- with(Autonomy, data.frame(uid, laut))

WVS <- join(WVS, Autonomy, by=c("uid"), type="left", match="first")
WVS$autonomy <- NULL
rm(Autonomy, AutM, autscores)

# duplicate equality

Equality <- with(WVS, data.frame(uid, equality, c001, d059, d060))

Equality[,3:ncol(Equality)] <- sapply(Equality[,3:ncol(Equality)],
                                      function(x)ifelse(x<=-1,NA,x))

Equality$removeme <- with(Equality, ifelse(is.na(c001) & is.na(d059) & is.na(d060), 1, 0))
Equality <- subset(Equality, removeme == 0)
Equality$removeme <- NULL

colnames(Equality) <- c("uid", "equality", "menjob", "menleaders", "boycollege")

EquM <- mirt(Equality[ ,  3:ncol(Equality)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

equscores <- fscores(EquM, full.scores = TRUE, full.scores.SE = TRUE)
Equality <- cbind(Equality, equscores)
Equality <- rename(Equality, c("F1" = "lequ", "SE_F1" = "se_lequ"))
with(Equality, cor(lequ, equality, use="complete.obs"))
Equality <- with(Equality, data.frame(uid, lequ))

WVS <- join(WVS, Equality, by=c("uid"), type="left", match="first")
WVS$equality <- NULL

rm(Equality, EquM, equscores)

# duplicate choice

Choice <- with(WVS, data.frame(uid, choice, f118, f120, f121))

Choice[,3:ncol(Choice)] <- sapply(Choice[,3:ncol(Choice)],
                                  function(x)ifelse(x<=-1,NA,x))

Choice$removeme <- with(Choice, ifelse(is.na(f118) & is.na(f120) & is.na(f121), 1, 0))
Choice <- subset(Choice, removeme == 0)
Choice$removeme <- NULL

colnames(Choice) <- c("uid", "choice", "hj", "aj", "dj")


ChoM <- mirt(Choice[ ,  3:ncol(Choice)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

choscores <- fscores(ChoM, full.scores = TRUE, full.scores.SE = TRUE)
Choice <- cbind(Choice, choscores)
Choice <- rename(Choice, c("F1" = "lcho", "SE_F1" = "se_lcho"))
with(Choice, cor(lcho, choice, use="complete.obs"))
Choice <- with(Choice, data.frame(uid, lcho))

WVS <- join(WVS, Choice, by=c("uid"), type="left", match="first")
WVS$choice <- NULL

rm(Choice, ChoM, choscores)

# duplicate voice

Voice <- with(WVS, data.frame(uid, voice, e001, e002, e003, e004))

Voice[,3:ncol(Voice)] <- sapply(Voice[,3:ncol(Voice)],
                                function(x)ifelse(x<=-1,NA,x))

Voice$acsay <- NA
Voice$acsay <- with(Voice, ifelse(e001 == 3, 2, acsay))
Voice$acsay <- with(Voice, ifelse(e002 == 3, 1, acsay))
Voice$acsay <- with(Voice, ifelse(e001 != 3 & e002 != 3 & !is.na(e001), 0, acsay))

Voice$apsay <- NA
Voice$apsay <- with(Voice, ifelse((e003 == 2  & e004 == 4) | (e003 == 4  & e004 == 2),
                                  3, apsay))
Voice$apsay <- with(Voice, ifelse((e003 == 2  & e004 != 4) | (e003 == 4  & e004 != 2),
                                   2, apsay))
Voice$apsay <- with(Voice, ifelse((e003 != 2  & e004 == 4) | (e003 != 4  & e004 == 2),
                                  1, apsay))
Voice$apsay <- with(Voice, ifelse((e003 != 2  & e004 != 4) & (e003 != 4  & e004 != 2),
                                  0, apsay))


Voice$removeme <- with(Voice, ifelse(is.na(acsay) & is.na(apsay), 1, 0))
Voice <- subset(Voice, removeme == 0)
Voice$removeme <- NULL

VoiM <- mirt(Voice[ ,  7:ncol(Voice)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

voiscores <- fscores(VoiM, full.scores = TRUE, full.scores.SE = TRUE)
Voice <- cbind(Voice, voiscores)
Voice <- rename(Voice, c("F1" = "lvoi", "SE_F1" = "se_lvoi"))
with(Voice, cor(lvoi, voice, use="complete.obs"))
Voice <- with(Voice, data.frame(uid, lvoi))

WVS <- join(WVS, Voice, by=c("uid"), type="left", match="first")
WVS$voice <- NULL

rm(Voice, VoiM, voiscores)

# duplicate emancvalues

Emanc <- with(WVS, data.frame(uid, emancvalues, laut, lequ, lcho, lvoi))
Emanc$lemanc <- with(Emanc, (1/4)*(laut + lequ + lcho + lvoi))

with(Emanc, cor(emancvalues, lemanc, use="complete.obs"))

A1 <- lm(lemanc ~ lequ + lcho + lvoi, data=Emanc) # missing laut
A2 <- lm(lemanc ~ laut + lcho + lvoi, data=Emanc) # missing lequ
A3 <- lm(lemanc ~ laut + lequ + lvoi, data=Emanc) # missing lcho
A4 <- lm(lemanc ~ laut + lequ + lcho, data=Emanc) # missing lvoi
A1df <- tidy(A1)
A2df <- tidy(A2)
A3df <- tidy(A3)
A4df <- tidy(A4)

Emanc$lemanc <- with(Emanc, ifelse(is.na(laut) & is.na(lemanc),
                                   A1df[1,2] + A1df[2,2]*lequ +
                                     A1df[3,2]*lcho + A1df[4,2]*lvoi, lemanc))

Emanc$lemanc <- with(Emanc, ifelse(is.na(lequ) & is.na(lemanc),
                                   A2df[1,2] + A2df[2,2]*laut +
                                     A2df[3,2]*lcho + A2df[4,2]*lvoi, lemanc))

Emanc$lemanc <- with(Emanc, ifelse(is.na(lcho) & is.na(lemanc),
                                   A3df[1,2] + A3df[2,2]*laut +
                                     A3df[3,2]*lequ + A3df[4,2]*lvoi, lemanc))

Emanc$lemanc <- with(Emanc, ifelse(is.na(lvoi) & is.na(lemanc),
                                   A4df[1,2] + A4df[2,2]*laut +
                                     A4df[3,2]*lequ + A4df[4,2]*lcho, lemanc))

Emanc <- with(Emanc, data.frame(uid, lemanc))

WVS <- join(WVS, Emanc, by=c("uid"), type="left", match="first")
WVS$emancvalues <- NULL

rm(A1, A1df, A2, A2df, A3, A3df, A4, A4df, Emanc)


# Subset to just the columns we need. Also move uid around.
WVS <- WVS[ ,c(2:ncol(WVS), 1)]
WVS <- WVS[ ,  ncolwvs:ncol(WVS)]
WVS <- MoveFront(WVS, "uid")

write.table(WVS,file="wvs-unimputed.csv",sep=",",row.names=F,na="")

# Copula imputation

Copimp <- sbgcop.mcmc(WVS[, 5:ncol(WVS)], nsamp=100, seed=8675309)
Impmeans <- data.frame(Copimp$Y.pmean)
Full <- cbind(WVS[1:4],Impmeans)
Full[,5:18] <-round(Full[,5:18],0)
Full[,20] <-round(Full[,20],0)
write.table(Full,file="wvs-imputed.csv",sep=",",row.names=F,na="")
