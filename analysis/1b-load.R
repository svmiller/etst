setwd("~/Dropbox/projects/etst/analysis")

# library(digest)
library(foreign)
library(readxl)
library(car)
library(sqldf)
library(mirt)
library(sbgcop)
library(plyr)
library(broom)
library(DataCombine)
library(dummies)
library(arm)
library(RCurl)
# library(gdata)
library(WDI)
library(countrycode)
library(DAMisc)
library(splines)
library(zoo)
library(reshape2)


WVS <- read.csv("wvs-imputed.csv")

CNTS <- read.csv("~/Dropbox/data/cnts/2012/cnts-2012.csv") # CNTSDATA-2012.xls with CoW country codes. Script available upon request


data <- getURL("https://raw.githubusercontent.com/svmiller/wvsccodes/master/wvs-cow-ccodes-table.csv")
wvsccodes <- read.csv(text = data)

data <- getURL("https://raw.githubusercontent.com/svmiller/imputed-gdp/master/imputed-gdp.csv")
ImpGDP <- read.csv(text = data)

lg <- function(x)c(NA, x[1:(length(x)-1)])

SWIID <- read.dta("~/Dropbox/data/swiid/swiid50.dta")
EPR <- read.dta("~/Dropbox/data/ethnic-power-relations/EPR3CountryNewReduced.dta")
UCDP <- read.csv("~/Dropbox/data/ucdpprio/124922_1onset-conf2014.csv")
PITF <- read.csv("~/Dropbox/data/pitf/pitfworld-1995-2012-styear-stabb.csv")
GS <- read.csv("~/Dropbox/data/prs/prs-govstab-1984-2013.csv")
SFI <- read.csv("~/Dropbox/data/cfsp/SFIv2014.csv")

DPI <- read.dta("~/Dropbox/data/DPI/DPI2015/DPI2015_stata12.dta")

UDS <- read.csv("~/Dropbox/data/uds-extended/uds-extended.csv")

Mindist <- read.csv("~/Dropbox/data/gleditsch/mindist-ddy.csv")
NMC <- read.csv("~/Dropbox/data/MID/NMC/NMC_v4_0.csv")
HA <- read.csv("~/Dropbox/data/huthallee/ha-dir-claim-years.csv")
Rivalry <- read.csv("~/Dropbox/data/thompsondreyer2012hir/thompson-rivalry-years.csv")
Alliance <- read.csv("~/Dropbox/data/MID/alliance/alliance_v4.1_by_directed_yearly.csv")
Terrchange <- read.csv("~/Dropbox/data/MID/terrchange/tc2014.csv")
Mtn <- read.dta("~/Dropbox/data/mountainous/mountainous.dta")
GMLDDY <- read.csv("~/Dropbox/projects/gml-ddy.csv")
CY <- read.dta("~/Dropbox/data/EUGene-output/cy-1816-2008.dta")

Disease <- read.csv("~/Dropbox/data/global-burden-disease/gbd-deaths-cmnnd.csv")

flag_dups <- function(a, b){
  a$dup <- as.numeric(duplicated(a[,b]))
  a$duprev <- as.numeric(duplicated(a[,b], fromLast=TRUE))
  a$dup <- with(a, ifelse(dup == 1 | duprev == 1, 1, 0))
  a$duprev <- NULL
  return(a)
}
