# 2) Let's now work on the macro frame.
# -------------------------------------

WVS <- join(WVS, wvsccodes, by=c("wvsccode"), type="left", match="first")

# Some case changes
WVS <- subset(WVS, !is.na(ccode)) # Get rid of HK, PR, and Palestine
WVS$ccode[WVS$ccode == 341 & WVS$year < 2006] <- 345 # Montenegro changes.
WVS$country[WVS$ccode == 345] <- "Serbia"

Macro <- unique(sqldf("select country, ccode, year, wave from WVS"))

# Start with economic threat index first.

# Exp log GDP per capita
ImpGDPs <- with(ImpGDP, data.frame(ccode, year, explogrgdppc))
# ImpGDP$explogrgdppc <- exp(ImpGDP$explogrgdppc)
# ImpGDP$explogwbgdppc <- ImpGDP$explogrgdppc
#ImpGDPs <- rename(ImpGDPs, c(explogrgdppc="explogwbgdppc"))


ImpGDPs <- ddply(ImpGDPs, ~ccode, transform, l1_lrgdppc = lg(explogrgdppc))
ImpGDPs <- ddply(ImpGDPs, ~ccode, transform, l2_lrgdppc = lg(l1_lrgdppc))
ImpGDPs <- ddply(ImpGDPs, ~ccode, transform, l3_lrgdppc = lg(l2_lrgdppc))
ImpGDPs <- ddply(ImpGDPs, ~ccode, transform, l4_lrgdppc = lg(l3_lrgdppc))
ImpGDPs <- ddply(ImpGDPs, ~ccode, transform, l5_lrgdppc = lg(l4_lrgdppc))

ImpGDPs$lrgdppc_5yd <- with(ImpGDPs, l5_lrgdppc - l1_lrgdppc)
ImpGDPs$lrgdppc_4yd <- with(ImpGDPs, l4_lrgdppc - l1_lrgdppc)
ImpGDPs$lrgdppc_3yd <- with(ImpGDPs, l3_lrgdppc - l1_lrgdppc)

ImpGDPs$lrgdppc_5yd <- with(ImpGDPs, 
                            ifelse(is.na(lrgdppc_5yd), lrgdppc_4yd, lrgdppc_5yd))
ImpGDPs$lrgdppc_5yd <- with(ImpGDPs, 
                            ifelse(is.na(lrgdppc_5yd), lrgdppc_3yd, lrgdppc_5yd))

ImpGDPs <- subset(ImpGDPs, select=c("ccode","year","l1_lrgdppc","lrgdppc_5yd"))

Macro <- join(Macro, ImpGDPs, by=c("ccode", "year"), type="left", match="first")

# Get inflation data.
# -------------------

CPI <- WDI(country="all", indicator=c("FP.CPI.TOTL.ZG"), start=1991, end=2014)
# Inflation, consumer prices (annual %) http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG

CPI <- rename(CPI, c(FP.CPI.TOTL.ZG ="cpi"))

CPI$ccode <- countrycode(CPI$iso2c, "iso2c", "cown")
CPI$ccode[CPI$iso2c == "RS"] <- 345

CPI <- subset(CPI, !is.na(ccode))

CPI$country <- CPI$iso2c <- NULL

CPI <- CPI[order(CPI$ccode, CPI$year), ]

CPI <- ddply(CPI, ~ccode, transform, l1_cpi = lg(cpi))

CPI$cpi <- NULL

Macro <- join(Macro, CPI, by=c("ccode", "year"), type="left", match="first")

# Fill in some missing values.
Macro$l1_cpi[Macro$ccode == 346 & Macro$year == 1998] <- 5 # Bosnia in 1998 http://www.indexmundi.com/g/g.aspx?v=71&c=bk&l=en
Macro$l1_cpi[Macro$ccode == 346 & Macro$year == 2001] <- 4.973 # Bosnia in 2001, via IMF 
Macro$l1_cpi[Macro$ccode == 232 & Macro$year == 2005] <- 3.4 # Andorra in 2005, http://www.indexmundi.com/g/g.aspx?c=an&v=71
Macro$l1_cpi[Macro$ccode == 713 & Macro$year == 1994] <- 2.942 # Taiwan in 1994, via index mundi IMF
Macro$l1_cpi[Macro$ccode == 713 & Macro$year == 2006] <- 2.3 # Taiwan in 2006 http://www.indexmundi.com/g/g.aspx?v=71&c=tw&l=en
Macro$l1_cpi[Macro$ccode == 704 & Macro$year == 2011] <- 9.38 # Uzbekistan in 2010 http://www.indexmundi.com/uzbekistan/inflation_rate_%28consumer_prices%29.html
Macro$l1_cpi[Macro$ccode == 660 & Macro$year == 2013] <- 6.581 # Lebanon in 2012 http://www.indexmundi.com/lebanon/inflation_rate_%28consumer_prices%29.html

mininf <- abs(min(Macro$l1_cpi, na.rm = TRUE)) + 1
Macro$logcpi <- log(Macro$l1_cpi + mininf)
summary(Macro)

# Get GINI data.
# --------------

SWIID$gini <- with(SWIID, rowMeans(SWIID[,4:103]))

SWIID$ccode <- countrycode(SWIID$country, "country.name", "cown")

SWIID$ccode[SWIID$country == "Serbia"] <- 345
SWIID$ccode[SWIID$country == "Serbia and Montenegro"] <- 345
SWIID$country <- NULL
SWIID$year <- SWIID$year + 1

SWIID <- with(SWIID, data.frame(ccode, year, gini))

Macro <- join(Macro, SWIID, by=c("ccode", "year"), type="left", match="first")
# Macro <- rename(Macro, c(gini_net="gini"))

Macro$gini[Macro$ccode == 232 & Macro$year == 2005] <- 28.22657 # This is what the value was in 2003.
Macro$gini[Macro$ccode == 645 & Macro$year == 2004] <- 34.63313 # This is what the value was in 2003, v 4_1 of the data.
Macro$gini[Macro$ccode == 645 & Macro$year == 2006] <- 35.30008 # This is what the value was in 2004, v 4_1 of the data.
Macro$gini[Macro$ccode == 670 & Macro$year == 2003] <- 32 # No confidence in this one. Comes from Global Peace Index estimate.

# Unemployment rate.
# ------------------

Unemp <- WDI(country="all", indicator=c("SL.UEM.TOTL.ZS"), start=1991, end=2015)
# Unemployment, total (% of total labor force) (modeled ILO estimate) | http://data.worldbank.org/indicator/SL.UEM.TOTL.ZS?page=1
summary(Unemp)
Unemp <- rename(Unemp, c(SL.UEM.TOTL.ZS="unemploymentrate"))

Unemp$ccode <- countrycode(Unemp$iso2c, "iso2c", "cown")
Unemp$ccode[Unemp$iso2c == "RS"] <- 345

# write.table(Macro,file="balls.csv",sep=",",row.names=F,na="")

Unemp$country <- Unemp$iso2c <- NULL

# Quick and dirty year lag.
Unemp$year <- Unemp$year + 1

Macro <- join(Macro, Unemp, by=c("ccode", "year"), type="left", match="first")

# Fill in some missing values.
Macro$unemploymentrate[Macro$ccode == 232 & Macro$year == 2005] <- .7 # Super rough: http://jdsurvey.net/jds/jdsurveyAnalisis.jsp?ES_COL=131&Idioma=I&SeccionCol=05&ESID=501
Macro$unemploymentrate[Macro$ccode == 713 & Macro$year == 1994] <- 1.45 # http://www.indexmundi.com/taiwan/unemployment_rate.html
Macro$unemploymentrate[Macro$ccode == 713 & Macro$year == 2006] <- 4.13 # http://www.indexmundi.com/taiwan/unemployment_rate.html
Macro$unemploymentrate[Macro$ccode == 713 & Macro$year == 2012] <- 4.4 # http://www.focus-economics.com/country-indicator/taiwan/unemployment

# Create index

Macro$z_lrgdppc <- with(Macro, rescale(l1_lrgdppc))
Macro$z_lrgdppc <- Macro$z_lrgdppc * -1
Macro$z_gdploss <- with(Macro, rescale(lrgdppc_5yd))
Macro$z_cpi <- with(Macro, rescale(logcpi))
Macro$z_gini <- with(Macro, rescale(gini))
Macro$z_unemploy <- with(Macro, rescale(unemploymentrate))


Macro <- ddply(Macro, c("wave"), transform, z_lrgdppc = arm::rescale(l1_lrgdppc))
Macro$z_lrgdppc <- Macro$z_lrgdppc * -1
Macro <- ddply(Macro, c("wave"), transform, z_gdploss = arm::rescale(lrgdppc_5yd))
Macro <- ddply(Macro, c("wave"), transform, z_cpi = arm::rescale(l1_cpi))
Macro <- ddply(Macro, c("wave"), transform, z_gini = arm::rescale(gini))
Macro <- ddply(Macro, c("wave"), transform, z_unemploy = arm::rescale(unemploymentrate))

Macro$eti <- with(Macro, (1/5)*(z_lrgdppc + z_gdploss + z_cpi + z_gini + z_unemploy))

ET1 <- lm(eti ~ z_lrgdppc + z_gdploss + z_cpi + z_unemploy, data=Macro) # missing gini
ET1df <- tidy(ET1)

Macro$eti <- with(Macro, ifelse(is.na(eti) & is.na(gini),
                                   ET1df[1,2] + ET1df[2,2]*z_lrgdppc + ET1df[3,2]*z_gdploss +
                                   ET1df[4,2]*z_cpi + ET1df[5,2]*z_unemploy, eti))

rm(ET1, ET1df)

Macro <- ddply(Macro, c("wave"), transform, z_eti = arm::rescale(eti))

# 3) Create social threat index
# -----------------------------

CNTS <- subset(CNTS, !(is.na(ccode)))
CNTS <- subset(CNTS, year >= 1945)
CNTS <- CNTS[order(CNTS$ccode, CNTS$year), ]
# CNTS$domestic10 <- CNTS$domestic1 + CNTS$domestic2 + CNTS$domestic3 + CNTS$domestic4 + CNTS$domestic5 + CNTS$domestic6 + CNTS$domestic7 + CNTS$domestic8


WCI <- subset(CNTS, select = c("ccode","year", "domestic9"))
summary(WCI)

# Okay, I think Kosovo and Montenegro are blowing up the five-year sum/average because they don't have many years in the data set (well, Kosovo). Get rid of them.
WCI <- subset(WCI, WCI$ccode != 341)
WCI <- subset(WCI, WCI$ccode != 347)
WCI <- subset(WCI, WCI$ccode != 626)

WCI <- ddply(WCI, ~ccode, transform, l1_domestic9 = lg(domestic9))
WCI <- ddply(WCI, ~ccode, transform, l2_domestic9 = lg(l1_domestic9))
WCI <- ddply(WCI, ~ccode, transform, l3_domestic9 = lg(l2_domestic9))
WCI <- ddply(WCI, ~ccode, transform, l4_domestic9 = lg(l3_domestic9))
WCI <- ddply(WCI, ~ccode, transform, l5_domestic9 = lg(l4_domestic9))

WCI$wci5ya <- with(WCI, 16*(l1_domestic9) + 8*(l2_domestic9) + 4*(l3_domestic9) + 2*(l4_domestic9) + l5_domestic9)
WCI$logwci5ya <- log(WCI$wci5ya + 1)

WCI$wci4ya <- with(WCI, 16*(l1_domestic9) + 8*(l2_domestic9) + 4*(l3_domestic9) + 2*(l4_domestic9))
WCI$logwci4ya <- log(WCI$wci4ya + 1)

WCI$wci3ya <- with(WCI, 16*(l1_domestic9) + 8*(l2_domestic9) + 4*(l3_domestic9))
WCI$logwci3ya <- log(WCI$wci3ya + 1)

WCI$logwci5ya <- ifelse(is.na(WCI$logwci5ya), WCI$logwci4ya, WCI$logwci5ya)
WCI$logwci5ya <- ifelse(is.na(WCI$logwci5ya), WCI$logwci3ya, WCI$logwci5ya)

WCI$wci5ya <- ifelse(is.na(WCI$wci5ya), WCI$wci4ya, WCI$wci5ya)
WCI$wci5ya <- ifelse(is.na(WCI$wci5ya), WCI$wci3ya, WCI$wci5ya)

WCI <- with(WCI, data.frame(ccode, year, logwci5ya, wci5ya))


Macro <- join(Macro, WCI, by=c("ccode", "year"), type="left", match="first")


# Ethnic Power Relations data
# ---------------------------

EPR <- subset(EPR, select=c("cowcode","year","discpop"))
EPR <- rename(EPR, c(cowcode="ccode"))
EPR$year <- EPR$year + 1 # This "pre-lags" the value in question.

Macro <- join(Macro, EPR, by=c("ccode", "year"), type="left", match="first")
# Missingness in Andorra (2005), Cyprus (2006), and Singapore (2002).
# We'll impute Andorra as zero. Cyprus as .24 (Turkish Cypriots, http://en.wikipedia.org/wiki/Demographics_of_Cyprus#Ethnic_groups), and Singapore as 0
Macro$discpop[Macro$ccode == 232 & Macro$year == 2005] <- 0
Macro$discpop[Macro$ccode == 352 & Macro$year == 2006] <- .24
Macro$discpop[Macro$ccode == 352 & Macro$year == 2011] <- .24
Macro$discpop[Macro$ccode == 830 & Macro$year == 2002] <- 0

# UCDP intrastate conflict onset data
# -----------------------------------

UCDP <- subset(UCDP, gwno!=-99)
UCDP$ccode <- UCDP$gwno
UCDP$gwno <- NULL

UCDP$ccode[UCDP$year > 1989 & UCDP$ccode == 678] <- 679
UCDP$ccode[UCDP$ccode == 972] <- 955
UCDP$ccode[UCDP$ccode == 970] <- 946
UCDP$ccode[UCDP$ccode == 971] <- 970
UCDP$ccode[UCDP$ccode == 973] <- 947
UCDP$ccode[UCDP$ccode == 260 & UCDP$year >= 1990] <- 255

UCDP <- subset(UCDP, ccode!=711)

UCDP <- UCDP[order(UCDP$ccode, UCDP$year), ]

UCDP <- subset(UCDP, select = c("ccode","year","newconflictinyearv414"))
UCDP <- UCDP[order(UCDP$ccode, UCDP$year), ]

UCDP$newconflictinyearv414[UCDP$ccode == 2] <- 0

UCDP <- ddply(UCDP, ~ccode, transform, l1_onset = lg(newconflictinyearv414))
UCDP <- ddply(UCDP, ~ccode, transform, l2_onset = lg(l1_onset))
UCDP <- ddply(UCDP, ~ccode, transform, l3_onset = lg(l2_onset))
UCDP <- ddply(UCDP, ~ccode, transform, l4_onset = lg(l3_onset))
UCDP <- ddply(UCDP, ~ccode, transform, l5_onset = lg(l4_onset))

UCDP$ucdpintraonset <- with(UCDP, ifelse(l1_onset == 1 | l2_onset == 1 | l3_onset == 1 
                                         | l4_onset == 1 | l5_onset == 1, 1, 0)) 

UCDP <- with(UCDP, data.frame(ccode, year, ucdpintraonset))

Macro <- join(Macro, UCDP, by=c("ccode", "year"), type="left", match="first")

Macro$ucdpintraonset[Macro$ccode == 232 & Macro$year == 2005] <- 0
Macro$ucdpintraonset[Macro$ccode == 349 & Macro$year == 1995] <- 0

# Instability
# -----------

WVS <- ddply(WVS, c("country", "year"), transform, varideo = var(ideo))

Instab <-  aggregate(cbind(varideo) ~ country + ccode + year + wave, WVS, mean)
SFI$ccode <- countrycode(SFI$country, "country.name", "cown")
SFI$ccode[SFI$country == "Serbia" | SFI$country == "Serbia & Montenegro"] <- 345

SFI <- with(SFI, data.frame(ccode, year, polleg))

Instab <- join(Instab, SFI, by=c("ccode", "year"), type="left", match="first")
# I'll impute Taiwan to be 1 and Andorra to be zero.
Instab$polleg[Instab$ccode == 713 & Instab$year == 1994] <- 1
Instab$polleg[Instab$ccode == 232 & Instab$year == 2005] <- 0

Instab$instab <- with(Instab, (polleg+1)*(varideo))
Instab <- with(Instab, data.frame(ccode, year, instab))


Macro <- join(Macro, Instab, by=c("ccode", "year"), type="left", match="first")

# Create index

Macro$z_logwci5ya <- with(Macro, rescale(logwci5ya))
Macro$z_discpop <- with(Macro, rescale(discpop))
Macro$z_ucdp <- with(Macro, rescale(ucdpintraonset))
Macro$z_instab <- with(Macro, rescale(instab))


Macro <- ddply(Macro, c("wave"), transform, z_logwci5ya = arm::rescale(logwci5ya))
Macro <- ddply(Macro, c("wave"), transform, z_discpop = arm::rescale(discpop))
Macro <- ddply(Macro, c("wave"), transform, z_ucdp = arm::rescale(ucdpintraonset))
Macro <- ddply(Macro, c("wave"), transform, z_instab = arm::rescale(instab))


Macro$sti <- with(Macro, (1/4)*(z_logwci5ya + z_discpop + z_ucdp + z_instab))


# Missingness on discriminated population and govt stability are the biggest offenders.

ST1 <- lm(sti ~ z_logwci5ya + z_ucdp + z_instab, data=Macro) # missing discpop
ST1df <- tidy(ST1)

Macro$sti <- with(Macro, ifelse(is.na(sti) & is.na(z_discpop),
                                ST1df[1,2] + ST1df[2,2]*z_logwci5ya + 
                                  ST1df[3,2]*z_ucdp + ST1df[4,2]*z_instab, sti))


Macro <- ddply(Macro, c("wave"), transform, z_sti = arm::rescale(sti))

# Manipulate UDS data.
# --------------------

UDS$ccode <- UDS$cown
UDS$demest <- UDS$z1
UDS$ccode[UDS$cown == 818] <- 816
UDS$ccode[UDS$cown == 529] <- 530
UDS$ccode[UDS$cown == 347] <- 345

# Basic democracy

Demest <- with(UDS, data.frame(ccode, year, demest))

Macro <- join(Macro, Demest, by=c("ccode", "year"), type="left", match="first")

# Compound interest


comp_imp = function(Deposit, Interest) {
  Balance <- c(0, rep(NA, length(Deposit)))
  for (i in 2:length(Balance)) {
    Balance[i] = (Balance[i-1] + Deposit[i-1]) * (1+Interest[i-1])
  }
  return(Balance[-1])
}

UDS$interest <- .01
UDS <- subset(UDS, !(ccode == 740 & year < 1952))
UDS <- ddply(UDS, ~ ccode, transform, demstock = comp_imp(demest, interest))

Stock <- with(UDS, data.frame(ccode, year, demstock))

Macro <- join(Macro, Stock, by=c("ccode", "year"), type="left", match="first")

# Get external threat measure.
# ----------------------------

Mindist$majpow1 <- NA
Mindist$majpow1 <- with(Mindist, ifelse(ccode1 == 2 | ccode1 == 200 | ccode1 == 220 | ccode1 == 365 |
                                          (ccode1 == 710 & year >= 1950) | (ccode1 == 255 & year >= 1991) |
                                          (ccode1 == 740 & year >= 1991), 1, 0))
Mindist$majpow2 <- NA
Mindist$majpow2 <- with(Mindist, ifelse(ccode2 == 2 | ccode2 == 200 | ccode2 == 220 | ccode2 == 365 |
                                          (ccode2 == 710 & year >= 1950) | (ccode2 == 255 & year >= 1991) |
                                          (ccode2 == 740 & year >= 1991), 1, 0))
Mindist$majpow <- with(Mindist, ifelse(majpow1 == 1 | majpow2 == 1, 1, 0))

DYs <- sqldf("select * from Mindist where mindist < 401") # majpow == 1 OR 

NMCs <- with(NMC, data.frame(ccode, year, milper, tpop))
# NMCs <- rename(NMCs, c(ccode="ccode1"))

interim <- sqldf("select A.ccode ccode1, B.ccode ccode2, A.year, 
             A.milper milper1, B.milper milper2, A.tpop tpop1, B.tpop tpop2
             from DYs join NMCs A join NMCs B 
             on DYs.ccode1 = A.ccode and DYs.year = A.year 
             and DYs.ccode2 = B.ccode and DYs.year = B.year
             order by A.ccode, B.ccode")

DYs <- join(DYs, interim, by=c("ccode1", "ccode2", "year"), type="left", match="first")

rm(interim)

Milper <- WDI(country="all", indicator=c("MS.MIL.TOTL.P1", "SP.POP.TOTL"), 
              start=1960, end=2010)
Milper <- rename(Milper, c(MS.MIL.TOTL.P1="milper", SP.POP.TOTL="tpop"))

Milper$ccode <- countrycode(Milper$iso2c, "iso2c", "cown")
Milper$ccode[Milper$iso2c == "RS"] <- 345

Milper$country <- Milper$iso2c <- NULL

Milper <- subset(Milper, !is.na(ccode) & year > 2007)
Milper$milper <- with(Milper, ifelse(is.na(milper), 0, milper))
Milper$milper <- with(Milper, round(milper/1000))
Milper$tpop <- with(Milper, round(tpop/1000))

interim <- sqldf("select A.ccode ccode1, B.ccode ccode2, A.year, 
                 A.milper milper1, B.milper milper2, A.tpop tpop1, B.tpop tpop2
                 from DYs join Milper A join Milper B 
                 on DYs.ccode1 = A.ccode and DYs.year = A.year 
                 and DYs.ccode2 = B.ccode and DYs.year = B.year
                 order by A.ccode, B.ccode")

DYs <- DYs[order(DYs$ccode1, DYs$ccode2, DYs$year),]
r = merge(DYs, interim, by=c("ccode1", "ccode2", "year"), suffixes=c(".Data", ".Donor"), all.x = TRUE)
DYs$milper1 = ifelse(is.na(DYs$milper1), r$milper1.Donor, DYs$milper1)
DYs$milper2 = ifelse(is.na(DYs$milper2), r$milper2.Donor, DYs$milper2)
DYs$tpop1 = ifelse(is.na(DYs$tpop1), r$tpop1.Donor, DYs$tpop1)
DYs$tpop2 = ifelse(is.na(DYs$tpop2), r$tpop2.Donor, DYs$tpop2)
rm(r, interim, NMCs)

DYs$militarization <- with(DYs, (milper1/tpop1) - (milper2/tpop2))

# Huth and Allee claim data.

HA$terrclaim <- 1

HAs <- with(HA, data.frame(ccode1, ccode2, year, terrclaim))
HAs <- rename(HAs, c(ccode1="ccode2", ccode2="ccode1"))
HAs <- HAs[ ,c(2,1,3,4)]

DYs <- join(DYs, HAs, by=c("ccode1", "ccode2", "year"), type="left", match="first")
DYs$terrclaim <- with(DYs, ifelse(is.na(terrclaim), 0, terrclaim))

rm(HAs)

# Rivalry data

Rivalrys1 <- with(Rivalry, data.frame(ccode1, ccode2, year, type1))
Rivalrys1 <- rename(Rivalrys1, c(ccode1="ccode2", ccode2="ccode1"))
Rivalrys2 <- with(Rivalry, data.frame(ccode1, ccode2, year, type1))

Rivalrys <- rbind(Rivalrys2, Rivalrys1)
rm(Rivalrys1, Rivalrys2)
Rivalrys$spatrivalry <- with(Rivalrys, ifelse(type1 == "spatial", 1, 0))
Rivalrys$type1 <- NULL

DYs <- join(DYs, Rivalrys, by=c("ccode1", "ccode2", "year"), type="left", match="first")
DYs$spatrivalry <- with(DYs, ifelse(is.na(spatrivalry), 0, spatrivalry))

rm(Rivalrys)

# Alliance data

Alliance$allied <- with(Alliance, ifelse(defense == 1 | neutrality == 1 | nonaggression == 1 | entente == 1, 1, 0))

Alliances <- with(Alliance, data.frame(ccode1, ccode2, year, allied))
DYs <- join(DYs, Alliances, by=c("ccode1", "ccode2", "year"), type="left", match="first")
DYs$allied <- with(DYs, ifelse(is.na(allied), 0, allied))

rm(Alliances)

# Territorial change data

Terrchanges <- with(Terrchange, data.frame(gainer, loser, year, conflict))
Terrchanges <- subset(Terrchanges, gainer != -9 )
Terrchanges <- subset(Terrchanges, loser != -9 )
Terrchanges <- subset(Terrchanges, conflict == 1)

Terrchanges <- rename(Terrchanges, c(gainer="ccode1", loser="ccode2", conflict="violtrans"))
Terrchanges2 <- rename(Terrchanges, c(ccode1="ccode2", ccode2="ccode1"))
Terrchanges <- rbind(Terrchanges, Terrchanges2)
rm(Terrchanges2)
Terrchanges$year5 <- Terrchanges$year + 4

Terrchanges <- adply(Terrchanges, 1, summarise, year = seq(year, year5))[c("ccode1", "ccode2", "year","violtrans")]
Terrchanges$year <- Terrchanges$year + 1

DYs <- join(DYs, Terrchanges, by=c("ccode1", "ccode2", "year"), type="left", match="first")
DYs$violtrans <- with(DYs, ifelse(is.na(violtrans), 0, violtrans))

rm(Terrchanges)

# Mountainous terrain

Mtns <- with(Mtn, data.frame(ccode, newlmtnest))
Mtns <- rename(Mtns, c(ccode="ccode1", newlmtnest="newlmtnest1"))

DYs <- join(DYs, Mtns, by=c("ccode1"), type="left", match="first")

Mtns <- rename(Mtns, c(ccode1="ccode2", newlmtnest1="newlmtnest2"))
DYs <- join(DYs, Mtns, by=c("ccode2"), type="left", match="first")

DYs$mtndiff <- with(DYs, newlmtnest1 - newlmtnest2)
DYs <- MoveFront(DYs, "ccode1")

# MIDs, finally.

GMLDDY$midonset <- with(GMLDDY, ifelse(is.na(midonset), 0, midonset))
GMLDDY$midongoing <- with(GMLDDY, ifelse(is.na(midongoing), 0, midongoing))

GMLDDY$isterr <- with(GMLDDY, ifelse(revtype11 == 1 | revtype12 == 1 | revtype21 == 1 | revtype22 == 1, 1, 0))
GMLDDY$isterr <- with(GMLDDY, ifelse(is.na(isterr), 0, isterr))
GMLDDY$terrmidon <- with(GMLDDY, ifelse(isterr  == 1 & midonset == 1, 1, 0))

# Moldova fixes.
GMLDDY$isterr[GMLDDY$dispnum3 == 3558] <- 1
GMLDDY$terrmidon[GMLDDY$dispnum3 == 3558] <- 1

# Abkhazia War fix (MID#3561), Georgia War
GMLDDY$isterr[GMLDDY$dispnum3 == 3561] <- 1
GMLDDY$terrmidon[GMLDDY$dispnum3 == 3561 & GMLDDY$midonset == 1] <- 1

GMLDDY$isterr[GMLDDY$dispnum3 == 4416] <- 1
GMLDDY$terrmidon[GMLDDY$dispnum3 == 4416 & GMLDDY$midonset == 1] <- 1

GMLDDY$isterr[GMLDDY$dispnum3 == 4422] <- 1
GMLDDY$terrmidon[GMLDDY$dispnum3 == 4422 & GMLDDY$midonset == 1] <- 1

GMLDDY$isterr[GMLDDY$dispnum3 == 4424] <- 1
GMLDDY$terrmidon[GMLDDY$dispnum3 == 4424 & GMLDDY$midonset == 1] <- 1

GMLDDY$isterr[GMLDDY$dispnum3 == 4436] <- 1
GMLDDY$terrmidon[GMLDDY$dispnum3 == 4436 & GMLDDY$midonset == 1] <- 1


GMLDDYs <- with(GMLDDY, data.frame(ccode1, ccode2, dyad, year, isterr, terrmidon, midongoing, midonset))
GMLDDYs$dyad <- with(GMLDDYs, as.numeric(paste0("1",sprintf("%03d", ccode1), sprintf("%03d", ccode2))))
GMLDDYs <- GMLDDYs[order(GMLDDYs$dyad, GMLDDYs$year),] 

DYs <- join(DYs, GMLDDYs, by=c("ccode1", "ccode2", "year"), type="left", match="first")

totals <- ddply(DYs, c("dyad"), summarize, total=sum(isterr))
DYs <- join(DYs, totals, by=c("dyad"), type="left", match="first")

hasterrmids <- subset(DYs, total > 0)
hasterrmids <- btscs(hasterrmids, "isterr", "year", "dyad")
hasterrmids$orig_order <- NULL

noterrmids <- subset(DYs, total == 0)
require(data.table)
noterrmids <- data.table(noterrmids)
noterrmids[, c("spell") := {
  rr <- rle(!isterr)$lengths
  list(sequence(rr) * !isterr)
}, by=dyad]
noterrmids <- as.data.frame(noterrmids)
noterrmids$spell <- with(noterrmids, spell - 1)

MID <- rbind(hasterrmids, noterrmids)
MID <- MID[order(MID$dyad, MID$year),] 

require(brglm)
TT1 <- brglm(terrmidon ~ mindist + majpow1 + militarization + terrclaim + spatrivalry + allied + violtrans + mtndiff + bs(spell, 4),
           data=MID, family=binomial(logit), na.action = na.exclude, method="brglm.fit", pl=TRUE)

write.table(MID,file="etst-mid.csv",sep=",",row.names=F,na="")

MID <- cbind(MID, ptart = predict(TT1,type='response'))
MID <- rename(MID, c(ccode1="ccode"))
MID$year <- MID$year + 1

MIDs <- aggregate(cbind(ptart) ~ ccode + year, MID, sum)


Macro <- join(Macro, MIDs, by=c("ccode", "year"), type="left", match="first")
Macro$ptart[Macro$ccode == 920] <- 0
Macro$logitptart <- car::logit(Macro$ptart)

# Communicable disease
# --------------------

Diseases <- subset(Disease, age_group_name == "All Ages" & unit == "percent") # "rate per 100,000"

Diseases$ccode <- countrycode(Diseases$location_code, "iso3c", "cown")
Diseases$ccode[Diseases$location_name == "Serbia"] <- 345
Diseases <- subset(Diseases, !is.na(ccode))

Diseases <- with(Diseases, data.frame(ccode, year, mean, sex_name))

Female <- WDI(country="all", indicator=c("SP.POP.TOTL.FE.ZS"), start=1991, end=2014)
#  Population, female (% of total) http://data.worldbank.org/indicator/SP.POP.TOTL.FE.ZS

Female <- rename(Female, c(SP.POP.TOTL.FE.ZS ="perw"))

Female$ccode <- countrycode(Female$iso2c, "iso2c", "cown")
Female$ccode[Female$iso2c == "RS"] <- 345

Female <- subset(Female, !is.na(ccode))
Female$country <- Female$iso2c <- NULL

Diseases <- join(Diseases, Female, by=c("ccode", "year"), type="left", match="first")

Diseases$dcmnnd <- NA
Diseases$dcmnnd <- with(Diseases, ifelse(sex_name == "Female", (perw/100)*mean, (1-(perw/100))*mean))

Diseases <- aggregate(cbind(dcmnnd) ~ ccode + year, Diseases, sum)

Macro <- join(Macro, Diseases, by=c("ccode", "year"), type="left", match="first")

# Missing: Taiwan (1994, 2006, 2012) and Andorra (2005)
# Andorra has a 1.07 male/female ratio in 2005 (est.)
Macro$dcmnnd <- with(Macro, ifelse(ccode == 232 & year == 2005, ((107/207)*43.1 + (100/207)*26.7), dcmnnd))

# Taiwan is apparently 1/1.
Macro$dcmnnd <- with(Macro, ifelse(ccode == 713 & year == 1994, ((1/2)*62 + (1/2)*42.5), dcmnnd))
Macro$dcmnnd <- with(Macro, ifelse(ccode == 713 & year == 2006, ((1/2)*70.7 + (1/2)*43), dcmnnd))
Macro$logdcmnnd <- log(Macro$dcmnnd)

# "Life histories"
# ---------------
# a) average life expectancy
# b) inverse fertility rate
# c) mean college education

Lifeexp <- WDI(country="all", indicator=c("SP.DYN.LE00.IN"), start=1991, end=2014)
# Life expectancy at birth, total (years)  http://data.worldbank.org/indicator/SP.DYN.LE00.IN

Lifeexp <- rename(Lifeexp, c(SP.DYN.LE00.IN ="lifeexp"))
Lifeexp$ccode <- countrycode(Lifeexp$iso2c, "iso2c", "cown")
Lifeexp$ccode[Lifeexp$iso2c == "RS"] <- 345
Lifeexp <- subset(Lifeexp, !is.na(ccode))
Lifeexp$country <- Lifeexp$iso2c <- NULL

Macro <- join(Macro, Lifeexp, by=c("ccode", "year"), type="left", match="first")
# Missing: Serbia (1996), Taiwan (1994, 2006, 2012), and Andorra (2005)
# Serbia was 71 in 1991 and 72 in 1997. We'll impute 71.5
Macro$lifeexp[Macro$ccode == 345 & Macro$year == 1996] <- 71.5

# I can't find Taiwan in 1994, but I found 1995. It's 74. https://www.ucar.edu/communications/gcip/m9popgrowth/m9pdfc7.pdf
# 2006 and 2012: http://www.indexmundi.com/g/g.aspx?c=tw&v=30
Macro$lifeexp[Macro$ccode == 713 & Macro$year == 1994] <- 74
Macro$lifeexp[Macro$ccode == 713 & Macro$year == 2006] <- 77.43
Macro$lifeexp[Macro$ccode == 713 & Macro$year == 2012] <- 78.48

# http://www.indexmundi.com/g/g.aspx?v=30&c=an&l=en
Macro$lifeexp[Macro$ccode == 232 & Macro$year == 2005] <- 83.51

Fert <- WDI(country="all", indicator=c("SP.DYN.TFRT.IN"), start=1991, end=2014)
# Fertility rate, total (births per woman) http://data.worldbank.org/indicator/SP.DYN.TFRT.IN

Fert <- rename(Fert, c(SP.DYN.TFRT.IN ="fert"))
Fert$ccode <- countrycode(Fert$iso2c, "iso2c", "cown")
Fert$ccode[Fert$iso2c == "RS"] <- 345
Fert <- subset(Fert, !is.na(ccode))
Fert$country <- Fert$iso2c <- NULL

Macro <- join(Macro, Fert, by=c("ccode", "year"), type="left", match="first")

# Serbia was 1.7 in 1995
Macro$fert[Macro$ccode == 345 & Macro$year == 1996] <- 1.7

# Same problem for Taiwan in 1994, but we have 1995 from the same source. It's 1.8.
Macro$fert[Macro$ccode == 713 & Macro$year == 1994] <- 1.8
# Same sources for the other years: http://www.indexmundi.com/g/g.aspx?c=tw&v=31
Macro$fert[Macro$ccode == 713 & Macro$year == 2006] <- 1.57
Macro$fert[Macro$ccode == 713 & Macro$year == 2012] <- 1.1
# http://www.indexmundi.com/g/g.aspx?c=an&v=31
Macro$fert[Macro$ccode == 232 & Macro$year == 2005] <- 1.29

Meancol <- aggregate(cbind(collegeed) ~ ccode + year, WVS, mean)
Meancol <- rename(Meancol, c(collegeed ="percol"))
Meancol$percol <- Meancol$percol*100
Macro <- join(Macro, Meancol, by=c("ccode", "year"), type="left", match="first")

Tert <- WDI(country="all", indicator=c("SE.TER.ENRR"), start=1991, end=2014)
# Gross enrolment ratio, tertiary, both sexes http://data.worldbank.org/indicator/SE.TER.ENRR

Tert <- rename(Tert, c(SE.TER.ENRR ="tert"))
Tert$ccode <- countrycode(Tert$iso2c, "iso2c", "cown")
Tert$ccode[Tert$iso2c == "RS"] <- 345
Tert <- subset(Tert, !is.na(ccode))
Tert$country <- Tert$iso2c <- NULL

Tert <-ddply(Tert ,c("ccode"), transform, tert = na.locf(tert,na.rm=FALSE,fromLast=TRUE))

Macro <- join(Macro, Tert, by=c("ccode", "year"), type="left", match="first")

Lifeh <- with(Macro, data.frame(ccode, year, lifeexp, fert, tert, percol))
Lifeh$fertmm <- with(Lifeh, (fert - min(fert, na.rm = TRUE))/(max(fert, na.rm = TRUE) - min(fert, na.rm = TRUE)))
Lifeh$ifertmm <- 1 - Lifeh$fertmm

Lifeh$mirtlife <- with(Lifeh, round(lifeexp))
Lifeh$mirttert <- with(Lifeh, round(tert))
Lifeh$mirtfert <- with(Lifeh, round(ifertmm*10))
Lifeh$mirtperc <- with(Lifeh, round(percol))

LifehM <- mirt(Lifeh[ ,  9:ncol(Lifeh)], model = 1,
              itemtype = "graded", SE = TRUE, verbose = FALSE)

lifehscores <- fscores(LifehM, full.scores = TRUE, full.scores.SE = TRUE)
Lifeh <- cbind(Lifeh, lifehscores)
Lifeh <- rename(Lifeh, c("F1" = "llifeh", "SE_F1" = "se_llifeh"))
Lifeh <- with(Lifeh, data.frame(ccode, year, llifeh))

Macro <- join(Macro, Lifeh, by=c("ccode", "year"), type="left", match="first")


# Merge, finally.
# ---------------

Data <- join(WVS, Macro, by=c("ccode", "year", "country", "wave"), type="left", match="first")
Data <- subset(Data, wave <= 5)

# Recode some DVs, and make an index.
Data$sldummy <- with(Data, recode(strongleader, "1:2=0; 3:4=1"))
Data$ardummy <- with(Data, recode(armyrule, "1:2=0; 3:4=1"))
Data$eddummy <- with(Data, recode(expertdecision, "1:2=0; 3:4=1"))
Data$hddummy <- with(Data, recode(havedem, "1:2=1; 3:4=0"))

# Make the index
Data$havedem <- with(Data, recode(havedem, "4=1; 3=2; 2=3; 1=4"))
Data$aindex <- with(Data, strongleader + armyrule + expertdecision + havedem)
IndexM <- mirt(Data[ ,  c("strongleader", "armyrule", "expertdecision", "havedem")], model = 1,
              itemtype = "graded", SE = TRUE, verbose = FALSE)

indexscores <- fscores(IndexM, full.scores = TRUE, full.scores.SE = TRUE)
Data <- cbind(Data, indexscores)
Data <- rename(Data, c("F1" = "index", "SE_F1" = "se_index"))

# Recode some IVs (micro)
Data <- ddply(Data, c("country", "wave"), transform, z_age = arm::rescale(age))
Data <- ddply(Data, c("country", "wave"), transform, z_ideo = arm::rescale(ideo))
Data <- ddply(Data, c("country", "wave"), transform, z_inc = arm::rescale(incscale))
Data <- ddply(Data, c("country", "wave"), transform, z_satisf = arm::rescale(satisfin))
Data <- ddply(Data, c("country", "wave"), transform, z_class = arm::rescale(socialclass))
Data <- ddply(Data, c("country", "wave"), transform, z_tradv = arm::rescale(ltradv))
Data <- ddply(Data, c("country", "wave"), transform, z_agency = arm::rescale(agency))
Data <- ddply(Data, c("country", "wave"), transform, z_emanc = arm::rescale(lemanc))
Data <- ddply(Data, c("country", "wave"), transform, z_intp = arm::rescale(interestp))
Data <- ddply(Data, c("country", "wave"), transform, z_postma = arm::rescale(postma))


# Recode some IVs (macro)
Data <- ddply(Data, c("wave"), transform, z_sti = arm::rescale(sti))
Data <- ddply(Data, c("wave"), transform, z_eti = arm::rescale(eti))
Data <- ddply(Data, c("wave"), transform, z_demest = arm::rescale(demest))
Data <- ddply(Data, c("wave"), transform, z_demstock = arm::rescale(demstock))
Data <- ddply(Data, c("wave"), transform, z_ptart = arm::rescale(logitptart))
Data <- ddply(Data, c("wave"), transform, z_dcmnnd = arm::rescale(logdcmnnd))
Data <- ddply(Data, c("wave"), transform, z_lifeh = arm::rescale(llifeh))






Data <- ddply(Data, c("wave"), transform, 
             demquantile = as.integer(cut(demstock, quantile(demstock, probs=0:10/10, na.rm = TRUE), 
                                       include.lowest=TRUE, right=FALSE)))


Data$wbregion <- countrycode(Data$ccode, "cown", "region")
Data$wbregion[Data$country == "Yugoslavia"   ] <- "Southern Europe"
Data$wbregion[Data$country == "Taiwan"   ] <- "Eastern Asia"


write.table(Data,file="etst-data.csv",sep=",",row.names=F,na="")


