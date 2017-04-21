library(foreign)

################################################################################ prepare data for UNPS2013/14  ######################################################################
##get fertilizer use - this is at plot level
agsec3A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC3A.dta")
agsec3B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC3B.dta")
sum(duplicated(agsec3A2013[c("HHID","plotID")]))
sum(duplicated(agsec3B2013[c("HHID","plotID")]))

##merge in production - this is at crop level
agsec5A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC5A.dta")
agsec5B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC5B.dta")
##get quantity at productionID level
agsec5A2013$prod <- agsec5A2013$a5aq6a*agsec5A2013$a5aq6d
agsec5B2013$prod <- agsec5B2013$a5bq6a*agsec5B2013$a5bq6d
##aggregate to product level

prodA2013 <- aggregate(agsec5A2013$prod, list(agsec5A2013$HHID, agsec5A2013$plotID, agsec5A2013$cropID), sum, na.rm=T)
names(prodA2013) <- c("HHID","plotID","cropID","prod")
prodB2013 <- aggregate(agsec5B2013$prod, list(agsec5B2013$HHID, agsec5B2013$plotID, agsec5B2013$cropID), sum, na.rm=T)
names(prodB2013) <- c("HHID","plotID","cropID","prod")
prodA2013$prod[prodA2013$prod > 200000] <- NA
prodB2013$prod[prodB2013$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC4A.dta")
agsec4B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC4B.dta")
agsec4A2013$a4aq9[agsec4A2013$a4aq8 == "Pure Stand"] <- 100
agsec4B2013$a4bq9[agsec4B2013$a4bq8 == "Pure Stand"] <- 100

##how to handle mixed cropping
areaA2013 <- aggregate(cbind(agsec4A2013$a4aq7,agsec4A2013$a4aq9), list(agsec4A2013$HHID, agsec4A2013$plotID, agsec4A2013$cropID), sum, na.rm=T)
names(areaA2013) <- c("HHID","plotID","cropID","area", "prop")
areaB2013 <- aggregate(cbind(agsec4B2013$a4bq7,agsec4B2013$a4bq9), list(agsec4B2013$HHID, agsec4B2013$plotID, agsec4B2013$cropID), sum, na.rm=T)
names(areaB2013) <- c("HHID","plotID","cropID","area", "prop")
areaA2013$prop[areaA2013$prop>100] <- NA
areaB2013$prop[areaB2013$prop>100] <- NA


yieldA2013 <- merge(areaA2013, prodA2013, by = c("HHID","plotID","cropID"))
yieldB2013 <- merge(areaB2013, prodB2013, by = c("HHID","plotID","cropID"))

allA2013 <- merge(yieldA2013, agsec3A2013[c(1,3,17:23)])
allB2013 <- merge(yieldB2013, agsec3B2013[c(1,2,17:23)])
names(allA2013) <- c("HHID", "plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2013) <- c("HHID", "plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2013$season <- 1
allB2013$season <- 2

all2013 <- rbind(allA2013, allB2013)

all2013$yield <- all2013$prod / (all2013$area * all2013$prop/100)
all2013$yield[all2013$yield > 15000] <- NA 

##merge in extension - this is at household level

agsec92013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC9.dta")
agsec92013$agext <- 0
agsec92013$agext[agsec92013$a9q5a=="Yes"] <- 1
ext2013 <- aggregate(cbind(agsec92013$agext, agsec92013$wgt_X), list(agsec92013$HHID),max, na.rm=T)
names(ext2013) <- c("HHID","ext","w")
### calculate percentage of households that reported to have received extension on ag production in the last 12 months
sum(ext2013$ext*ext2013$w)/sum(ext2013$w)

all2013 <- merge(all2013, ext2013, by="HHID")

################################################################################ prepare data for UNPS2011/12  ######################################################################
##get fertilizer use - this is at plot level
agsec3A2011 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC3A.dta")
agsec3B2011 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC3B.dta")
##merge in production - this is at crop level
### issues with labels here when converting from stata...
agsec5A2011 <- cbind(read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC5A.dta", convert.factors=F)[1:4], read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC5A.dta")[-(1:4)])
agsec5B2011 <- cbind(read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC5B.dta", convert.factors=F)[1:4], read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC5B.dta")[-(1:4)])
##get quantity at productionID level
agsec5A2011$prod <- agsec5A2011$a5aq6a*agsec5A2011$a5aq6d
agsec5B2011$prod <- agsec5B2011$a5bq6a*agsec5B2011$a5bq6d
##aggregate to product level
prodA2011 <- aggregate(agsec5A2011$prod, list(agsec5A2011$HHID, agsec5A2011$plotID, agsec5A2011$cropID), sum, na.rm=T)
names(prodA2011) <- c("HHID","plotID","cropID","prod")
prodB2011 <- aggregate(agsec5B2011$prod, list(agsec5B2011$HHID, agsec5B2011$plotID, agsec5B2011$cropID), sum, na.rm=T)
names(prodB2011) <- c("HHID","plotID","cropID","prod")
prodA2011$prod[prodA2011$prod > 200000] <- NA
prodB2011$prod[prodB2011$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2011 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC4A.dta")
agsec4B2011 <- cbind(read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC4B.dta", convert.factors=F)[1:4], read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC4B.dta")[-(1:4)])
agsec4A2011$a4aq9[agsec4A2011$a4aq8 == "Pure Stand"] <- 100
agsec4B2011$a4bq9[agsec4B2011$a4bq8 == "Pure Stand"] <- 100

areaA2011 <- aggregate(cbind(agsec4A2011$a4aq7,agsec4A2011$a4aq9), list(agsec4A2011$HHID, agsec4A2011$plotID, agsec4A2011$cropID), sum, na.rm=T)
names(areaA2011) <- c("HHID","plotID","cropID","area", "prop")
areaB2011 <- aggregate(cbind(agsec4B2011$a4bq7,agsec4B2011$a4bq9), list(agsec4B2011$HHID, agsec4B2011$plotID, agsec4B2011$cropID), sum, na.rm=T)
names(areaB2011) <- c("HHID","plotID","cropID","area", "prop")
areaA2011$prop[areaA2011$prop>100] <- NA
areaB2011$prop[areaB2011$prop>100] <- NA

yieldA2011 <- merge(areaA2011, prodA2011, by = c("HHID","plotID","cropID"))
yieldB2011 <- merge(areaB2011, prodB2011, by = c("HHID","plotID","cropID"))

allA2011 <- merge(yieldA2011, agsec3A2011[c(1,3,17:23)])
allB2011 <- merge(yieldB2011, agsec3B2011[c(1,3,17:23)])
names(allA2011) <- c("HHID", "plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2011) <- c("HHID", "plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2011$season <- 1
allB2011$season <- 2

all2011 <- rbind(allA2011, allB2011)

all2011$yield <- all2011$prod / (all2011$area * all2011$prop/100)
all2011$yield[all2011$yield > 15000] <- NA 

##merge in extension - this is at household level

agsec92011 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC9.dta")
agsec92011$agext <- 0
agsec92011$agext[agsec92011$a9q5a=="Yes"] <- 1
ext2011 <- aggregate(agsec92011$agext, list(agsec92011$HHID),max, na.rm=T)
names(ext2011) <- c("HHID","ext")
### calculate percentage of households that reported to have received extension on ag production in the last 12 months
sum(ext2011$ext)/length(ext2011$ext)

all2011 <- merge(all2011, ext2011, by="HHID")
all2011$w <- NA


################################################################################ prepare data for UNPS2010/11  ######################################################################
##get fertilizer use - this is at plot level
agsec3A2010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC3A.dta")
agsec3B2010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC3B.dta")
names(agsec3A2010)[1:3] <- c("HHID","parcelID","plotID")
names(agsec3B2010)[1:3] <- c("HHID","parcelID","plotID")
## from here, also parcelID is needed to uniquely identify plots
sum(duplicated(agsec3A2010[c("HHID","parcelID","plotID")]))
sum(duplicated(agsec3B2010[c("HHID","parcelID","plotID")]))

##merge in production - this is at crop level
### issues with labels here when converting from stata...
agsec5A2010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC5A.dta")
agsec5B2010 <-  cbind(read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC5B.dta", convert.factors=F)[1:4], read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC5B.dta")[-(1:4)])
names(agsec5A2010)[1:3] <- c("HHID","parcelID","plotID")
names(agsec5B2010)[1:3] <- c("HHID","parcelID","plotID")

##get quantity at productionID level
agsec5A2010$prod <- agsec5A2010$a5aq6a*agsec5A2010$a5aq6d
agsec5B2010$prod <- agsec5B2010$a5bq6a*agsec5B2010$a5bq6d
##aggregate to product level
prodA2010 <- aggregate(agsec5A2010$prod, list(agsec5A2010$HHID, agsec5A2010$parcelID,agsec5A2010$plotID, agsec5A2010$cropID), sum, na.rm=T)
names(prodA2010) <- c("HHID","parcelID","plotID","cropID","prod")
prodB2010 <- aggregate(agsec5B2010$prod, list(agsec5B2010$HHID, agsec5B2010$parcelID, agsec5B2010$plotID, agsec5B2010$cropID), sum, na.rm=T)
names(prodB2010) <- c("HHID","parcelID","plotID","cropID","prod")
prodA2010$prod[prodA2010$prod > 200000] <- NA
prodB2010$prod[prodB2010$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC4A.dta")
agsec4B2010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC4B.dta")
names(agsec4A2010)[1:3] <- c("HHID","parcelID","plotID")
names(agsec4B2010)[1:3] <- c("HHID","parcelID","plotID")
agsec4A2010$a4aq9[agsec4A2010$a4aq7 == "Pure Stand"] <- 100
agsec4B2010$a4bq9[agsec4B2010$a4bq7 == "Pure stand"] <- 100

areaA2010 <- aggregate(cbind(agsec4A2010$a4aq8,agsec4A2010$a4aq9), list(agsec4A2010$HHID, agsec4A2010$parcelID, agsec4A2010$plotID, agsec4A2010$cropID), sum, na.rm=T)
names(areaA2010) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaB2010 <- aggregate(cbind(agsec4B2010$a4bq8,agsec4B2010$a4bq9), list(agsec4B2010$HHID, agsec4B2010$parcelID, agsec4B2010$plotID, agsec4B2010$cropID), sum, na.rm=T)
names(areaB2010) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaA2010$prop[areaA2010$prop>100] <- NA
areaB2010$prop[areaB2010$prop>100] <- NA

yieldA2010 <- merge(areaA2010, prodA2010, by = c("HHID","parcelID","plotID","cropID"))
yieldB2010 <- merge(areaB2010, prodB2010, by = c("HHID","parcelID","plotID","cropID"))

allA2010 <- merge(yieldA2010, agsec3A2010[c(1:3,16:21,23)])
allB2010 <- merge(yieldB2010, agsec3B2010[c(1:3,16:21,23)])
names(allA2010) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2010) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2010$season <- 1
allB2010$season <- 2

all2010 <- rbind(allA2010, allB2010)

all2010$yield <- all2010$prod / (all2010$area * all2010$prop/100)
all2010$yield[all2010$yield > 15000] <- NA 

##merge in extension - this is at household level
## this time it is only those that received extension that have been recorded
agsec92010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC9.dta")
agsec92010$agext <- 0
agsec92010$agext[agsec92010$a9q5a=="Yes"] <- 1
ext2010 <- aggregate(agsec92010$agext, list(agsec92010$HHID),max, na.rm=T)
names(ext2010) <- c("HHID","ext")
### calculate percentage of households that reported to have received extension on ag production in the last 12 months
sum(ext2010$ext)/length(ext2010$ext)

all2010 <- merge(all2010, ext2010, by="HHID", all.x=T)
all2010$w <- NA
all2010$ext[is.na(all2010$ext)] <- 0

sum(aggregate(all2010$ext,list(all2010$HHID), max)[2])/2145
#20 percent of households got extension 9n 2010


## remove parcelIDs to merge with more recent waves, but in fact I should add parcelIDs to the recent waves...
all2010$parcelID <- NULL

################################################################################ prepare data for UNPS2009/10  ######################################################################
##get fertilizer use - this is at plot level
agsec3A2009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC3A.dta")
agsec3B2009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC3B.dta")
names(agsec3A2009)[1:3] <- c("HHID","parcelID","plotID")
names(agsec3B2009)[1:3] <- c("HHID","parcelID","plotID")
## from here, also parcelID is needed to uniquely identify plots
sum(duplicated(agsec3A2009[c("HHID","parcelID","plotID")]))
sum(duplicated(agsec3B2009[c("HHID","parcelID","plotID")]))

##merge in production - this is at crop level
### issues with labels here when converting from stata...
agsec5A2009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC5A.dta")
agsec5A2009$a5aq4 <- NULL
agsec5B2009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC5B.dta")
agsec5B2009$a5bq4 <- NULL

names(agsec5A2009)[1:4] <- c("HHID","parcelID","plotID","cropID")
names(agsec5B2009)[1:4] <- c("HHID","parcelID","plotID","cropID")

##get quantity at productionID level
agsec5A2009$prod <- agsec5A2009$a5aq6a*agsec5A2009$a5aq6d
agsec5B2009$prod <- agsec5B2009$a5bq6a*agsec5B2009$a5bq6d
##aggregate to product level
prodA2009 <- aggregate(agsec5A2009$prod, list(agsec5A2009$HHID, agsec5A2009$parcelID,agsec5A2009$plotID, agsec5A2009$cropID), sum, na.rm=T)
names(prodA2009) <- c("HHID","parcelID","plotID","cropID","prod")
prodB2009 <- aggregate(agsec5B2009$prod, list(agsec5B2009$HHID, agsec5B2009$parcelID, agsec5B2009$plotID, agsec5B2009$cropID), sum, na.rm=T)
names(prodB2009) <- c("HHID","parcelID","plotID","cropID","prod")
prodA2009$prod[prodA2009$prod > 200000] <- NA
prodB2009$prod[prodB2009$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC4A.dta")
agsec4A2009$a4aq1 <- NULL
agsec4A2009$a4aq5 <- NULL
agsec4B2009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC4B.dta")
agsec4B2009$a4bq1 <- NULL
agsec4B2009$a4bq5 <- NULL
names(agsec4A2009)[1:4] <- c("HHID","parcelID","plotID","cropID")
names(agsec4B2009)[1:4] <- c("HHID","parcelID","plotID","cropID")
agsec4A2009$a4aq9[agsec4A2009$a4aq7 == "Pure Stand"] <- 100
agsec4B2009$a4bq9[agsec4B2009$a4bq7 == "Pure stand"] <- 100

areaA2009 <- aggregate(cbind(agsec4A2009$a4aq8,agsec4A2009$a4aq9), list(agsec4A2009$HHID, agsec4A2009$parcelID, agsec4A2009$plotID, agsec4A2009$cropID), sum, na.rm=T)
names(areaA2009) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaB2009 <- aggregate(cbind(agsec4B2009$a4bq8,agsec4B2009$a4bq9), list(agsec4B2009$HHID, agsec4B2009$parcelID, agsec4B2009$plotID, agsec4B2009$cropID), sum, na.rm=T)
names(areaB2009) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaA2009$prop[areaA2009$prop>100] <- NA
areaB2009$prop[areaB2009$prop>100] <- NA

yieldA2009 <- merge(areaA2009, prodA2009, by = c("HHID","parcelID","plotID","cropID"))
yieldB2009 <- merge(areaB2009, prodB2009, by = c("HHID","parcelID","plotID","cropID"))

allA2009 <- merge(yieldA2009, agsec3A2009[c(1:3,14:19,21)])
allB2009 <- merge(yieldB2009, agsec3B2009[c(1:3,14:19,21)])
names(allA2009) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2009) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2009$season <- 1
allB2009$season <- 2

all2009 <- rbind(allA2009, allB2009)

all2009$yield <- all2009$prod / (all2009$area * all2009$prop/100)
all2009$yield[all2009$yield > 15000] <- NA 

##merge in extension - this is at household level
## this time it is only those that received extension that have been recorded
agsec92009 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC10.dta")
agsec92009$agext <- 0
agsec92009$agext[agsec92009$a10q5a=="Yes"] <- 1
ext2009 <- aggregate(agsec92009$agext, list(agsec92009$HHID),max, na.rm=T)
names(ext2009) <- c("HHID","ext")
### calculate percentage of households that reported to have received extension on ag production in the last 12 months
sum(ext2009$ext)/length(ext2009$ext)

all2009 <- merge(all2009, ext2009, by="HHID", all.x=T)
#merge in weights
all2009 <- merge(all2009, read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/GSEC1.dta")[c("HHID","wgt09")])
names(all2009)[names(all2009) == "wgt09"] <- "w"
all2009$ext[is.na(all2009$ext)] <- 0

all2009$fert_use <- as.numeric(all2009$fert_use == "Yes")
###

inter <- aggregate(all2009[c("fert_use","w")],list(all2009$HHID, all2009$parcelID, all2009$plotID, all2009$season),max, na.rm=T)
names(inter)[1:4] <- c("HHID", "parcelID","plotID", "season")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)
sum(inter$fert_use[inter$season == 1]*inter$w[inter$season == 1], na.rm=T)/sum(inter$w[inter$season == 1], na.rm=T)
sum(inter$fert_use[inter$season == 2]*inter$w[inter$season == 2], na.rm=T)/sum(inter$w[inter$season == 2], na.rm=T)

inter <- aggregate(all2009[c("fert_use","w")],list(all2009$HHID),max, na.rm=T)
names(inter)[1] <- c("HHID")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)



## remove parcelIDs to merge with more recent waves, but in fact I should add parcelIDs to the recent waves...
all2009$parcelID <- NULL


all2009$year <- 2009
all2010$year <- 2010
all2011$year <- 2011
all2013$year <- 2013

all <- rbind(all2009,all2010,all2011, all2013)





#################

library(locfit)

#pdf("~/data/projects/MAFAP/analysis/yields.pdf",sep=""))


fit <- locfit((fert_use == "Yes" & ext== 0 )~log(yield),data=all[  all$yield>0 & !is.na(all$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
#df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

plot(df[,1],df$fit, type="l",  xlab="log(yield)", ylab="% change in proportion",ylim = c(0,.0125), xlim=c(5.1,8),lwd=3)
#lines(df[,1],df$upper, lty=4)
#lines(df[,1],df$lower, lty=4)

fit <- locfit((ext == 1 & fert_use== "No")~log(yield),data=all[  all$yield>0 & !is.na(all$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
#df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.1,9))
lines(df[,1],df$fit, lwd=3, col="red")
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

fit <- locfit((fert_use == "Yes" & ext == 1 )~log(yield),data=all[  all$yield>0 & !is.na(all$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
#df$fit <- (df$fit - mean(df$fit))/mean(df$fit)
#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.5,7.5))
lines(df[,1],df$fit, lwd=3, col="green")
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

rug(log(all$yield[  all$yield>0 & !is.na(all$yield)]))

quantile(log(all$yield[  all$yield>0 & !is.na(all$yield)]),c(.2,.9))








