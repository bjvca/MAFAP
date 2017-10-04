library(foreign)
rm(list=ls())
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

prodA2013 <- aggregate(agsec5A2013$prod, list(agsec5A2013$HHID, agsec5A2013$parcelID, agsec5A2013$plotID, agsec5A2013$cropID), sum, na.rm=T)
names(prodA2013) <- c("HHID","parcelID","plotID","cropID","prod")
prodB2013 <- aggregate(agsec5B2013$prod, list(agsec5B2013$HHID, agsec5B2013$parcelID, agsec5B2013$plotID, agsec5B2013$cropID), sum, na.rm=T)
names(prodB2013) <- c("HHID","parcelID","plotID","cropID","prod")
prodA2013$prod[prodA2013$prod > 200000] <- NA
prodB2013$prod[prodB2013$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC4A.dta")
agsec4B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC4B.dta")
agsec4A2013$a4aq9[agsec4A2013$a4aq8 == "Pure Stand"] <- 100
agsec4B2013$a4bq9[agsec4B2013$a4bq8 == "Pure Stand"] <- 100

##how to handle mixed cropping
areaA2013 <- aggregate(cbind(agsec4A2013$a4aq7,agsec4A2013$a4aq9), list(agsec4A2013$HHID, agsec4A2013$parcelID, agsec4A2013$plotID, agsec4A2013$cropID), sum, na.rm=T)
names(areaA2013) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaB2013 <- aggregate(cbind(agsec4B2013$a4bq7,agsec4B2013$a4bq9), list(agsec4B2013$HHID, agsec4B2013$parcelID, agsec4B2013$plotID, agsec4B2013$cropID), sum, na.rm=T)
names(areaB2013) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaA2013$prop[areaA2013$prop>100] <- NA
areaB2013$prop[areaB2013$prop>100] <- NA



yieldA2013 <- merge(areaA2013, prodA2013, by = c("HHID","parcelID","plotID","cropID"))
yieldB2013 <- merge(areaB2013, prodB2013, by = c("HHID","parcelID","plotID","cropID"))

allA2013 <- merge(yieldA2013, agsec3A2013[c(1,2,3,17:23)])
allB2013 <- merge(yieldB2013, agsec3B2013[c(1,3,2,17:23)])



##

names(allA2013) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2013) <- c("HHID","parcelID", "plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2013$season <- 1
allB2013$season <- 2

all2013 <- rbind(allA2013, allB2013)

all2013$yield <- all2013$prod / (all2013$area * all2013$prop/100)
all2013$yield[all2013$yield > 15000] <- NA 

### merge in instruments, this is at parcel level
agsec2a <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC2A.dta")[c( "HHID", "parcelID", "a2aq1owned" , "a2aq6", "a2aq19" )]
names(agsec2a) <- c("HHID","parcelID","own","dist","topo")
agsec2b <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC2B.dta")[c( "HHID", "parcelID", "a2bq1owned" , "a2bq6", "a2bq17" )]
names(agsec2b) <- c("HHID","parcelID","own","dist","topo")
agsec2 <- rbind(agsec2a,agsec2b)

all2013 <- merge(all2013,agsec2, by=c("HHID","parcelID"))

##merge in extension - this is at household level

agsec92013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC9.dta")
agsec92013$agext <- 0
agsec92013$agext[agsec92013$a9q5a=="Yes"] <- 1
ext2013 <- aggregate(cbind(agsec92013$agext, agsec92013$wgt_X), list(agsec92013$HHID),max, na.rm=T)
names(ext2013) <- c("HHID","ext","w")
### calculate percentage of households that reported to have received extension on ag production in the last 12 months
sum(ext2013$ext*ext2013$w)/sum(ext2013$w)

all2013 <- merge(all2013, ext2013, by="HHID")


inter <- aggregate(cbind(all2013$fert_use == "Yes",all2013$w), list(all2013$HHID, all2013$plotID, all2013$season),max, na.rm=T)

names(inter) <- c("HHID","plotID", "season", "fert_use", "w")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)
sum(inter$fert_use[inter$season == 1]*inter$w[inter$season == 1], na.rm=T)/sum(inter$w[inter$season == 1], na.rm=T)
sum(inter$fert_use[inter$season == 2]*inter$w[inter$season == 2], na.rm=T)/sum(inter$w[inter$season == 2], na.rm=T)
inter <- aggregate(cbind(all2013$fert_use == "Yes",all2013$w), list(all2013$HHID),max, na.rm=T)

names(inter) <- c("HHID", "fert_use", "w")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)

## merge in HHsize, this is at HHlevel
gsec2 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC2.dta")[c("HHID","h2q7")]
gsec2$ones <- 1
hhsize <- aggregate(gsec2$ones, list(gsec2$HHID), sum)

names(hhsize) <- c("HHID","hhsize")
hhsize$HHID <- gsub("-","",substring(hhsize$HHID,2))
hhsize$HHID <-  as.numeric(as.character(hhsize$HHID))

all2013 <- merge(all2013, hhsize, by="HHID")

### merge in femhead, eduhead
gsec2 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC2.dta")[c("HHID","h2q3","h2q4","h2q8")]
heads <- subset(gsec2,h2q4 == "Head")
heads$h2q4 <- NULL
names(heads) <- c("HHID","femhead","agehead")
heads$femhead <- heads$femhead == "Female"
heads$HHID <- gsub("-","",substring(heads$HHID,2))
heads$HHID <-  as.numeric(as.character(heads$HHID))

all2013 <- merge(all2013, heads, by="HHID")

### merge in headedu
## identify heads
gsec2 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC2.dta")[c("HHID","PID","h2q4")]
heads <- subset(gsec2,h2q4 == "Head")

gsec4 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC4.dta")

edu <- merge(heads, gsec4, by = c("HHID","PID"))[c("HHID","h4q4")]
names(edu) <- c("HHID","literate")
edu$literate <-  edu$literate == "Able to read and write"
edu$HHID <- gsub("-","",substring(edu$HHID,2))
edu$HHID <-  as.numeric(as.character(edu$HHID))

all2013 <- merge(all2013, edu, by="HHID")

### merge in potential instrument at housheold level
sec16 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC16.dta")
inputcost <- subset(sec16, h16q00 == "Unusually High Costs of Agricultural Inputs")[c("HHID","h16q01")]
inputcost$h16q01 <- inputcost$h16q01 == "Yes"
names(inputcost) <- c("HHID","inputshock")
inputcost$HHID <- gsub("-","",substring(inputcost$HHID,2))
inputcost$HHID <-  as.numeric(as.character(inputcost$HHID))
all2013 <- merge(all2013, inputcost, by="HHID")

## merge in assets at household level
sec14 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC14A.dta")
sec14$HHID <- gsub("-","",substring(sec14$HHID,2))
sec14$HHID <-  as.numeric(as.character(sec14$HHID))
bike <- subset(sec14, h14q2 == "Bicycle")[c( "HHID",  "h14q3")]
names(bike) <- c("HHID", "bike")
bike$bike <-  bike$bike == "Yes"
mobile <- subset(sec14, h14q2 == "Mobile phone")[c( "HHID",  "h14q3")]
names(mobile) <- c("HHID", "mobile")
mobile$mobile <-  mobile$mobile == "Yes"
all2013 <- merge(all2013, bike, by="HHID")
all2013 <- merge(all2013, mobile, by="HHID")



summary(lm(log(yield)~(fert_use=="Yes")*ext + hhsize + femhead + agehead + literate + dist + inputshock + topo + own + bike + mobile ,data=all2013[all2013$yield>0 & all2013$cropID == 210,]))


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
prodA2011 <- aggregate(agsec5A2011$prod, list(agsec5A2011$HHID, agsec5A2011$parcelID, agsec5A2011$plotID, agsec5A2011$cropID), sum, na.rm=T)
names(prodA2011) <- c("HHID","parcelID","plotID","cropID","prod")
prodB2011 <- aggregate(agsec5B2011$prod, list(agsec5B2011$HHID, agsec5B2011$parcelID, agsec5B2011$plotID, agsec5B2011$cropID), sum, na.rm=T)
names(prodB2011) <- c("HHID","parcelID","plotID","cropID","prod")
prodA2011$prod[prodA2011$prod > 200000] <- NA
prodB2011$prod[prodB2011$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2011 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC4A.dta")
agsec4B2011 <- cbind(read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC4B.dta", convert.factors=F)[1:4], read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC4B.dta")[-(1:4)])
agsec4A2011$a4aq9[agsec4A2011$a4aq8 == "Pure Stand"] <- 100
agsec4B2011$a4bq9[agsec4B2011$a4bq8 == "Pure Stand"] <- 100

areaA2011 <- aggregate(cbind(agsec4A2011$a4aq7,agsec4A2011$a4aq9), list(agsec4A2011$HHID,agsec4A2011$parcelID, agsec4A2011$plotID, agsec4A2011$cropID), sum, na.rm=T)
names(areaA2011) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaB2011 <- aggregate(cbind(agsec4B2011$a4bq7,agsec4B2011$a4bq9), list(agsec4B2011$HHID, agsec4B2011$parcelID, agsec4B2011$plotID, agsec4B2011$cropID), sum, na.rm=T)
names(areaB2011) <- c("HHID","parcelID","plotID","cropID","area", "prop")
areaA2011$prop[areaA2011$prop>100] <- NA
areaB2011$prop[areaB2011$prop>100] <- NA

yieldA2011 <- merge(areaA2011, prodA2011, by = c("HHID","parcelID","plotID","cropID"))
yieldB2011 <- merge(areaB2011, prodB2011, by = c("HHID","parcelID","plotID","cropID"))

allA2011 <- merge(yieldA2011, agsec3A2011[c(1,3,17:23)])
allB2011 <- merge(yieldB2011, agsec3B2011[c(1,3,17:23)])
names(allA2011) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2011) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2011$season <- 1
allB2011$season <- 2

all2011 <- rbind(allA2011, allB2011)

all2011$yield <- all2011$prod / (all2011$area * all2011$prop/100)
all2011$yield[all2011$yield > 15000] <- NA 

### merge in instruments, this is at parcel level
agsec2a <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC2A.dta")[c( "HHID", "parcelID", "a2aq6", "a2aq19" )]
names(agsec2a) <- c("HHID","parcelID","dist","topo")
agsec2a$own <- "Owned"
agsec2a <- agsec2a[c("HHID","parcelID","own","dist","topo")]
agsec2b <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC2B.dta")[c( "HHID", "parcelID", "a2bq6", "a2bq17" )]
names(agsec2b) <- c("HHID","parcelID","dist","topo")
agsec2b$own <- "Access / Use Rights"
agsec2b <- agsec2b[c("HHID","parcelID","own","dist","topo")]
agsec2 <- rbind(agsec2a,agsec2b)

all2011 <- merge(all2011,agsec2, by=c("HHID","parcelID"), all.x=T)

##merge in extension - this is at household level

agsec92011 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/AGSEC9.dta")
agsec92011$agext <- 0
agsec92011$agext[agsec92011$a9q5a=="Yes"] <- 1
ext2011 <- aggregate(agsec92011$agext, list(agsec92011$HHID),max, na.rm=T)
names(ext2011) <- c("HHID","ext")


all2011 <- merge(all2011, ext2011, by="HHID")
all2011 <- merge(all2011, read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/GSEC1.dta")[c("HHID","mult")])
names(all2011)[names(all2011) == "mult"] <- "w"

## merge in assets at household level
sec14 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2011/GSEC14.dta")
bike <- subset(sec14, h14q2 == "Bicycle")[c( "HHID",  "h14q3")]
names(bike) <- c("HHID", "bike")
bike$bike <-  bike$bike == "Yes"
mobile <- subset(sec14, h14q2 == "Mobile phone")[c( "HHID",  "h14q3")]
names(mobile) <- c("HHID", "mobile")
mobile$mobile <-  mobile$mobile == "Yes"
all2011 <- merge(all2011, bike, by="HHID")
all2011 <- merge(all2011, mobile, by="HHID")


inter <- aggregate(cbind(all2011$ext,all2011$w), list(all2011$HHID),max)
names(inter) <- c("HHID","ext","w")
sum(inter$ext*inter$w, na.rm=T)/sum(inter$w, na.rm=T)

sum((all2011$fert_use == "Yes") * all2011$w, na.rm=T)/sum(all2011$w, na.rm=T)


inter <- aggregate(cbind(all2011$fert_use == "Yes",all2011$w), list(all2011$HHID, all2011$plotID, all2011$season),max, na.rm=T)

names(inter) <- c("HHID","plotID", "season", "fert_use", "w")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)
sum(inter$fert_use[inter$season == 1]*inter$w[inter$season == 1], na.rm=T)/sum(inter$w[inter$season == 1], na.rm=T)
sum(inter$fert_use[inter$season == 2]*inter$w[inter$season == 2], na.rm=T)/sum(inter$w[inter$season == 2], na.rm=T)

inter <- aggregate(cbind(all2011$fert_use == "Yes",all2011$w), list(all2011$HHID),max, na.rm=T)
names(inter) <- c("HHID","fert_use","w")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)



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
allB2010$a3bq14[allB2010$a3bq14 == 1] <- "Yes"
allB2010$a3bq14[allB2010$a3bq14 != "Yes"] <- "No"

names(allA2010) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
names(allB2010) <- c("HHID", "parcelID","plotID", "cropID", "area", "prop", "prod", "fert_use", "fert_typ", "fert_qty", "fert_bgt", "fert_qty_bgt", "fert_paid", "fert_where_bgt")
allA2010$season <- 1
allB2010$season <- 2

all2010 <- rbind(allA2010, allB2010)
#merge in weights here
all2010 <- merge(all2010, read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/GSEC1.dta")[c("HHID","wgt10")])
names(all2010)[names(all2010) == "wgt10"] <- "w"

all2010$yield <- all2010$prod / (all2010$area * all2010$prop/100)
all2010$yield[all2010$yield > 15000] <- NA 

### merge in instruments, this is at parcel level
agsec2a <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC2A.dta")[c( "HHID", "prcid", "a2aq6", "a2aq21" )]
names(agsec2a) <- c("HHID","parcelID","dist","topo")
agsec2a$own <- "Owned"
agsec2a <- agsec2a[c("HHID","parcelID","own","dist","topo")]
agsec2a$parcelID <- as.numeric(as.character(agsec2a$parcelID))
agsec2b <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC2B.dta")[c( "HHID", "prcid", "a2bq6", "a2bq20" )]
names(agsec2b) <- c("HHID","parcelID","dist","topo")
agsec2b$own <- "Access / Use Rights"
agsec2b <- agsec2b[c("HHID","parcelID","own","dist","topo")]
agsec2b$parcelID <- as.numeric(as.character(agsec2b$parcelID))
agsec2 <- rbind(agsec2a,agsec2b)

all2010 <- merge(all2010,agsec2, by=c("HHID","parcelID"))

##merge in extension - this is at household level
## this time it is only those that received extension that have been recorded
agsec92010 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/AGSEC9.dta")
agsec92010$agext <- 0
agsec92010$agext[agsec92010$a9q5a=="Yes"] <- 1
ext2010 <- aggregate(agsec92010$agext, list(agsec92010$HHID),max, na.rm=T)
names(ext2010) <- c("HHID","ext")
### calculate percentage of households that reported to have received extension on ag production in the last 12 months


all2010 <- merge(all2010, ext2010, by="HHID", all.x=T)
all2010$ext[is.na(all2010$ext)] <- 0

## merge in assets at household level
sec14 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2010/GSEC14.dta")
bike <- subset(sec14, h14q2 == "Bicycle")[c( "HHID",  "h14q3")]
names(bike) <- c("HHID", "bike")
bike$bike <- bike$bike == 1
mobile <- subset(sec14, h14q2 == "Mobile phone")[c( "HHID",  "h14q3")]
names(mobile) <- c("HHID", "mobile")
mobile$mobile <- mobile$mobile == 1
all2010 <- merge(all2010, bike, by="HHID")
all2010 <- merge(all2010, mobile, by="HHID")

###

inter <-  aggregate(all2010[c("ext","w")], list(all2010$HHID),max )
all2010$fert_use[is.na(all2010$fert_use)] <- "No"
all2010$fert_use <- (all2010$fert_use == "Yes")
inter <- aggregate(all2010[c("fert_use","w")],list(all2010$HHID, all2010$parcelID, all2010$plotID, all2010$season),max, na.rm=T)
names(inter)[1:4] <- c("HHID", "parcelID","plotID", "season")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)
sum(inter$fert_use[inter$season == 1]*inter$w[inter$season == 1], na.rm=T)/sum(inter$w[inter$season == 1], na.rm=T)
sum(inter$fert_use[inter$season == 2]*inter$w[inter$season == 2], na.rm=T)/sum(inter$w[inter$season == 2], na.rm=T)

inter <- aggregate(all2010[c("fert_use","w")],list(all2010$HHID),max, na.rm=T)
names(inter)[1] <- c("HHID")
inter$w[is.infinite(inter$w)] <- NA
inter$fert_use[is.infinite(inter$fert_use)] <- NA
sum(inter$fert_use*inter$w, na.rm=T)/sum(inter$w, na.rm=T)

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

### merge in instruments, this is at parcel level
agsec2a <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC2A.dta")[c( "HHID", "a2aq2", "a2aq6", "a2aq21" )]
names(agsec2a) <- c("HHID","parcelID","dist","topo")
agsec2a$own <- "Owned"
agsec2a <- agsec2a[c("HHID","parcelID","own","dist","topo")]
agsec2a$parcelID <- as.numeric(as.character(agsec2a$parcelID))
agsec2b <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/AGSEC2B.dta")[c( "HHID", "a2bq2", "a2bq6", "a2bq20" )]
names(agsec2b) <- c("HHID","parcelID","dist","topo")
agsec2b$own <- "Access / Use Rights"
agsec2b <- agsec2b[c("HHID","parcelID","own","dist","topo")]
agsec2b$parcelID <- as.numeric(as.character(agsec2b$parcelID))
agsec2 <- rbind(agsec2a,agsec2b)

all2009 <- merge(all2009,agsec2, by=c("HHID","parcelID"))


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

## merge in assets at household level
sec14 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2009/GSEC14.dta")
bike <- subset(sec14, h14q2 == "Bicycle")[c( "HHID",  "h14q3")]
names(bike) <- c("HHID", "bike")
bike$bike <-  bike$bike == "Yes"
mobile <- subset(sec14, h14q2 == "Mobile phone")[c( "HHID",  "h14q3")]
names(mobile) <- c("HHID", "mobile")
mobile$mobile <-  mobile$mobile == "Yes"
all2009 <- merge(all2009, bike, by="HHID")
all2009 <- merge(all2009, mobile, by="HHID")


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

all2009$fert_use <-  all2009$fert_use == 1
all2011$fert_use <-  all2011$fert_use == "Yes"
all2013$fert_use <-  all2013$fert_use == "Yes"

all <- rbind(all2009,all2010,all2011, all2013)
write.dta(all,file="/home/bjvca/data/projects/MAFAP/data/allfert.dta")

## what are most fertilizered crops
tapply(all$fert_use, all$cropID, mean, na.rm=T)
table(all$cropID)

## only for maize:
maize <- subset(all, cropID == 130  )
write.dta(maize,file="/home/bjvca/data/projects/MAFAP/data/maizefert.dta")
## only for beans:
beans <- subset(all, cropID == 210  )
write.dta(beans,file="/home/bjvca/data/projects/MAFAP/data/beansfert.dta")
## only for vegetables:
veg <- subset(all, cropID %in% c(  410,   420,   430,   440,   450,   460,   470 )  )
write.dta(veg,file="/home/bjvca/data/projects/MAFAP/data/vegfert.dta")

####
table(all$fert_use == 0 & all$ext== 0)/sum(table(all$fert_use == 0 & all$ext== 0))
table(all$fert_use == 1 & all$ext== 0)/sum(table(all$fert_use == 1 & all$ext== 0))
table(all$fert_use == 0 & all$ext== 1)/sum(table(all$fert_use == 0 & all$ext== 1))
table(all$fert_use == 1 & all$ext== 1)/sum(table(all$fert_use == 1 & all$ext== 1))



#### only for maize 

####
table(maize$fert_use == 0 & maize$ext== 0)/sum(table(maize$fert_use == 0 & maize$ext== 0))
table(maize$fert_use == 1 & maize$ext== 0)/sum(table(maize$fert_use == 1 & maize$ext== 0))
table(maize$fert_use == 0 & maize$ext== 1)/sum(table(maize$fert_use == 0 & maize$ext== 1))
table(maize$fert_use == 1 & maize$ext== 1)/sum(table(maize$fert_use == 1 & maize$ext== 1))


pdf("~/data/projects/MAFAP/analysis/maize.pdf")


fit <- locfit((fert_use == 1 & ext== 0 )~log(yield),data=maize[  maize$yield>0 & !is.na(maize$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))[2]  & df$log.yield.>quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))[1],]

df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

plot(df[,1],df$fit, type="l",  xlab="log(yield)", ylab="% change in proportion",ylim = c(-.5,.5), xlim=quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8)),lwd=3)
#lines(df[,1],df$upper, lty=4)
#lines(df[,1],df$lower, lty=4)

fit <- locfit((ext == 1 & fert_use== 0)~log(yield),data=maize[  maize$yield>0 & !is.na(maize$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))[2]  & df$log.yield.>quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))[1],]
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.1,9))
lines(df[,1],df$fit, lwd=3, col="black",lty=2)
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

fit <- locfit((fert_use == 1 & ext == 1 )~log(yield),data=maize[  maize$yield>0 & !is.na(maize$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))[2]  & df$log.yield.>quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))[1],]
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)
#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.5,7.5))
lines(df[,1],df$fit, lwd=3, col="black", lty=3)
abline(h=0, lwd=3, lty=1)
legend("topleft",legend=c("fertilizer","extension","extension + fertilizer"), lty=c(1,2,3), bty="n",lwd=3, col = c("black","black","black"))
dev.off()
####

quantile(log(maize$yield[  maize$yield>0 & !is.na(maize$yield)]),c(.2,.8))
### for beans
table(beans$fert_use == 0 & beans$ext== 0)/sum(table(beans$fert_use == 0 & beans$ext== 0))
table(beans$fert_use == 1 & beans$ext== 0)/sum(table(beans$fert_use == 1 & beans$ext== 0))
table(beans$fert_use == 0 & beans$ext== 1)/sum(table(beans$fert_use == 0 & beans$ext== 1))
table(beans$fert_use == 1 & beans$ext== 1)/sum(table(beans$fert_use == 1 & beans$ext== 1))


pdf("~/data/projects/MAFAP/analysis/beans.pdf")


fit <- locfit((fert_use == 1 & ext== 0 )~log(yield),data=beans[  beans$yield>0 & !is.na(beans$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8))[2]
  & df$log.yield.>quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8))[1]
,]

df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

plot(df[,1],df$fit, type="l",  xlab="log(yield)", ylab="% change in proportion",ylim = c(-.5,.5), xlim=quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8)),lwd=3)
#lines(df[,1],df$upper, lty=4)
#lines(df[,1],df$lower, lty=4)

fit <- locfit((ext == 1 & fert_use== 0)~log(yield),data=beans[  beans$yield>0 & !is.na(beans$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8))[2]
  & df$log.yield.>quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8))[1],]
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.1,9))
lines(df[,1],df$fit, lwd=3, col="black",lty=2)
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

fit <- locfit((fert_use == 1 & ext == 1 )~log(yield),data=beans[  beans$yield>0 & !is.na(beans$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8))[2]
  & df$log.yield.>quantile(log(beans$yield[  beans$yield>0 & !is.na(beans$yield)]),c(.2,.8))[1],]
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)
#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.5,7.5))
lines(df[,1],df$fit, lwd=3, col="black", lty=3)
abline(h=0, lwd=3, lty=1)
legend("topleft",legend=c("fertilizer","extension","extension + fertilizer"), lty=c(1,2,3), bty="n",lwd=3, col = c("black","black","black"))
dev.off()

#### for vegs

table(veg$fert_use == 0 & veg$ext== 0)/sum(table(veg$fert_use == 0 & veg$ext== 0))
table(veg$fert_use == 1 & veg$ext== 0)/sum(table(veg$fert_use == 1 & veg$ext== 0))
table(veg$fert_use == 0 & veg$ext== 1)/sum(table(veg$fert_use == 0 & veg$ext== 1))
table(veg$fert_use == 1 & veg$ext== 1)/sum(table(veg$fert_use == 1 & veg$ext== 1))


pdf("~/data/projects/MAFAP/analysis/veg.pdf")


fit <- locfit((fert_use == 1 & ext== 0 )~log(yield),data=veg[  veg$yield>0 & !is.na(veg$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8))[2]
  & df$log.yield.>quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8))[1]
,]

df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

plot(df[,1],df$fit, type="l",  xlab="log(yield)", ylab="% change in proportion",ylim = c(-.5,.5), xlim=quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8)),lwd=3)
#lines(df[,1],df$upper, lty=4)
#lines(df[,1],df$lower, lty=4)

fit <- locfit((ext == 1 & fert_use== 0)~log(yield),data=veg[  veg$yield>0 & !is.na(veg$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8))[2]
  & df$log.yield.>quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8))[1],]
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.1,9))
lines(df[,1],df$fit, lwd=3, col="black",lty=2)
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

fit <- locfit((fert_use == 1 & ext == 1 )~log(yield),data=veg[  veg$yield>0 & !is.na(veg$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df <- df[ df$log.yield. < quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8))[2]
  & df$log.yield.>quantile(log(veg$yield[  veg$yield>0 & !is.na(veg$yield)]),c(.2,.8))[1],]
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)
#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.5,7.5))
lines(df[,1],df$fit, lwd=3, col="black", lty=3)
abline(h=0, lwd=3, lty=1)
legend("topleft",legend=c("fertilizer","extension","extension + fertilizer"), lty=c(1,2,3), bty="n",lwd=3, col = c("black","black","black"))
dev.off()


all <- subset(all, cropID == 130 | cropID == 120 )
### pooled regressions - to export 
summary(lm(log(yield)~fert_use*ext+as.factor(season)+as.factor(year),data=all[all$yield>0,], weights = w))

inter <- aggregate(all[c("fert_use","w")],list(all$HHID, all2009$, all2009$plotID, all2009$season),max, na.rm=T)
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

### hh fixed effects
### merge in hh level averages
test <- aggregate(all$yield, list(all$HHID), mean, na.rm=TRUE)
names(test) <- c("HHID","av_yld")
all <- merge(all, test, by="HHID")
test <- aggregate(all$fert_use, list(all$HHID), mean, na.rm=TRUE)
names(test) <- c("HHID","av_fert")
all <- merge(all, test, by="HHID")
test <- aggregate(all$ext, list(all$HHID), mean, na.rm=TRUE)
names(test) <- c("HHID","av_ext")
all <- merge(all, test, by="HHID")
all$yld_dev <- all$yield - all$av_yld
all$fert_dev <- all$fert_use - all$av_fert
all$ext_dev <- all$ext - all$av_ext
summary(lm(yld_dev~fert_dev,data=all))
summary(lm(yld_dev~ext_dev,data=all))
summary(lm(yld_dev~fert_dev*ext_dev,data=all))


