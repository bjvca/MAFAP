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

all2011$year <- 2011
all2013$year <- 2013

all <- rbind(all2011, all2013)





#################

library(locfit)

#pdf("~/data/projects/MAFAP/analysis/yields.pdf",sep=""))


fit <- locfit((fert_use == "Yes")~log(yield),data=all[  all$yield>0 & !is.na(all$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

plot(df[,1],df$fit, type="l",  xlab="log(yield)", ylab="% change in proportion",ylim = c(-.2,.8), xlim=c(5.2,7.3),lwd=3)
#lines(df[,1],df$upper, lty=4)
#lines(df[,1],df$lower, lty=4)

fit <- locfit(ext~log(yield),data=all[  all$yield>0 & !is.na(all$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)

#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.3,8))
lines(df[,1],df$fit, lwd=3, col="red")
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

fit <- locfit((fert_use == "Yes")*ext~log(yield),data=all[  all$yield>0 & !is.na(all$yield),])
fit0 <- scb(fit, ev = lfgrid(100))
### this is an extremely inelegant solution - I can not extract the confidence bands from the scb object, I can only get them when I print the object to the screen.  So I decided to print the to disk and then load then again
sink(file="temp.txt")
print(fit0)
sink()
df <- read.table("temp.txt")
df$fit <- (df$fit - mean(df$fit))/mean(df$fit)
#plot(df[,1],df$fit, type="l",  xlab="log(total land)", ylab="probability", xlim=c(5.5,7.5))
lines(df[,1],df$fit, lwd=3, col="green")
#lines(df[,1],df$lower, lty=4)
#text(-0.5, 0.1, "commercialized")

rug(log(all$yield[  all$yield>0 & !is.na(all$yield)]))

quantile(log(all$yield[  all$yield>0 & !is.na(all$yield)]),c(.2,.9))








