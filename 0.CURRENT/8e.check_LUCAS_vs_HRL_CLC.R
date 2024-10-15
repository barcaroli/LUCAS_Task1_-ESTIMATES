##############################################################################
# LUCAS - Check of coeherence between LUCAS and HRL and CLC
##############################################################################

setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc")
dire <- "C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc"
load("m_clc.RData")
load("master_with_imperv.RData")
m <- read.csv2("master2019_with_weights_and_strata_2018.csv")
countries <- levels(m$NUTS0_16)

# CZECH REPUBLIC------------------------------------------------
s2009 <- read.delim("Survey_2009_cal_wgt.txt")
s2012 <- read.delim("Survey_2012_cal_wgt.txt")
s2015 <- read.delim("Survey_2015_cal_wgt.txt")
s2018 <- read.delim("Survey_2018_cal_wgt.txt")

s2009 <- s2009[s2009$NUTS0_16=="CZ",]
s2012 <- s2012[s2012$NUTS0_16=="CZ",]
s2015 <- s2015[s2015$NUTS0_16=="CZ",]
s2018 <- s2018[s2018$NUTS0_16=="CZ",]

est_current <- rep(0,4)
s2009$art <- ifelse(substr(s2009$land_cover,1,1)=="A",1,0)
table(s2009$art)
est_current[1] <- sum(s2009$art*s2009$cal_wgt)

s2012$art <- ifelse(substr(s2012$land_cover,1,1)=="A",1,0)
table(s2012$art)
est_current[2] <- sum(s2012$art*s2012$cal_wgt)

s2015$art <- ifelse(substr(s2015$land_cover,1,1)=="A",1,0)
table(s2015$art)
est_current[3] <- sum(s2015$art*s2015$cal_wgt)

s2018$art <- ifelse(substr(s2018$land_cover,1,1)=="A",1,0)
table(s2018$art)
est_current[4] <- sum(s2018$art*s2018$cal_wgt)

est_new <- rep(0,4)
s2009 <- merge(s2009,m_CLC[,c("POINT_ID","art12")])
s2009 <- merge(s2009,m2[,c("POINT_ID","imp15")])
# s2009$artsat <- ifelse(s2009$imp15 > mean(s2009$imp15) & s2009$art12 > mean(s2009$art12),1,0)
s2009$artsat <- ifelse(s2009$imp15 > mean(s2009$imp15),1,0)
# s2018$artsat <- ifelse(s2018$art12 > mean(s2018$art12),1,0)
table(s2009$artsat)
table(s2009$art,s2009$artsat)
est_new[1] <- sum(s2009$artsat*s2009$cal_wgt)

s2012 <- merge(s2012,m_CLC[,c("POINT_ID","art12")])
s2012 <- merge(s2012,m2[,c("POINT_ID","imp15")])
# s2012$artsat <- ifelse(s2012$imp15 > mean(s2012$imp15) & s2012$art12 > mean(s2012$art12),1,0)
s2012$artsat <- ifelse(s2012$imp15 > mean(s2012$imp15),1,0)
# s2018$artsat <- ifelse(s2018$art12 > mean(s2018$art12),1,0)
table(s2012$artsat)
table(s2012$art,s2012$artsat)
est_new[2] <- sum(s2012$artsat*s2012$cal_wgt)

s2015 <- merge(s2015,m_CLC[,c("POINT_ID","art12")])
s2015 <- merge(s2015,m2[,c("POINT_ID","imp15")])
# s2015$artsat <- ifelse(s2015$imp15 > mean(s2015$imp15) & s2015$art12 > mean(s2015$art12),1,0)
s2015$artsat <- ifelse(s2015$imp15 > mean(s2015$imp15),1,0)
# s2018$artsat <- ifelse(s2018$art12 > mean(s2018$art12),1,0)

table(s2015$artsat)
table(s2015$art,s2015$artsat)
est_new[3] <- sum(s2015$artsat*s2015$cal_wgt)

s2018 <- merge(s2018,m_CLC[,c("POINT_ID","art18")])
s2018 <- merge(s2018,m2[,c("POINT_ID","imp15")])
# s2018$artsat <- ifelse(s2018$imp15 > mean(s2018$imp15) & s2018$art18 > mean(s2018$art18),1,0)
s2018$artsat <- ifelse(s2018$imp15 > mean(s2018$imp15),1,0)
# s2018$artsat <- ifelse(s2018$art12 > mean(s2018$art18),1,0)


table(s2018$artsat)
table(s2018$art,s2018$artsat)
est_new[4] <- sum(s2018$artsat*s2018$cal_wgt)

est_current
est_new

# CROATIA------------------------------------------------
s2015 <- read.delim("Survey_2015_cal_wgt.txt")
s2018 <- read.delim("Survey_2018_cal_wgt.txt")
s2015 <- s2015[s2015$NUTS0_16=="HR",]
s2018 <- s2018[s2018$NUTS0_16=="HR",]

s2015 <- merge(s2015,m_CLC[,c("POINT_ID","art12")])
s2015 <- merge(s2015,m2[,c("POINT_ID","imp15")])

s2018 <- merge(s2018,m_CLC[,c("POINT_ID","art18")])
s2018 <- merge(s2018,m2[,c("POINT_ID","imp15")])

est_current <- rep(0,2)
s2015$art <- ifelse(substr(s2015$land_cover,1,1)=="A",1,0)
table(s2015$art)
est_current[1] <- sum(s2015$art*s2015$cal_wgt)

s2018$art <- ifelse(substr(s2018$land_cover,1,1)=="A",1,0)
table(s2018$art)
est_current[2] <- sum(s2018$art*s2018$cal_wgt)


est_new <- rep(0,2)
# s2015$artsat <- ifelse(s2015$imp15 > mean(s2015$imp15) & s2015$art12 > mean(s2015$art12),1,0)
s2015$artsat <- ifelse(s2015$imp15 > mean(s2015$imp15),1,0)
s2018$artsat <- ifelse(s2018$art12 > mean(s2018$art12),1,0)

table(s2015$artsat)
table(s2015$art,s2015$artsat)
est_new[1] <- sum(s2015$artsat*s2015$cal_wgt)

# s2018$artsat <- ifelse(s2018$imp15 > mean(s2018$imp15) & s2018$art18 > mean(s2018$art18),1,0)
s2018$artsat <- ifelse(s2018$imp15 > mean(s2018$imp15),1,0)
# s2018$artsat <- ifelse(s2018$art12 > mean(s2018$art18),1,0)


table(s2018$artsat)
table(s2018$art,s2018$artsat)
est_new[2] <- sum(s2018$artsat*s2018$cal_wgt)

est_current
est_new
