#---------------------------------------------------------------------------------
# Script to estimate at EU level (23, 27, 28) Land cover and Land use three digits
#---------------------------------------------------------------------------------
options(stringsAsFactors = TRUE)
library(xlsx)

setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc - with LUE LUD/")

library(ReGenesees)
options(RG.lonely.psu = "average")


s2009 <- read.delim("Survey_2009_cal_wgt.txt")
s2012 <- read.delim("Survey_2012_cal_wgt.txt")
s2015 <- read.delim("Survey_2015_cal_wgt.txt")
s2018 <- read.delim("Survey_2018_cal_wgt.txt")

s2009$year <- "2009"
s2012$year <- "2012"
s2015$year <- "2015"
s2018$year <- "2018"

#---------------------------------------------------
# Europe 23 countries
#---------------------------------------------------

europa23 <- rbind(s2009,
                  s2012[!s2012$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),],
                  s2015[!s2015$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),],
                  s2018[!s2018$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),])

europa23$settlement <- as.factor(europa23$settlement)
europa23$fao_class_name <- as.factor(europa23$fao_class_name)
europa23$lue <- as.factor(europa23$lue)
europa23$lud <- as.factor(europa23$lud)

europa23$year <- as.factor(europa23$year)
europa23$stratum <- as.factor(paste(europa23$year,europa23$stratum,sep="*"))
europa23$ID <- paste(europa23$year,europa23$POINT_ID)
europa23$land_cover_1 <- as.factor(substr(europa23$land_cover,1,3))
europa23$land_use_1 <- as.factor(substr(europa23$land_use,1,4))
europa23$wgts <- europa23$cal_wgt 


# table(dati$stratum)

des <- e.svydesign(data=europa23, ids= ~ ID, strata= ~ stratum, weights= ~ wgts, 
                   self.rep.str= NULL, check.data= TRUE)

des <- des.addvars(des,area=1)

ls <- find.lon.strata(des)
# if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~ year + NUTS0_16)
if (!is.null(ls)) des <- collapse.strata(des)

est_eu23 <- svystatTM(des, ~ 
                        # area +
                             # land_cover_1 +
                             # land_use_1 +
                             settlement +
                             # settl_pc +
                             fao_class_name+
                             lue+
                             lud,
                             estimator="Total",
                             by=~year,
                             vartype=c("se","cv"),
                             conf.int= TRUE, 
                             conf.lev= 0.95)

# save.image("europa23.RData")

est_eu23_t <- as.data.frame(t(est_eu23))
est_eu23_t$variable <- row.names(est_eu23_t)
est_eu23_t$variable[1] <- "variable"
write.xlsx(est_eu23_t, ".\\EU_estimates\\Europe_settl_FAO_LUE_LUD.xlsx", sheetName = "Europe23", 
           col.names = FALSE, row.names = FALSE, append = FALSE)

# write.table(est_eu23,".\\EU_estimates\\Europe23.csv",sep = ";",dec=".",row.names=F)

#---------------------------------------------------
# Europe 27 countries
#---------------------------------------------------
gc()

europa27 <- rbind(s2012[s2012$NUTS0_16 != "HR",],
                  s2015[s2015$NUTS0_16 != "HR",],
                  s2018[s2018$NUTS0_16 != "HR",])

europa27$settlement <- as.factor(europa27$settlement)
europa27$fao_class_name <- as.factor(europa27$fao_class_name)
europa27$lue <- as.factor(europa27$lue)
europa27$lud <- as.factor(europa27$lud)

europa27$year <- as.factor(europa27$year)
europa27$stratum <- as.factor(paste(europa27$year,europa27$stratum,sep="*"))
europa27$ID <- paste(europa27$year,europa27$POINT_ID)
europa27$land_cover_1 <- as.factor(substr(europa27$land_cover,1,3))
europa27$land_use_1 <- as.factor(substr(europa27$land_use,1,4))
europa27$wgts <- europa27$cal_wgt 
# table(dati$stratum)

des <- e.svydesign(data=europa27, ids= ~ ID, strata= ~ stratum, weights= ~ wgts, 
                   self.rep.str= NULL, check.data= TRUE)

des <- des.addvars(des,area=1)

ls <- find.lon.strata(des)
# if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~ year + NUTS0_16)
if (!is.null(ls)) des <- collapse.strata(des)

est_eu27 <- svystatTM(des, ~ 
                        # area +
                        # land_cover_1 +
                        # land_use_1 +
                        settlement +
                        # settl_pc +
                        fao_class_name+
                        lue+
                        lud,
                      estimator="Total",
                      by=~year,
                      vartype=c("se","cv"),
                      conf.int= TRUE, 
                      conf.lev= 0.95)
# save.image("europa27.RData")
est_eu27_t <- as.data.frame(t(est_eu27))
est_eu27_t$variable <- row.names(est_eu27_t)
est_eu27_t$variable[1] <- "variable"
write.xlsx(est_eu27_t, ".\\EU_estimates\\Europe_settl_FAO_LUE_LUD.xlsx", sheetName = "Europe27", 
           col.names = FALSE, row.names = FALSE, append = TRUE)
# write.table(est_eu27,".\\EU_estimates\\Europe27.csv",sep = ";",dec=".",row.names=F)

#---------------------------------------------------
# Europe 28 countries
#---------------------------------------------------
# load("europa27.RData")
europa28 <- rbind(s2015,
                  s2018)

europa28$settlement <- as.factor(europa28$settlement)
europa28$fao_class_name <- as.factor(europa28$fao_class_name)
europa28$lue <- as.factor(europa28$lue)
europa28$lud <- as.factor(europa28$lud)

europa28$year <- as.factor(europa28$year)
europa28$stratum <- as.factor(paste(europa28$year,europa28$stratum,sep="*"))
europa28$ID <- paste(europa28$year,europa28$POINT_ID)
europa28$land_cover_1 <- as.factor(substr(europa28$land_cover,1,3))
europa28$land_use_1 <- as.factor(substr(europa28$land_use,1,4))
europa28$wgts <- europa28$cal_wgt 

# table(dati$stratum)

des <- e.svydesign(data=europa28, ids= ~ ID, strata= ~ stratum, weights= ~ wgts, 
                   self.rep.str= NULL, check.data= TRUE)

des <- des.addvars(des,area=1)

ls <- find.lon.strata(des)
# if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~ year + NUTS0_16)
if (!is.null(ls)) des <- collapse.strata(des)

est_eu28 <- svystatTM(des, ~ 
                        # area +
                        # land_cover_1 +
                        # land_use_1 +
                        settlement +
                        # settl_pc +
                        fao_class_name+
                        lue+
                        lud,
                      estimator="Total",
                      by=~year,
                      vartype=c("se","cv"),
                      conf.int= TRUE, 
                      conf.lev= 0.95)

est_eu28_t <- as.data.frame(t(est_eu28))
est_eu28_t$variable <- row.names(est_eu28_t)
est_eu28_t$variable[1] <- "variable"
write.xlsx(est_eu28_t, ".\\EU_estimates\\Europe_settl_FAO_LUE_LUD.xlsx", sheetName = "Europe28", 
           col.names = FALSE, row.names = FALSE, append = TRUE)

# write.table(est_eu28,".\\EU_estimates\\Europe28.csv",sep = ";",dec=".",row.names=F)

# save.image(file="europa28.RData")