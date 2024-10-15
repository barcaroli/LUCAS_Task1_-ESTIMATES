#---------------------------------------------------------------------------------
# Script to estimate at EU level (22, 26, 26) - Land cover and Land use one digit
#---------------------------------------------------------------------------------
options(stringsAsFactors = TRUE)
library(xlsx)
library(data.table)
path_output <- "D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/EU_estimates/"
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/DATA")

library(ReGenesees)
options(RG.lonely.psu = "average")

s2009 <- fread("Survey_2009_cal_wgt.txt")
s2009 <- s2009[s2009$NUTS0_16 != "UK",]
s2012 <- fread("Survey_2012_cal_wgt.txt")
s2012 <- s2012[s2012$NUTS0_16 != "UK",]
s2015 <- fread("Survey_2015_cal_wgt.txt")
s2015 <- s2015[s2015$NUTS0_16 != "UK",]
s2018 <- fread("Survey_2018_cal_wgt.txt")
s2018 <- s2018[s2018$NUTS0_16 != "UK",]

survey2022 <- fread("Survey_2022_cal_wgt.txt")
s2022 <- NULL
s2022$POINT_ID <- survey2022$POINT_ID
s2022$SURVEY_OBS_TYPE <- NA
s2022$land_cover <- survey2022$SURVEY_LC1
s2022$land_use <- survey2022$SURVEY_LU1
s2022$settlement <- NA
s2022$lud <- NA
s2022$lue <- NA
s2022$fao_class_name <- NA
s2022$ELEV <- survey2022$ELEV
s2022$NUTS0_16 <- survey2022$NUTS0_16
s2022$NUTS1_16 <- NA
s2022$NUTS2_16 <- survey2022$NUTS2_16
s2022$STR05 <- NA
s2022$STR18 <- NA
s2022$STRATUM <- survey2022$STRATUM_LUCAS
s2022$initial_weights <- survey2022$initial_weights
s2022$STRATUM_LABEL <- survey2022$STRATUM_LUCAS
s2022$ELEV2 <- NA
s2022$point_area <- 1
s2022$stratum <- survey2022$STRATUM_LUCAS
s2022$cal_wgt <- survey2022$cal_wgt
s2022 <- as.data.frame(s2022)


s2009$year <- "2009"
s2012$year <- "2012"
s2015$year <- "2015"
s2018$year <- "2018"
s2022$year <- "2022"

# Read population
pop <- read.csv2("pop_nuts.csv")
colnames(pop) <- c("NUTS","Pop2009","Pop2012","Pop2015","Pop2018")

# s2009$settl_pc_22 <- (as.numeric(s2009$settlement)-1) * 1000 / pop[pop$NUTS=="EU22","Pop2009"]
# s2009$settl_pc_26 <- (as.numeric(s2009$settlement)-1) * 1000 / pop[pop$NUTS=="EU26","Pop2009"]
# s2009$settl_pc_27 <- (as.numeric(s2009$settlement)-1) * 1000 / pop[pop$NUTS=="EU27","Pop2009"]
# s2012$settl_pc_22 <- (as.numeric(s2012$settlement)-1) * 1000 / pop[pop$NUTS=="EU22","Pop2012"]
# s2012$settl_pc_26 <- (as.numeric(s2012$settlement)-1) * 1000 / pop[pop$NUTS=="EU26","Pop2012"]
# s2012$settl_pc_27 <- (as.numeric(s2012$settlement)-1) * 1000 / pop[pop$NUTS=="EU27","Pop2012"]
# s2015$settl_pc_22 <- (as.numeric(s2015$settlement)-1) * 1000 / pop[pop$NUTS=="EU22","Pop2015"]
# s2015$settl_pc_26 <- (as.numeric(s2015$settlement)-1) * 1000 / pop[pop$NUTS=="EU26","Pop2015"]
# s2015$settl_pc_27 <- (as.numeric(s2015$settlement)-1) * 1000 / pop[pop$NUTS=="EU27","Pop2015"]
# s2018$settl_pc_22 <- (as.numeric(s2018$settlement)-1) * 1000 / pop[pop$NUTS=="EU22","Pop2018"]
# s2018$settl_pc_26 <- (as.numeric(s2018$settlement)-1) * 1000 / pop[pop$NUTS=="EU26","Pop2018"]
# s2018$settl_pc_27 <- (as.numeric(s2018$settlement)-1) * 1000 / pop[pop$NUTS=="EU27","Pop2018"]

#---------------------------------------------------
# Europe 22 countries
#---------------------------------------------------

europa22 <- rbind(s2009,
              s2012[!s2012$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),],
              s2015[!s2015$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),],
              s2018[!s2018$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),],
              s2022[!s2022$NUTS0_16 %in% c("BG","HR","CY","MT","RO"),])
europa22$settl_pc[europa22$year == 2009] <- (europa22$settlement[europa22$year == 2009]) * 1000 / pop[pop$NUTS=="EU22","Pop2009"]
europa22$settl_pc[europa22$year == 2012] <- (europa22$settlement[europa22$year == 2012]) * 1000 / pop[pop$NUTS=="EU22","Pop2012"]
europa22$settl_pc[europa22$year == 2015] <- (europa22$settlement[europa22$year == 2015]) * 1000 / pop[pop$NUTS=="EU22","Pop2015"]
europa22$settl_pc[europa22$year == 2018] <- (europa22$settlement[europa22$year == 2018]) * 1000 / pop[pop$NUTS=="EU22","Pop2018"]

europa22$settlement <- as.factor(europa22$settlement)
europa22$fao_class_name <- as.factor(europa22$fao_class_name)
europa22$lue <- as.factor(europa22$lue)
europa22$lud <- as.factor(europa22$lud)


europa22$year <- as.factor(europa22$year)
europa22$stratum <- as.factor(paste(europa22$year,europa22$stratum,sep="*"))
europa22$ID <- paste(europa22$year,europa22$POINT_ID)
europa22$land_cover_1 <- as.factor(substr(europa22$land_cover,1,1))
europa22$land_use_1 <- as.factor(substr(europa22$land_use,1,2))
europa22$wgts <- europa22$cal_wgt 

europa22 <- europa22[europa22$land_cover_1 != "U3" & europa22$land_use_1 != "E2",]

# table(dati$stratum)

des22 <- e.svydesign(data=europa22, ids= ~ ID, strata= ~ stratum, weights= ~ wgts, 
                   self.rep.str= NULL, check.data= TRUE)

des22 <- des.addvars(des22,area=1)

ls <- find.lon.strata(des22)
# if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~ year + NUTS0_16)
if (!is.null(ls)) des22 <- collapse.strata(des22)
# save(des,file=paste0(path_output,"Europe22_des.RData"))

est_eu22 <- svystatTM(des22, ~ area +
                        land_cover_1 +
                        land_use_1 ,
                      # settlement +
                      # settl_pc +
                      # fao_class_name,
                             estimator="Total",
                             by=~year,
                             vartype=c("se","cv"),
                             conf.int= TRUE, 
                             conf.lev= 0.95)
# sum(est_eu22[1:8,])
est_eu22_t <- as.data.frame(t(est_eu22))
est_eu22_t$variable <- row.names(est_eu22_t)
est_eu22_t$variable[1] <- "variable"

write.xlsx(est_eu22_t, file=paste0(path_output,"Europe_LC_LU_1digit.xlsx"), 
           sheetName = "Europe22", 
           col.names = FALSE, row.names = FALSE, append = FALSE)

# write.table(est_eu22,".\\EU_estimates\\Europe22.csv",sep = ";",dec=".",row.names=F)

#---------------------------------------------------
# Europe 26 countries
#---------------------------------------------------

europa26 <- rbind(s2012[s2012$NUTS0_16 != "HR",],
                  s2015[s2015$NUTS0_16 != "HR",],
                  s2018[s2018$NUTS0_16 != "HR",],
                  s2022[s2022$NUTS0_16 != "HR",])
europa26$settl_pc[europa26$year == 2009] <- (europa26$settlement[europa26$year == 2009]) * 1000 / pop[pop$NUTS=="EU26","Pop2009"]
europa26$settl_pc[europa26$year == 2012] <- (europa26$settlement[europa26$year == 2012]) * 1000 / pop[pop$NUTS=="EU26","Pop2012"]
europa26$settl_pc[europa26$year == 2015] <- (europa26$settlement[europa26$year == 2015]) * 1000 / pop[pop$NUTS=="EU26","Pop2015"]
europa26$settl_pc[europa26$year == 2018] <- (europa26$settlement[europa26$year == 2018]) * 1000 / pop[pop$NUTS=="EU26","Pop2018"]

europa26$settlement <- as.factor(europa26$settlement)
europa26$fao_class_name <- as.factor(europa26$fao_class_name)


europa26$year <- as.factor(europa26$year)
europa26$stratum <- as.factor(paste(europa26$year,europa26$stratum,sep="*"))
europa26$ID <- paste(europa26$year,europa26$POINT_ID)
europa26$land_cover_1 <- as.factor(substr(europa26$land_cover,1,1))
europa26$land_use_1 <- as.factor(substr(europa26$land_use,1,2))
europa26$wgts <- europa26$cal_wgt 

europa26 <- europa26[europa26$land_cover_1 != "U3" & europa26$land_use_1 != "E2",]
# table(dati$stratum)

des26 <- e.svydesign(data=europa26, ids= ~ ID, strata= ~ stratum, weights= ~ wgts, 
                   self.rep.str= NULL, check.data= TRUE)

des26 <- des.addvars(des26,area=1)

ls <- find.lon.strata(des26)
# if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~ year + NUTS0_16)
if (!is.null(ls)) des26 <- collapse.strata(des26)

est_eu26 <- svystatTM(des26, ~ area +
                        land_cover_1 +
                        land_use_1 ,
                      # settlement +
                      # settl_pc +
                      # fao_class_name,
                      estimator="Total",
                      by=~year,
                      vartype=c("se","cv"),
                      conf.int= TRUE, 
                      conf.lev= 0.95)

est_eu26_t <- as.data.frame(t(est_eu26))
est_eu26_t$variable <- row.names(est_eu26_t)
est_eu26_t$variable[1] <- "variable"
write.xlsx(est_eu26_t, file=paste0(path_output,"Europe_LC_LU_1digit.xlsx"), 
           sheetName = "Europe26", 
           col.names = FALSE, row.names = FALSE, append = TRUE)

#---------------------------------------------------
# Europe 27 countries
#---------------------------------------------------

europa27 <- rbind(s2015,
                  s2018,
                  s2022)
europa27$settl_pc[europa27$year == 2009] <- (europa27$settlement[europa27$year == 2009]) * 1000 / pop[pop$NUTS=="EU27","Pop2009"]
europa27$settl_pc[europa27$year == 2012] <- (europa27$settlement[europa27$year == 2012]) * 1000 / pop[pop$NUTS=="EU27","Pop2012"]
europa27$settl_pc[europa27$year == 2015] <- (europa27$settlement[europa27$year == 2015]) * 1000 / pop[pop$NUTS=="EU27","Pop2015"]
europa27$settl_pc[europa27$year == 2018] <- (europa27$settlement[europa27$year == 2018]) * 1000 / pop[pop$NUTS=="EU27","Pop2018"]

europa27$settlement <- as.factor(europa27$settlement)
europa27$fao_class_name <- as.factor(europa27$fao_class_name)


europa27$year <- as.factor(europa27$year)
europa27$stratum <- as.factor(paste(europa27$year,europa27$stratum,sep="*"))
europa27$ID <- paste(europa27$year,europa27$POINT_ID)
europa27$land_cover_1 <- as.factor(substr(europa27$land_cover,1,1))
europa27$land_use_1 <- as.factor(substr(europa27$land_use,1,2))
europa27$wgts <- europa27$cal_wgt 
europa27 <- europa27[europa27$land_cover_1 != "U3" & europa27$land_use_1 != "E2",]

# table(dati$stratum)

des27 <- e.svydesign(data=europa27, ids= ~ ID, strata= ~ stratum, weights= ~ wgts, 
                   self.rep.str= NULL, check.data= TRUE)

des27 <- des.addvars(des27,area=1)

ls <- find.lon.strata(des27)
# if (!is.null(ls)) des <- collapse.strata(des, block.vars = ~ year + NUTS0_16)
if (!is.null(ls)) des27 <- collapse.strata(des27)

est_eu27 <- svystatTM(des27, ~ area +
                        land_cover_1 +
                        land_use_1 ,
                        # settlement +
                        # settl_pc +
                        # fao_class_name,
                      estimator="Total",
                      by=~year,
                      vartype=c("se","cv"),
                      conf.int= TRUE, 
                      conf.lev= 0.95)

est_eu27_t <- as.data.frame(t(est_eu27))
est_eu27_t$variable <- row.names(est_eu27_t)
est_eu27_t$variable[1] <- "variable"
write.xlsx(est_eu27_t, file=paste0(path_output,"Europe_LC_LU_1digit.xlsx"), 
           sheetName = "Europe27", 
           col.names = FALSE, row.names = FALSE, append = TRUE)

save.image(file=paste0(path_output,"Europe_1digit.RData"))