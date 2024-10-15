#--------------------------------------
# Application of svyDelta function
# to calculate sampling variance
# of variations 2018 vs 2022
# ONLY ON PANEL DATA
#--------------------------------------
setwd("D:/Google Drive/LUCAS 2024/Item_2")
options(scipen=999)
library(data.table)
library(ReGenesees)
library(xlsx)
data_path <- "D:\\Google Drive\\LUCAS 2024\\data\\"

load(paste0(data_path,"master.RData"))
master$CLC18_2 <- as.factor(substr(master$CLC18,1,2))
master$STR18 <- as.factor(master$STR18)
table(master$CLC18_2,useNA="ifany")
table(master$STR18,useNA="ifany")
master <- master[!is.na(master$STR18) & !is.na(master$CLC18_2),]
master$STR18 <- ifelse(master$STR18 == 9,10,master$STR18)
master$STR18 <- as.factor(master$STR18)


areas <- fread(paste0(data_path,"NUTS2_16 areas.csv"))
areas$NUTS0_16 <- as.factor(substr(areas$NUTS2_16,1,2))
sum(areas$Area)
length(levels(areas$NUTS0_16))
areas2 <- aggregate(Area~NUTS0_16,data=areas,FUN=sum)
areas2 <- areas2[-28,]


s2018 <- fread(paste0(data_path,"Survey_2018_cal_wgt.txt"))
s2018 <- s2018[s2018$NUTS0_16 != "UK",]

load(paste0(data_path,"s2022.RData"))

panel <- intersect(s2018$POINT_ID,s2022$POINT_ID)

s2018 <- s2018[s2018$POINT_ID %in% panel,]

s2022 <- s2022[s2022$POINT_ID %in% panel,]


# #-----------
# # LUCAS 2018
# #-----------
s2018 <- merge(s2018,master[,c("POINT_ID","CLC18_2"),],by="POINT_ID")
table(s2018$CLC18_2,useNA="ifany")
table(s2018$STR18,useNA="ifany")
sum(s2018$cal_wgt)
LC2018 <- as.factor(substr(s2018$land_cover,1,1))
a <- model.matrix(~LC2018-1)
s2018 <- cbind(s2018,a)
# s2018$LC1_2_2018 <- as.factor(substr(s2018$land_cover,1,2))
# s2018$LC1_3_2018 <- as.factor(s2018$land_cover)
LU2018 <- as.factor(substr(s2018$land_use,1,2))
a <- model.matrix(~LU2018-1)
s2018 <- cbind(s2018,a)
# s2018$LU1_2_2018 <- as.factor(substr(s2018$land_use,1,3))
# s2018$LU1_3_2018 <- as.factor(s2018$land_use )
s2018 <- s2018[order(s2018$stratum),]
s2018$NUTS0_16 <- as.factor(s2018$NUTS0_16)
s2018$stratum <- as.factor(s2018$stratum)
s2018$CLC18_2 <- as.factor(s2018$CLC18_2)
s2018$STR18 <- as.factor(s2018$STR18)
# #--------------------------------------------------------------
# # calibration of 2018 data (only total area by NUTS0)
des_2018 <- e.svydesign(data=s2018, ids= ~ POINT_ID, strata= ~ stratum,
                        weights= ~ cal_wgt, self.rep.str= NULL, check.data= TRUE)
ls <- find.lon.strata(des_2018)
if (!is.null(ls)) des_2018 <- collapse.strata(des_2018)
# save(des_2018,file="des_2018.RData")
# calibration
poptot2018 <- pop.template(data=master,
                           calmodel= ~ STR18+CLC18_2 - 1, partition= FALSE)
poptot2018 <- fill.template(universe=master,template=poptot2018)
# poptot2018[1,] <- areas2$Area
cal_2018 <- e.calibrate(design=des_2018,
                             df.population= poptot2018,
                             calmodel= ~ STR18+CLC18_2 - 1,
                             calfun= "linear")
check.cal(cal_2018)
s2018$wgt_cal <- weights(cal_2018)
sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018A)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018B)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018C)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018D)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018E)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018F)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018G)/sum(s2018$wgt_cal)
sum(s2018$wgt_cal*s2018$LC2018H)/sum(s2018$wgt_cal)
# 
# 
# #-----------
# # LUCAS 2022
# #-----------
LC2022 <- as.factor(substr(s2022$SURVEY_LC1,1,1))
a <- model.matrix(~LC2022-1)
s2022 <- cbind(s2022,a)
# s2022$LC1_2_2022 <- as.factor(substr(s2022$SURVEY_LC1,1,2))
# s2022$LC1_3_2022 <- as.factor(s2022$SURVEY_LC1)
LU2022 <- as.factor(substr(s2022$SURVEY_LU1,1,2))
a <- model.matrix(~LU2022-1)
s2022 <- cbind(s2022,a)
# s2022$LU1_2_2022 <- as.factor(substr(s2022$SURVEY_LU1,1,3))
# s2022$LU1_3_2022 <- as.factor(s2022$SURVEY_LU1)
s2022 <- s2022[order(s2022$WGT_LUCAS),]
s2022$NUTS0_16 <- as.factor(s2022$NUTS0_16)
s2022$STRATUM_LUCAS <- as.factor(s2022$STRATUM_LUCAS)
s2022$CLC18_2 <- as.factor(s2022$CLC18_2)
s2022$STR18 <- as.factor(s2022$STR18)
# #--------------------------------------------------------------
# # calibration of 2022 data (only total area by NUTS0)
des_2022 <- e.svydesign(data=s2022, ids= ~ POINT_ID, strata= ~ STRATUM_LUCAS,
                         weights= ~ WGT_LUCAS, self.rep.str= NULL, check.data= TRUE)
ls <- find.lon.strata(des_2022)
if (!is.null(ls)) des_2022 <- collapse.strata(des_2022)
# save(des_2022,file="des_2022.RData")
# calibration
# poptot2022 <- pop.template(data=des_2022,
#                        calmodel= ~ CLC18_2 - 1, partition= FALSE)
# poptot2022 <- fill.template(universe=master,template=poptot2022)
# poptot2022[1,] <- areas2$Area
cal_2022 <- e.calibrate(design=des_2022,
                         df.population= poptot2018,
                         calmodel= ~ STR18:CLC18_2 - 1,
                         calfun= "linear")
check.cal(cal_2022)
s2022$wgt_cal <- weights(cal_2022)
sum(s2022$wgt_cal)
# save.image(file="svyDeltaPanel.RData")
sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022A)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022B)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022C)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022D)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022E)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022F)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022G)/sum(s2022$wgt_cal)
sum(s2022$wgt_cal*s2022$LC2022H)/sum(s2022$wgt_cal)
# load("svyDeltaPanel.RData")
#-------------------------------
# Calculate 2018/2022 variations
#-------------------------------

(sum(s2022$wgt_cal*s2022$LC2022C)/sum(s2022$wgt_cal) - sum(s2018$wgt_cal*s2018$LC2018C)/sum(s2018$wgt_cal)) / 
  (sum(s2022$wgt_cal*s2022$LC2022C)/sum(s2022$wgt_cal))

(sum(s2022$LC2022C)/nrow(s2022) - sum(s2018$LC2018C)/nrow(s2022)) / 
  (sum(s2022$LC2022C)/nrow(s2022))

#------------------------------------------------------------------------------
artificial <- svyDelta(expr=expression((LC2022A.2 - LC2018A.1)/LC2018A.1),
                       design1=cal_2018, design2=cal_2022, by = NULL,
                       des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                       vartype = c("se", "cv", "cvpct"),
                       conf.int = TRUE, conf.lev = 0.95)
artificial
artificial_NUTS0 <- svyDelta(expr=expression((LC2022A.2 - LC2018A.1)/LC2018A.1),
                             design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                             des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                             vartype = c("se", "cv", "cvpct"),
                             conf.int = TRUE, conf.lev = 0.95)
artificial_NUTS0
write.xlsx(artificial,"svyDeltaPanel_results.xlsx",append=FALSE,sheetName = "art")
write.xlsx(artificial_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "artNUTS0")
#------------------------------------------------------------------------------

cropland <- svyDelta(expr=expression((LC2022B.2 - LC2018B.1)/LC2018B.1),
                     design1=cal_2018, design2=cal_2022, by = NULL,
                     des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                     vartype = c("se", "cv", "cvpct"),
                     conf.int = TRUE, conf.lev = 0.95)
cropland
cropland_NUTS0 <- svyDelta(expr=expression((LC2022B.2 - LC2018B.1)/LC2018B.1),
                           design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                           des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                           vartype = c("se", "cv", "cvpct"),
                           conf.int = TRUE, conf.lev = 0.95)
cropland_NUTS0
write.xlsx(cropland,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "crop")
write.xlsx(cropland_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "cropNUTS2")
#------------------------------------------------------------------------------

woodland <- svyDelta(expr=expression((LC2022C.2 - LC2018C.1)/LC2018C.1),
                     design1=cal_2018, design2=cal_2022, by = NULL,
                     des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                     vartype = c("se", "cv", "cvpct"),
                     conf.int = TRUE, conf.lev = 0.95)
woodland
woodland_NUTS0 <- svyDelta(expr=expression((LC2022C.2 - LC2018C.1)/LC2018C.1),
                           design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                           des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                           vartype = c("se", "cv", "cvpct"),
                           conf.int = TRUE, conf.lev = 0.95)
woodland_NUTS0
write.xlsx(woodland,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "wood")
write.xlsx(woodland_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "woodNUTS2")
#------------------------------------------------------------------------------

shrubland <- svyDelta(expr=expression((LC2022D.2 - LC2018D.1)/LC2018D.1),
                      design1=cal_2018, design2=cal_2022, by = NULL,
                      des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                      vartype = c("se", "cv", "cvpct"),
                      conf.int = TRUE, conf.lev = 0.95)
shrubland
shrubland_NUTS0 <- svyDelta(expr=expression((LC2022D.2 - LC2018D.1)/LC2018D.1),
                            design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                            des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                            vartype = c("se", "cv", "cvpct"),
                            conf.int = TRUE, conf.lev = 0.95)
shrubland_NUTS0
write.xlsx(shrubland,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "shrub")
write.xlsx(shrubland_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "shrubNUTS2")
#------------------------------------------------------------------------------

grassland <- svyDelta(expr=expression((LC2022E.2 - LC2018E.1)/LC2018E.1),
                      design1=cal_2018, design2=cal_2022, by = NULL,
                      des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                      vartype = c("se", "cv", "cvpct"),
                      conf.int = TRUE, conf.lev = 0.95)
grassland
grassland_NUTS0 <- svyDelta(expr=expression((LC2022E.2 - LC2018E.1)/LC2018E.1),
                            design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                            des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                            vartype = c("se", "cv", "cvpct"),
                            conf.int = TRUE, conf.lev = 0.95)
grassland_NUTS0
write.xlsx(grassland,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "grass")
write.xlsx(grassland_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "grassNUTS2")
#------------------------------------------------------------------------------

bareland <- svyDelta(expr=expression((LC2022F.2 - LC2018F.1)/LC2018F.1),
                     design1=cal_2018, design2=cal_2022, by = NULL,
                     des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                     vartype = c("se", "cv", "cvpct"),
                     conf.int = TRUE, conf.lev = 0.95)
bareland
bareland_NUTS0 <- svyDelta(expr=expression((LC2022F.2 - LC2018F.1)/LC2018F.1),
                           design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                           des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                           vartype = c("se", "cv", "cvpct"),
                           conf.int = TRUE, conf.lev = 0.95)
bareland_NUTS0
write.xlsx(bareland,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "bare")
write.xlsx(bareland_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "bareNUTS2")
#
#------------------------------------------------------------------------------

water <- svyDelta(expr=expression((LC2022G.2 - LC2018G.1)/LC2018G.1),
                  design1=cal_2018, design2=cal_2022, by = NULL,
                  des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                  vartype = c("se", "cv", "cvpct"),
                  conf.int = TRUE, conf.lev = 0.95)
water
water_NUTS0 <- svyDelta(expr=expression((LC2022G.2 - LC2018G.1)/LC2018G.1),
                        design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                        des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                        vartype = c("se", "cv", "cvpct"),
                        conf.int = TRUE, conf.lev = 0.95)
water_NUTS0
write.xlsx(water,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "water")
write.xlsx(water_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "waterNUTS2")
#
#------------------------------------------------------------------------------

wetland <- svyDelta(expr=expression((LC2022H.2 - LC2018H.1)/LC2018H.1),
                    design1=cal_2018, design2=cal_2022, by = NULL,
                    des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                    vartype = c("se", "cv", "cvpct"),
                    conf.int = TRUE, conf.lev = 0.95)
wetland
# wetland_NUTS0 <- svyDelta(expr=expression((LC2018H.1 - LC2022H.2)/LC2018H.1),
#                         design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
#                         des.INDEP = FALSE, rho.STRAT = c("noStrat"),
#                         vartype = c("se", "cv", "cvpct"),
#                         conf.int = TRUE, conf.lev = 0.95)
# wetland_NUTS0
write.xlsx(wetland,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "wetland")
# write.xlsx(wetland_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "wetlandNUTS2")

#------------------------------------------------------------------------------

primary <- svyDelta(expr=expression((LU2022U1.2 - LU2018U1.1)/LU2018U1.1),
                    design1=cal_2018, design2=cal_2022, by = NULL,
                    des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                    vartype = c("se", "cv", "cvpct"),
                    conf.int = TRUE, conf.lev = 0.95)
primary
primary_NUTS0 <- svyDelta(expr=expression((LU2022U1.2 - LU2018U1.1)/LU2018U1.1),
                          design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                          des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                          vartype = c("se", "cv", "cvpct"),
                          conf.int = TRUE, conf.lev = 0.95)
primary_NUTS0
write.xlsx(primary,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "primary")
write.xlsx(primary_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "primaryNUTS2")

#------------------------------------------------------------------------------

secondary <- svyDelta(expr=expression((LU2022U2.2 - LU2018U2.1)/LU2018U2.1),
                      design1=cal_2018, design2=cal_2022, by = NULL,
                      des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                      vartype = c("se", "cv", "cvpct"),
                      conf.int = TRUE, conf.lev = 0.95)
secondary
# secondary_NUTS0 <- svyDelta(expr=expression((LU2018U2.1 - LU2022U2.2)/LU2018U2.1),
#                           design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
#                           des.INDEP = FALSE, rho.STRAT = c("noStrat"),
#                           vartype = c("se", "cv", "cvpct"),
#                           conf.int = TRUE, conf.lev = 0.95)
# secondary_NUTS0
write.xlsx(secondary,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "secondary")
# write.xlsx(secondary_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "secondaryNUTS2")

#------------------------------------------------------------------------------

tertiary <- svyDelta(expr=expression((LU2022U3.2 - LU2018U3.1)/LU2018U3.1), 
                     design1=cal_2018, design2=cal_2022, by = NULL,
                     des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                     vartype = c("se", "cv", "cvpct"),
                     conf.int = TRUE, conf.lev = 0.95)
tertiary
tertiary_NUTS0 <- svyDelta(expr=expression((LU2022U3.2 - LU2018U3.1)/LU2018U3.1), 
                           design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                           des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                           vartype = c("se", "cv", "cvpct"),
                           conf.int = TRUE, conf.lev = 0.95)
tertiary_NUTS0
write.xlsx(tertiary,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "tertiary")
write.xlsx(tertiary_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "tertiaryNUTS2")

#------------------------------------------------------------------------------

unused <- svyDelta(expr=expression((LU2022U4.2 - LU2018U4.1)/LU2018U4.1), 
                   design1=cal_2018, design2=cal_2022, by = NULL,
                   des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                   vartype = c("se", "cv", "cvpct"),
                   conf.int = TRUE, conf.lev = 0.95)
unused
unused_NUTS0 <- svyDelta(expr=expression((LU2022U4.2 - LU2018U4.1)/LU2018U4.1), 
                         design1=cal_2018, design2=cal_2022, by = ~ NUTS0_16,
                         des.INDEP = FALSE, rho.STRAT = c("noStrat"),
                         vartype = c("se", "cv", "cvpct"),
                         conf.int = TRUE, conf.lev = 0.95)
unused_NUTS0
write.xlsx(unused,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "unused")
write.xlsx(unused_NUTS0,"svyDeltaPanel_results.xlsx",append=TRUE,sheetName = "unusedNUTS2")

#########################################################
save.image(file="svyDeltaPanel.RData")