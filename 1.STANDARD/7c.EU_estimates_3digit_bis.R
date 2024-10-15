#---------------------------------------------------------------------------------
# Script to estimate at EU level (22, 26, 26) - Land cover and Land use 3 digits
#---------------------------------------------------------------------------------
# This is to be launched after 7cEU_estimates_digit.R
#---------------------------------------------------------------------------------
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/EU_estimates")
load("Europe_3digit.RData")
library(ReGenesees)
library(xlsx)
est_eu22 <- svystatTM(des22, ~ area +
                        # land_cover_1 ,
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

write.xlsx(est_eu22_t, file=paste0(path_output,"Europe_LC_LU_3digit_bis.xlsx"), 
           sheetName = "Europe22", 
           col.names = FALSE, row.names = FALSE, append = FALSE)
est_eu26 <- svystatTM(des26, ~ area +
                        # land_cover_1 ,
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
write.xlsx(est_eu26_t, file=paste0(path_output,"Europe_LC_LU_3digit_bis.xlsx"), 
           sheetName = "Europe26", 
           col.names = FALSE, row.names = FALSE, append = TRUE)
est_eu27 <- svystatTM(des27, ~ area +
                        # land_cover_1 ,
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
write.xlsx(est_eu27_t, file=paste0(path_output,"Europe_LC_LU_3digit_bis.xlsx"), 
           sheetName = "Europe27", 
           col.names = FALSE, row.names = FALSE, append = TRUE)
