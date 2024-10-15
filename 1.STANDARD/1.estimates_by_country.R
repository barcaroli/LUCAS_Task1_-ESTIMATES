##############################################################
# LUCAS - Calibration and estimation by country 2018
##############################################################
options(stringsAsFactors = TRUE)
library(data.table)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD")

path_data <- "D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/DATA/"

library(ReGenesees)

# Prepare the path for the output
dire <- getwd()
direnew1 <- paste(dire, "/estimates2022", sep = "")
# if (dir.exists(direnew1))
#   unlink(direnew1,recursive=TRUE)
if (!dir.exists(direnew1)) 
  dir.create(direnew1)

direnew2 <- paste(dire, "/weights2022", sep = "")
# if (dir.exists(direnew2))
#   unlink(direnew2,recursive=TRUE)
if (!dir.exists(direnew2)) 
  dir.create(direnew2)
# Read areas

areas <- read.csv2(paste0(path_data,"NUTS2_16_areas.csv"))

# Read population
pop <- read.csv2(paste0(path_data,"Pop_nuts.csv"))
colnames(pop) <- c("NUTS","Pop2009","Pop2012","Pop2015","Pop2018")


# Read survey 2022
s2022 <- fread(paste0(path_data,"survey_2022.txt"))
# dati <- fread(paste0(path_data,"EU_LUCAS_2022.csv"))
# s <- fread(paste0(path_data,"sample_LUCAS_2022.csv"))
# s$V1 <- s$SURVEY_LC1 <- s$SURVEY_LU1 <- NULL
# s2022 <- merge(s,dati[,c("POINT_ID","SURVEY_LC1","SURVEY_LU1")],by="POINT_ID")
# s2022 <- s2022[!is.na(s2022$SURVEY_LC1) & s2022$SURVEY_LC1 != 8 & s2022$SURVEY_LU1 != 8,]
#------------------------------------------

s2022$SURVEY_LC1 <- factor(s2022$SURVEY_LC1)
s2022$SURVEY_LU1 <- factor(s2022$SURVEY_LU1)

s2022$SURVEY_LC1_1 <- as.factor(substr(as.character(s2022$SURVEY_LC1),1,1))
table(s2022$SURVEY_LC1_1, useNA="ifany")
s2022$SURVEY_LC1_2 <- as.factor(substr(as.character(s2022$SURVEY_LC1),1,2))
table(s2022$SURVEY_LC1_2, useNA="ifany")
s2022$SURVEY_LC1_3 <- as.factor(substr(as.character(s2022$SURVEY_LC1),1,3))
table(s2022$SURVEY_LC1_3, useNA="ifany")
s2022$SURVEY_LU1_1 <- as.factor(substr(as.character(s2022$SURVEY_LU1),1,2))
table(s2022$SURVEY_LU1_1, useNA="ifany")
s2022$SURVEY_LU1_2 <- as.factor(substr(as.character(s2022$SURVEY_LU1),1,3))
table(s2022$SURVEY_LU1_2, useNA="ifany")
s2022$SURVEY_LU1_3 <- as.factor(substr(as.character(s2022$SURVEY_LU1),1,4))
table(s2022$SURVEY_LU1_3, useNA="ifany")

# write.table(s2022,file=(paste0(path_data,"survey_2022.txt")),sep="\t",quote=F,row.names=F)

# Read master 
# giulio
load(paste0(path_data,"master.RData"))

# sum(master$point_area)
master$ELEV2 <- ifelse(master$ELEV < 100, 1, 
                    ifelse(master$ELEV < 200, 2,
                      ifelse(master$ELEV < 500, 3,
                             ifelse(master$ELEV < 1000, 4,
                                    ifelse(master$ELEV < 1500, 5, 6)))))
table(master$ELEV2)
master$ELEV2 <- as.factor(master$ELEV2)
# master <- merge(master,master2)
# master <- merge(master,master3)

m4 <- fread(paste0(path_data,"master2019_with_point_area.csv"))
master <- merge(master,m4[,c("POINT_ID","point_area")],by="POINT_ID")


# exclusion of NUTS2  --> CHIEDERE CONFERMA!
master <- master[master$NUTS0_16 != "UK",]
master$NUTS0_16 <- factor(master$NUTS0_16)
master$NUTS1_16 <- factor(master$NUTS1_16)
master$NUTS2_16 <- factor(master$NUTS2_16)


excl_nuts2 <- c("ES63","ES64","ES70","FR9","PT20","PT30") 
master <- master[!(master$NUTS2_16 %in% excl_nuts2),]


# LOOP

# i <- which(levels(master$NUTS0_16) == "CY")

i <- which(levels(as.factor(master$NUTS0_16))=="IT")
for (i in c(1:27)) {
  country <- levels(as.factor(master$NUTS0_16))[i]
    cat("\n Country: ",country,"\n")
    m <- master[master$NUTS0_16 == country,]
    m$NUTS0_16 <- factor(m$NUTS0_16)
    m$NUTS1_16 <- factor(m$NUTS1_16)
    m$NUTS2_16 <- factor(m$NUTS2_16)
    m$ELEV2 <- factor(m$ELEV2)
    # m$imperviousness <- ifelse(is.na(m$imperviousness),0,m$imperviousness)

    s <- merge(s2022,m[,c("POINT_ID","point_area","ELEV2")],
               by=c("POINT_ID"))  
    s <- s[!is.na(s$WEIGHTS),]
    #--------------------------------------------------------------

    
    if (nrow(s) > 0 ) {
      # s <- merge(s,pop[,c("NUTS","Pop2018")],by.x=c("NUTS0_16"),by.y=c("NUTS"),all.x=T)
      # s$settl_pc <- (as.numeric(s$settlement)-1) * 1000 / s$Pop2018
      # sum(s$settl_pc*s$initial_weight_area)*pop[pop$NUTS == "AT","Pop2018"] / (sum(area_country$Area)*1000)
      # mean(as.numeric(s$settlement)-1)
      # sum(s$settl_pc*s$initial_weight_area)*pop[pop$NUTS == "AT","Pop2018"] / 1000
      # sum(area_country$Area)
      s$NUTS0_16 <- factor(s$NUTS0_16)
      s$NUTS1_16 <- factor(s$NUTS1_16)
      s$NUTS2_16 <- factor(s$NUTS2_16)
      s$ELEV2 <- factor(s$ELEV2)
      s$STRATO <- as.factor(paste0(s$NUTS0_16,s$STRATUM))
      # s$STRATO <- as.factor(paste0(s$NUTS0_16,s$STR05))
      #s$initial_weight_area <- s$WEIGHTS
      s$ones <- 1
      m$ones <- 1

      ###############################################################################
      sum(s$point_area*s$WEIGHTS)
      areatot <- sum(m$point_area)
      master_areas <- tapply(m$point_area,m$NUTS2_16,FUN=sum)
      cbind(areas[areas$NUTS2_16 %in% names(master_areas),],master_areas)
      
      areatots <- sum(s$point_area*s$WEIGHTS)
      
      # area_country <- areas[substr(as.character(areas$NUTS2_16),1,2) == country,]
      # sum(area_country$Area)
      # sum(s$WEIGHTS)
      # sum(s$WEIGHTS*s$point_area)
      # CALIBRATION
      # design
      des <- e.svydesign(data=s, ids= ~ POINT_ID, strata= ~ STRATO, weights= ~ WEIGHTS, self.rep.str= NULL, check.data= TRUE)
      ls <- find.lon.strata(des)
      if (!is.null(ls)) des <- collapse.strata(des)
      
      # known totals
      s$NUTS2_16 <- factor(s$NUTS2_16)
      # table(s$NUTS2_16)
      # table(s$ELEV2)
    
      if (length(levels(s$NUTS2_16)) > 1) {
        levels(des$variables$ELEV2) <- levels(m$ELEV2)
        levels(des$variables$NUTS2_16) <- levels(m$NUTS2_16)
        poptemp <- pop.template(data=des,
                                calmodel=   ~ 
                                  point_area:ELEV2  - 1,
                                 partition= FALSE)
        popfill <- fill.template(universe=m, template= poptemp)
        ht <- aux.estimates(design=des, calmodel = ~ point_area:ELEV2 - 1)
        ht[, names(ht) %in% names(des)] <- popfill[, names(popfill) %in% names(ht)]
        bb <- bounds.hint(des,ht)
      
      # calibration
        cal <- e.calibrate(design=des, 
                         df.population= popfill, 
                         calmodel=   ~ point_area:ELEV2 - 1,
                         calfun= "linear",
                         bounds = bb
                         ) 
      }
      if (length(levels(s$NUTS2_16)) == 1) {
        poptemp <- pop.template(data=des,
                                calmodel= ~ point_area:ELEV2 - 1, 
                                partition= FALSE)
        popfill <- fill.template(universe=m, template= poptemp)
        ht <- aux.estimates(design=des, calmodel = ~ ones:ELEV2 - 1)
        ht[, names(ht) %in% names(des)] <- popfill[, names(popfill) %in% names(ht)]
        bb <- bounds.hint(des,ht)
        
        # calibration
        cal <- e.calibrate(design=des, 
                           df.population= popfill, 
                           calmodel= ~ point_area:ELEV2 - 1, 
                           calfun= "linear", 
                           bounds =   c(0.1,3)) 
      }
      check.cal(cal)

      summary(s$WEIGHTS)
      sum(s$WEIGHTS)
      sum(m$ones)
      sum(m$point_area)
      cal$prob <- cal$prob/cal$variables$point_area
      sum(weights(cal))
      summary(weights(cal))
      # cal$prob <- cal$prob/cal$variables$point_area
      sum(weights(cal))
      df <- NULL
      df$POINT_ID <- cal$variables$POINT_ID
      df$cal_wgt <- weights(cal)
      df <- as.data.frame(df)
      filename <- paste(country,"_calibrated_wgts_2022.txt",sep="")
      write.table(df,file = file.path(direnew2, filename),sep="\t",quote=FALSE,row.names=FALSE,dec=".")
      
      sum(weights(cal))

      est_LC1_LU1 <- svystatTM(cal, ~ 
                                 SURVEY_LC1_1+
                                 SURVEY_LU1_1+
                                 SURVEY_LC1_2+
                                 SURVEY_LU1_2+
                                 SURVEY_LC1_3+
                                 SURVEY_LU1_3,
                                 # art_imp+
                                 # art_clc+
                                 # settlement+
                                 # settl_pc+
                                 # fao_class_name+
                                 # lue+
                                 # lud,
                               estimator="Total",
                               vartype=c("se","cv"),
                               conf.int= TRUE, 
                               conf.lev= 0.95)
      sum(est_LC1_LU1$Total[1:8])

      est_LC1_LU1_NUTS1_16 <- svystatTM(cal, ~ 
                                          SURVEY_LC1_1+
                                          SURVEY_LU1_1+
                                          SURVEY_LC1_2+
                                          SURVEY_LU1_2+
                                          SURVEY_LC1_3+
                                          SURVEY_LU1_3,
                                          # art_imp+
                                          # art_clc+
                                          # settlement+
                                          # fao_class_name+
                                          # lue+
                                          # lud,
                                        by = ~ NUTS1_16,
                                        estimator="Total",
                                        vartype=c("se","cv"),
                                        conf.int= TRUE, 
                                        conf.lev= 0.95)      
    
      est_LC1_LU1_NUTS2_16 <- svystatTM(cal, ~ 
                                          SURVEY_LC1_1+
                                          SURVEY_LU1_1+
                                          SURVEY_LC1_2+
                                          SURVEY_LU1_2+
                                          SURVEY_LC1_3+
                                          SURVEY_LU1_3,
                                          # art_imp+
                                          # art_clc+
                                          # settlement+
                                          # fao_class_name+
                                          # lue+
                                          # lud,
                                        by = ~ NUTS2_16,
                                        estimator="Total",
                                        vartype=c("se","cv"),
                                        conf.int= TRUE, 
                                        conf.lev= 0.95)

      filename <- paste(country,'_est_LC1_LU1_2022.csv',sep="")
      write.svystat(est_LC1_LU1,file = file.path(direnew1, filename),sep=",",dec=".")
      
      # Transposed ----------------------------------------------
      est_LC1_LU1_NUTS1_16_t <- as.data.frame(t(est_LC1_LU1_NUTS1_16))
      filename <- paste(country,"_est_LC1_LU1_NUTS1_16_2022_t.csv",sep="")
      est_LC1_LU1_NUTS1_16_t$variable <- row.names(est_LC1_LU1_NUTS1_16_t)
      est_LC1_LU1_NUTS1_16_t$variable[1] <- "variable"
      write.table(est_LC1_LU1_NUTS1_16_t,file = file.path(direnew1, filename),sep=",",dec=".",
                  row.names=FALSE,col.names=FALSE,quote=FALSE)
      #-----------------------------------------------------------
      est_LC1_LU1_NUTS2_16_t <- as.data.frame(t(est_LC1_LU1_NUTS2_16))
      filename <- paste(country,"_est_LC1_LU1_NUTS2_16_2022_t.csv",sep="")
      est_LC1_LU1_NUTS2_16_t$variable <- row.names(est_LC1_LU1_NUTS2_16_t)
      est_LC1_LU1_NUTS2_16_t$variable[1] <- "variable"
      write.table(est_LC1_LU1_NUTS2_16_t,file = file.path(direnew1, filename),sep=",",dec=".",
                  row.names=FALSE,col.names=FALSE,quote=FALSE)
      
    }
}



