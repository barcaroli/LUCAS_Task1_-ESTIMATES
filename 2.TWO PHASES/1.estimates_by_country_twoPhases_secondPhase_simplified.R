#----------------------------------------------
# Script to execute second phase estimation
# based on known totals of LC and LU
# obtained from the first phase
#----------------------------------------------
# SIMPLIFIED VERSION: 
# calibration only on LC and LU estimates
# 1 digit from first phase
#----------------------------------------------
# N.B. this script works only if called 
# by 1.estimates_by_country_twoPhases.R
#----------------------------------------------
# library(ReGenesees)
# load("firstPhase.RData")
# Select only Field
s2 <- s[s$SURVEY_OBS_TYPE == 1,]
cat("\nN.obs before selection:",nrow(s2))
# Select only obs without 'x' in LC1 and LU1
s2 <- droplevels(s2)
table(s2$SURVEY_LC1)
table(s2$SURVEY_LU1)
if(length(grep("x",s2$SURVEY_LC1))>0){
  s2 <- s2[-grep("x",s2$SURVEY_LC1),]
}
if(length(grep("x",s2$SURVEY_LU1))>0){
  s2 <- s2[-grep("x",s2$SURVEY_LU1),]
}
if(length(grep("X",s2$SURVEY_LC1))>0){
  s2 <-  s2[-grep("X",s2$SURVEY_LC1),]
}
if(length(grep("X",s2$SURVEY_LU1))>0){
  s2 <-  s2[-grep("X",s2$SURVEY_LU1),]
}

# Select only obs without given LC1 and LU1 values
s2 <- s2[!s2$SURVEY_LC1 %in% c("A00","A10","A20",
                               "B00","B10","B20","B30","B40","B50","B60","B70","B80",
                               "C00","C20",
                               "D00",
                               "E00",
                               "F00",
                               "G00","G10","G20",
                               "H00","H10","H20","H30"),]
s2 <- s2[!s2$SURVEY_LU1 %in% c("8","U100","U110",
                               "U200","U220",
                               "U300","U310","U320","U360",
                               "U400"),]
cat("\nN.obs after selection:",nrow(s2))
s2 <- droplevels(s2)
table(s2$SURVEY_LC1)
table(s2$SURVEY_LU1)
save(s, file=paste0("Samples/", country, "_s_1stphase.RData"))
save(s2, file=paste0("Samples/", country, "_s_2ndphase.RData"))
# design
s2 <- s2[order(s2$STRATUM_LUCAS),]
des2 <- e.svydesign(data=s2, 
                    ids= ~ POINT_ID, 
                    strata= ~ STRATUM_LUCAS, 
                    # weights = ~ WGT_LUCAS,
                    weights = ~ cal_wgt,
                    self.rep.str= NULL, 
                    fpc= ~fpc, 
                    check.data= TRUE)
ls <- find.lon.strata(des2)
print(ls)
if (!is.null(ls)) {
  des2 <-tryCatch({
    des2 <- collapse.strata(des2, block.vars = ~NUTS2_24)
  },
  error = function(e) {
    cat("Error:", e$message, "\n")  
    des2 <- collapse.strata(des2)  
    return(des2)  
  })
}



#-----------------------------
# Prepare calibration
#-----------------------------
# Add LC1 and LU1 known totals to the current known totals vector
popfill2 <- pop.template(data=des2,
                         calmodel=   ~
                           SURVEY_LC1_1 +
                           SURVEY_LU1_1 - 1,
)
popfill2[1,] <- est_LC1_LU1$Total[c(2:9,11:13)]
cal2 <- e.calibrate(design=des2,
                    df.population= popfill2,
                    calmodel=   ~
                      SURVEY_LC1_1 +
                      SURVEY_LU1_1 - 1,
                    calfun= 'linear', bounds =   c(0.05,Inf))
check.cal(cal2)
#print(a)
#UWE(cal2)
# summary(s$WGT_LUCAS)
# sum(s$WGT_LUCAS)
# sum(m$ones)
sum(m$point_area)
# cal2$prob <- cal2$prob/cal2$variables$point_area
sum(weights(cal2))
summary(weights(cal2))

s2$cal_wgt <- weights(cal2)

# design
des3 <- e.svydesign(data=s2, 
                    ids= ~ POINT_ID, 
                    strata= ~ STRATUM_LUCAS, 
                    weights = ~ cal_wgt, 
                    self.rep.str= NULL, 
                    fpc= ~fpc, 
                    check.data= TRUE)
ls <- find.lon.strata(des3)
#if (!is.null(ls)) des3 <- collapse.strata(des3)
if (!is.null(ls)) {
  des3 <-tryCatch({
    des3 <- collapse.strata(des3, block.vars = ~NUTS2_24)
  },
  error = function(e) {
    cat("Error:", e$message, "\n")  
    des3 <- collapse.strata(des3)  
    return(des3)  
  })
}

est_LC1_LU1_2nd <- svystatTM(des3, ~ 
                             SURVEY_LC1_2+
                             SURVEY_LU1_2+
                             SURVEY_LC1_3+
                             SURVEY_LU1_3,
                             estimator="Total",
                             vartype=c("se","cv"),
                             conf.int= TRUE, 
                             conf.lev= 0.95)


# by nuts1
est_LC1_LU1_NUTS1_24_2nd <- svystatTM(des3, ~ 
                                        SURVEY_LC1_2+
                                        SURVEY_LU1_2+
                                        SURVEY_LC1_3+
                                        SURVEY_LU1_3,
                                      by = ~ NUTS1_24,
                                      estimator="Total",
                                      vartype=c("se","cv"),
                                      conf.int= TRUE,
                                      conf.lev= 0.95)

est_LC1_LU1_NUTS2_24_2nd <- svystatTM(des3, ~ 
                                        SURVEY_LC1_2+
                                        SURVEY_LU1_2+
                                        SURVEY_LC1_3+
                                        SURVEY_LU1_3,
                                      by = ~ NUTS2_24,
                                      estimator="Total",
                                      vartype=c("se","cv"),
                                      conf.int= TRUE, 
                                      conf.lev= 0.95)


# Transposed ----------------------------------------------
if (length(levels(s$NUTS1_24)) > 1){
  est_LC1_LU1_NUTS1_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS1_24_2nd))[-1,]
  est_LC1_LU1_NUTS1_24_2nd_t$variable <- row.names(est_LC1_LU1_NUTS1_24_2nd_t)
  #est_LC1_LU1_NUTS1_24_2nd_t$variable[1] <- "variable"
}
#-----------------------------------------------------------
if (length(levels(s$NUTS2_24)) > 1){
  est_LC1_LU1_NUTS2_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24_2nd))[-1,]
  est_LC1_LU1_NUTS2_24_2nd_t$variable <- row.names(est_LC1_LU1_NUTS2_24_2nd_t)
  #est_LC1_LU1_NUTS2_24_2nd_t$variable[1] <- "variable"
}

