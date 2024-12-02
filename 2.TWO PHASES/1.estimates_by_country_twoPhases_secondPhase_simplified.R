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
# design
s2 <- s2[order(s2$NUTS2_24,s2$STRATUM_LUCAS),]
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
  tryCatch({
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
print(a)
UWE(cal2)
summary(s$WGT_LUCAS)
sum(s$WGT_LUCAS)
sum(m$ones)
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
if (!is.null(ls)) des3 <- collapse.strata(des3)

est_LC1_LU1_2nd <- svystatTM(des3, ~ 
                           SURVEY_LC1_2+
                           SURVEY_LU1_2+
                           SURVEY_LC1_3+
                           SURVEY_LU1_3                         ,
                         estimator="Total",
                         vartype=c("se","cv"),
                         conf.int= TRUE, 
                         conf.lev= 0.95)
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
est_LC1_LU1_NUTS1_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS1_24_2nd))[-1,]
est_LC1_LU1_NUTS1_24_2nd_t$variable <- row.names(est_LC1_LU1_NUTS1_24_2nd_t)
est_LC1_LU1_NUTS1_24_2nd_t$variable[1] <- "variable"
#-----------------------------------------------------------
est_LC1_LU1_NUTS2_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24_2nd))[-1,]
est_LC1_LU1_NUTS2_24_2nd_t$variable <- row.names(est_LC1_LU1_NUTS2_24_2nd_t)
est_LC1_LU1_NUTS2_24_2nd_t$variable[1] <- "variable"

