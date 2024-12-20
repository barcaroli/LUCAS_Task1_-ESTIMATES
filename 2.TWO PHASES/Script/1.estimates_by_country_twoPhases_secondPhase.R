#----------------------------------------------
# Script to execute second phase estimation
# based on known totals of LC and LU
# obtained from the first phase
#----------------------------------------------
# N.B. this script works only if called 
# by 1.estimates_by_country_twoPhases.R
#----------------------------------------------

# Select only Field
s2 <- s[s$SURVEY_OBS_TYPE == 1,]
# design
des2 <- e.svydesign(data=s2, 
                    ids= ~ POINT_ID, 
                    strata= ~ STRATUM_LUCAS, 
                    weights = ~ WGT_LUCAS, 
                    self.rep.str= NULL, 
                    fpc= ~fpc, 
                    check.data= TRUE)
ls <- find.lon.strata(des2)
if (!is.null(ls)) des2 <- collapse.strata(des2)

#-----------------------------
# Check variables for calmodel
#-----------------------------
m <- droplevels(m)
s2 <- droplevels(s2)
calib_vars <- NULL
a <- table(m$ELEV2)
b <- table(s2$ELEV2)
if (length(a[a>0]) == length(b[b>0]) & length(b[b>0]) > 1) calib_vars <- "ELEV2"
a <- table(m$BCK21_R)
b <- table(s2$BCK21_R)
if (length(a[a>0]) == length(b[b>0]) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"BCK21_R")
a <- table(m$GRA18_10)
b <- table(s2$GRA18_10)
if (length(a[a>0] == length(b[b>0])) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"GRA18_10")
a <- table(m$FTY18_10)
b <- table(s2$FTY18_10)
if (length(a[a>0] == length(b[b>0])) & length(b[b>0]) > 1) calib_vars <- c(calib_vars,"FTY18_10")
print(calib_vars)
#-----------------------------
# Prepare calibration
#-----------------------------
# Add LC1 and LU1 known totals to the current known totals vector
popfill3 <- pop.template(data=des2,
                         calmodel=   ~
                           SURVEY_LC1_1 +
                           SURVEY_LU1_1 - 1,
)
popfill3[1,] <- est_LC1_LU1$Total[c(2:9,11:13)]

if (length(calib_vars) > 0) {
  stringa <- paste0(
    "poptemp2 <- pop.template(data=des2, calmodel = ~ point_area + point_area:(",
    paste(calib_vars, collapse = " + "),
    ") - 1)"
  )
  print(stringa)
  eval(parse(text=stringa))
  popfill2 <- fill.template(universe=m, template= poptemp2)
  popfill2[1] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==paesi[i]],na.rm=TRUE)
  popfill4 <- c(popfill2[1,],popfill3[1,])
  # popfill4 <- as.data.frame(popfill4)
  # colnames(popfill4) <- c(colnames(popfill2),colnames(popfill3))
  # popfill4 <- popfill4[,!colnames(popfill4) == "point_area:ELEV22"]
  popfill4 <- as.data.frame(c(popfill2[1,1],popfill3[1,],popfill2[1,2:8]))
  names(popfill4) <- c(names(popfill2)[1],names(popfill3),names(popfill2[2:8]))
  popfill4 <- popfill4[1,!colnames(popfill4) %in% c("point_area:ELEV22","SURVEY_LC1_1U1")]
  # Now, calibration!
  stringa <- paste0(
    "cal2 <- e.calibrate(design=des2,df.population= popfill4,calmodel = ~ point_area + point_area:(",
    paste(calib_vars, collapse = " + "),
    ") + SURVEY_LC1_1 + SURVEY_LU1_1 - 1, partition=FALSE, calfun= 'linear', bounds =   c(-Inf,Inf))"
  )
  print(stringa)
  eval(parse(text=stringa))
}
if (length(calib_vars) == 0) {
  cal2 <- e.calibrate(design=des2,
                     df.population= popfill3,
                     calmodel=   ~
                       SURVEY_LC1_1 +
                       SURVEY_LU1_1 - 1,
                     calfun= 'linear', bounds =   c(-Inf,Inf))
}


check.cal(cal2)
UWE(cal2)
summary(s$WGT_LUCAS)
sum(s$WGT_LUCAS)
sum(m$ones)
sum(m$point_area)
cal$prob <- cal2$prob/cal2$variables$point_area
sum(weights(cal2))
summary(weights(cal2))

s$cal_wgt <- weights(cal2)

# design
des3 <- e.svydesign(data=s, 
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
est_LC1_LU1_NUTS1_24_2nd <- svystatTM(des2, ~ 
                                    SURVEY_LC1_2+
                                    SURVEY_LU1_2+
                                    SURVEY_LC1_3+
                                    SURVEY_LU1_3,
                                  by = ~ NUTS1_24,
                                  estimator="Total",
                                  vartype=c("se","cv"),
                                  conf.int= TRUE,
                                  conf.lev= 0.95)

est_LC1_LU1_NUTS2_24_2nd <- svystatTM(des2, ~ 
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
#-----------------------------------------------------------
est_LC1_LU1_NUTS2_24_2nd_t <- as.data.frame(t(est_LC1_LU1_NUTS2_24_2nd))[-1,]

