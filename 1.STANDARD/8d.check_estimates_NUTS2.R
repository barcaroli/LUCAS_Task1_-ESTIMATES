##############################################################################
# LUCAS - Check of estimates variations for Land Cover A and Settlement- NUTS2
##############################################################################

setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc")
dire <- "C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc"
m <- read.csv2("master2019_with_weights_and_strata_2018.csv")
countries <- levels(m$NUTS0_16)
# countries <- countries[-1]
setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc/anomalies")
sink("check_NUTS2.txt")
setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc")
which(countries=="CZ")
for (k in (1:length(countries))) {
  cat("\n---------------------------------------------\n")
  cat("\n *** Country: ",countries[k]," *** \n")
    if (!countries[k] %in% c("BG","HR","CY","MT","RO")) {
      filename <- paste(dire,"/estimates2009/",countries[k],"_est_LC1_LU1_NUTS2_16_2009_t.csv",sep="")
      s2009 <- read.csv(filename,dec='.')
      filename <- paste(dire,"/estimates2012/",countries[k],"_est_LC1_LU1_NUTS2_16_2012_t.csv",sep="")
      s2012 <- read.csv(filename,dec='.')
      filename <- paste(dire,"/estimates2015/",countries[k],"_est_LC1_LU1_NUTS2_16_2015_t.csv",sep="")
      s2015 <- read.csv(filename,dec='.')
      filename <- paste(dire,"/estimates2018/",countries[k],"_est_LC1_LU1_NUTS2_16_2018_t.csv",sep="")
      s2018 <- read.csv(filename,dec='.')
      for (i in (1:(ncol(s2015)-1))) {
        LC_A_2009 <- s2009[s2009$variable == "Total.SURVEY_LC1_1A",i]
        LC_A_2009_CI_l <- s2009[s2009$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
        LC_A_2009_CI_u <- s2009[s2009$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
        Settlement2009 <- s2009[s2009$variable == "Total.settlement1",i]
        Settlement2009_CI_l <- s2009[s2009$variable == "CI.l(95%).Total.settlement1",i]
        Settlement2009_CI_u <- s2009[s2009$variable == "CI.u(95%).Total.settlement1",i]
        LC_A_2012 <- s2012[s2012$variable == "Total.SURVEY_LC1_1A",i]
        LC_A_2012_CI_l <- s2012[s2012$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
        LC_A_2012_CI_u <- s2012[s2012$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
        Settlement2012 <- s2012[s2012$variable == "Total.settlement1",i]
        Settlement2012_CI_l <- s2012[s2012$variable == "CI.l(95%).Total.settlement1",i]
        Settlement2012_CI_u <- s2012[s2012$variable == "CI.u(95%).Total.settlement1",i]
        LC_A_2015 <- s2015[s2015$variable == "Total.SURVEY_LC1_1A",i]
        LC_A_2015_CI_l <- s2015[s2015$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
        LC_A_2015_CI_u <- s2015[s2015$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
        Settlement2015 <- s2015[s2015$variable == "Total.settlement1",i]
        Settlement2015_CI_l <- s2015[s2015$variable == "CI.l(95%).Total.settlement1",i]
        Settlement2015_CI_u <- s2015[s2015$variable == "CI.u(95%).Total.settlement1",i]
        LC_A_2018 <- s2018[s2018$variable == "Total.SURVEY_LC1_1A",i]
        LC_A_2018_CI_l <- s2018[s2018$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
        LC_A_2018_CI_u <- s2018[s2018$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
        Settlement2018 <- s2018[s2018$variable == "Total.settlement1",i]
        Settlement2018_CI_l <- s2018[s2018$variable == "CI.l(95%).Total.settlement1",i]
        Settlement2018_CI_u <- s2018[s2018$variable == "CI.u(95%).Total.settlement1",i]
        if (ncol(s2009) >= i & ncol(s2012) >= i) {
          if (colnames(s2009)[i] == colnames(s2012)[i]) {
            if (LC_A_2009 > LC_A_2012_CI_u) {
              cat("\n NUTS2 :", colnames(s2009)[i])
              cat("\n LC1_1A 2009: ",LC_A_2009," C.I.(95%):",LC_A_2009_CI_l,"",LC_A_2009_CI_u)
              cat("\n LC1_1A 2012: ",LC_A_2012," C.I.(95%):",LC_A_2012_CI_l,"",LC_A_2012_CI_u)
              if (LC_A_2009 > LC_A_2012_CI_u) cat("\n *** Value 2009 exceeds limits C.I. 2012 *** ")
              cat("\n")
            }
            if (Settlement2009 > Settlement2012_CI_u) {
              cat("\n NUTS2 :", colnames(s2009)[i])
              cat("\n Settlement 2009: ",Settlement2009," C.I.(95%):",Settlement2009_CI_l,"",Settlement2009_CI_u)
              cat("\n Settlement 2012: ",Settlement2012," C.I.(95%):",Settlement2012_CI_l,"",Settlement2012_CI_u)
              if (Settlement2009 > Settlement2012_CI_u) cat("\n *** Value 2009 exceeds limits C.I. 2012 *** ")
              cat("\n")
            }
          }
        }  
        if (ncol(s2012) >= i & ncol(s2015) >= i) {
          if (colnames(s2012)[i] == colnames(s2015)[i]) {
            if (LC_A_2012 > LC_A_2015_CI_u) {
              cat("\n NUTS2 :", colnames(s2012)[i])
              cat("\n LC1_1A 2012: ",LC_A_2012," C.I.(95%):",LC_A_2012_CI_l,"",LC_A_2012_CI_u)
              cat("\n LC1_1A 2015: ",LC_A_2015," C.I.(95%):",LC_A_2015_CI_l,"",LC_A_2015_CI_u)
              if (LC_A_2012 > LC_A_2015_CI_u) cat("\n *** Value 2012 exceeds limits C.I. 2015 *** ")
              cat("\n")
            }
            if (Settlement2012 > Settlement2015_CI_u) {
              cat("\n NUTS2 :", colnames(s2012)[i])
              cat("\n Settlement 2012: ",Settlement2012," C.I.(95%):",Settlement2012_CI_l,"",Settlement2012_CI_u)
              cat("\n Settlement 2015: ",Settlement2015," C.I.(95%):",Settlement2015_CI_l,"",Settlement2015_CI_u)
              if (Settlement2012 > Settlement2015_CI_u) cat("\n *** Value 2012 exceeds limits C.I. 2015 *** ")
              cat("\n")
            }
          }
        }
        if (LC_A_2015 > LC_A_2018_CI_u) {
          cat("\n NUTS2 :", colnames(s2015)[i])
          cat("\n LC1_1A 2015: ",LC_A_2015," C.I.(95%):",LC_A_2015_CI_l,"",LC_A_2015_CI_u)
          cat("\n LC1_1A 2018: ",LC_A_2018," C.I.(95%):",LC_A_2018_CI_l,"",LC_A_2018_CI_u)
          if (LC_A_2015 > LC_A_2018_CI_u) cat("\n *** Value 2015 exceeds limits C.I. 2018 *** ")
          cat("\n")
        }
        if (Settlement2015 > Settlement2018_CI_u) {
          cat("\n NUTS2 :", colnames(s2015)[i])
          cat("\n Settlement 2015: ",Settlement2015," C.I.(95%):",Settlement2015_CI_l,"",Settlement2015_CI_u)
          cat("\n Settlement 2018: ",Settlement2018," C.I.(95%):",Settlement2018_CI_l,"",Settlement2018_CI_u)
          if (Settlement2015 > Settlement2018_CI_u) cat("\n *** Value 2015 exceeds limits C.I. 2018 *** ")
          cat("\n")
        }
      }
    }
  
  if (countries[k] %in% c("BG","CY","MT","RO")) {
    filename <- paste(dire,"/estimates2012/",countries[k],"_est_LC1_LU1_NUTS2_16_2012_t.csv",sep="")
    s2012 <- read.csv(filename,dec='.')
    filename <- paste(dire,"/estimates2015/",countries[k],"_est_LC1_LU1_NUTS2_16_2015_t.csv",sep="")
    s2015 <- read.csv(filename,dec='.')
    filename <- paste(dire,"/estimates2018/",countries[k],"_est_LC1_LU1_NUTS2_16_2018_t.csv",sep="")
    s2018 <- read.csv(filename,dec='.')
    for (i in (1:(ncol(s2015)-1))) {
      LC_A_2012 <- s2012[s2012$variable == "Total.SURVEY_LC1_1A",i]
      LC_A_2012_CI_l <- s2012[s2012$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
      LC_A_2012_CI_u <- s2012[s2012$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
      Settlement2012 <- s2012[s2012$variable == "Total.settlement1",i]
      Settlement2012_CI_l <- s2012[s2012$variable == "CI.l(95%).Total.settlement1",i]
      Settlement2012_CI_u <- s2012[s2012$variable == "CI.u(95%).Total.settlement1",i]
      LC_A_2015 <- s2015[s2015$variable == "Total.SURVEY_LC1_1A",i]
      LC_A_2015_CI_l <- s2015[s2015$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
      LC_A_2015_CI_u <- s2015[s2015$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
      Settlement2015 <- s2015[s2015$variable == "Total.settlement1",i]
      Settlement2015_CI_l <- s2015[s2015$variable == "CI.l(95%).Total.settlement1",i]
      Settlement2015_CI_u <- s2015[s2015$variable == "CI.u(95%).Total.settlement1",i]
      LC_A_2018 <- s2018[s2018$variable == "Total.SURVEY_LC1_1A",i]
      LC_A_2018_CI_l <- s2018[s2018$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
      LC_A_2018_CI_u <- s2018[s2018$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
      Settlement2018 <- s2018[s2018$variable == "Total.settlement1",i]
      Settlement2018_CI_l <- s2018[s2018$variable == "CI.l(95%).Total.settlement1",i]
      Settlement2018_CI_u <- s2018[s2018$variable == "CI.u(95%).Total.settlement1",i]
      if (LC_A_2012 > LC_A_2015_CI_u) {
        cat("\n NUTS2 :", colnames(s2012)[i])
        cat("\n LC1_1A 2012: ",LC_A_2012," C.I.(95%):",LC_A_2012_CI_l,"",LC_A_2012_CI_u)
        cat("\n LC1_1A 2015: ",LC_A_2015," C.I.(95%):",LC_A_2015_CI_l,"",LC_A_2015_CI_u)
        if (LC_A_2012 > LC_A_2015_CI_u) cat("\n *** Value 2012 exceeds limits C.I. 2015 *** ")
        cat("\n")
      }
      if (Settlement2012 > Settlement2015_CI_u) {
        cat("\n NUTS2 :", colnames(s2012)[i])
        cat("\n Settlement 2012: ",Settlement2012," C.I.(95%):",Settlement2012_CI_l,"",Settlement2012_CI_u)
        cat("\n Settlement 2015: ",Settlement2015," C.I.(95%):",Settlement2015_CI_l,"",Settlement2015_CI_u)
        if (Settlement2012 > Settlement2015_CI_u) cat("\n *** Value 2012 exceeds limits C.I. 2015 *** ")
        cat("\n")
      }
      if (LC_A_2015 > LC_A_2018_CI_u) {
        cat("\n NUTS2 :", colnames(s2015)[i])
        cat("\n LC1_1A 2015: ",LC_A_2015," C.I.(95%):",LC_A_2015_CI_l,"",LC_A_2015_CI_u)
        cat("\n LC1_1A 2018: ",LC_A_2018," C.I.(95%):",LC_A_2018_CI_l,"",LC_A_2018_CI_u)
        if (LC_A_2015 > LC_A_2018_CI_u) cat("\n *** Value 2015 exceeds limits C.I. 2018 *** ")
        cat("\n")
      }
      if (Settlement2015 > Settlement2018_CI_u) {
        cat("\n NUTS2 :", colnames(s2015)[i])
        cat("\n Settlement 2015: ",Settlement2015," C.I.(95%):",Settlement2015_CI_l,"",Settlement2015_CI_u)
        cat("\n Settlement 2018: ",Settlement2018," C.I.(95%):",Settlement2018_CI_l,"",Settlement2018_CI_u)
        if (Settlement2015 > Settlement2018_CI_u) cat("\n *** Value 2015 exceeds limits C.I. 2018 *** ")
        cat("\n")
      }
    }
  }

  if (countries[k] == "HR") {
    filename <- paste(dire,"/estimates2015/",countries[k],"_est_LC1_LU1_NUTS2_16_2015_t.csv",sep="")
    s2015 <- read.csv(filename,dec='.')
    filename <- paste(dire,"/estimates2018/",countries[k],"_est_LC1_LU1_NUTS2_16_2018_t.csv",sep="")
    s2018 <- read.csv(filename,dec='.')
    for (i in (1:(ncol(s2015)-1))) {
      LC_A_2015 <- s2015[s2015$variable == "Total.SURVEY_LC1_1A",i]
      LC_A_2015_CI_l <- s2015[s2015$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
      LC_A_2015_CI_u <- s2015[s2015$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
      Settlement2015 <- s2015[s2015$variable == "Total.settlement1",i]
      Settlement2015_CI_l <- s2015[s2015$variable == "CI.l(95%).Total.settlement1",i]
      Settlement2015_CI_u <- s2015[s2015$variable == "CI.u(95%).Total.settlement1",i]
      LC_A_2018 <- s2018[s2018$variable == "Total.SURVEY_LC1_1A",i]
      LC_A_2018_CI_l <- s2018[s2018$variable == "CI.l(95%).Total.SURVEY_LC1_1A",i]
      LC_A_2018_CI_u <- s2018[s2018$variable == "CI.u(95%).Total.SURVEY_LC1_1A",i]
      Settlement2018 <- s2018[s2018$variable == "Total.settlement1",i]
      Settlement2018_CI_l <- s2018[s2018$variable == "CI.l(95%).Total.settlement1",i]
      Settlement2018_CI_u <- s2018[s2018$variable == "CI.u(95%).Total.settlement1",i]
      if (LC_A_2015 > LC_A_2018_CI_u) {
        cat("\n NUTS2 :", colnames(s2015)[i])
        cat("\n LC1_1A 2015: ",LC_A_2015," C.I.(95%):",LC_A_2015_CI_l,"",LC_A_2015_CI_u)
        cat("\n LC1_1A 2018: ",LC_A_2018," C.I.(95%):",LC_A_2018_CI_l,"",LC_A_2018_CI_u)
        if (LC_A_2015 > LC_A_2018_CI_u) cat("\n *** Value 2015 exceeds limits C.I. 2018 *** ")
        cat("\n")
      }
      if (Settlement2015 > Settlement2018_CI_u) {
        cat("\n NUTS2 :", colnames(s2015)[i])
        cat("\n Settlement 2015: ",Settlement2015," C.I.(95%):",Settlement2015_CI_l,"",Settlement2015_CI_u)
        cat("\n Settlement 2018: ",Settlement2018," C.I.(95%):",Settlement2018_CI_l,"",Settlement2018_CI_u)
        if (Settlement2015 > Settlement2018_CI_u) cat("\n *** Value 2015 exceeds limits C.I. 2018 *** ")
        cat("\n")
      }
    }
  }  
   
  cat("\n")
}  
sink()
