sum(est$Total[est$Variable=="SURVEY_LC1_1A"])
sum(est$Total[est$Variable=="SURVEY_LC1_2A1"])
sum(est$Total[est$Variable=="SURVEY_LC1_2A2"])
sum(est$Total[est$Variable=="SURVEY_LC1_2A3"])


est <- NULL
for (k in EU26) {
  country <- read.csv(paste0(getwd(),"\\estimates2022\\",k,"_est_LC1_LU1_2022.csv"))
  country <- country[,c(1:3)]
  if (round(sum(country$Total[country$Variable=="SURVEY_LC1_1A"])) != 
      round(sum(country$Total[country$Variable=="SURVEY_LC1_2A1"]) +
      sum(country$Total[country$Variable=="SURVEY_LC1_2A2"]) +
      sum(country$Total[country$Variable=="SURVEY_LC1_2A3"]))) {
    cat("\nCountry: ",k)
    cat("\nA: ",round(sum(country$Total[country$Variable=="SURVEY
                                        _LC1_1A"])))
    cat("\nA1+A2+A3: ",round(sum(country$Total[country$Variable=="SURVEY_LC1_2A1"]) +
                        sum(country$Total[country$Variable=="SURVEY_LC1_2A2"]) +
                        sum(country$Total[country$Variable=="SURVEY_LC1_2A3"])))
  }
  est <- rbind(est,country)
}

s$LC1_A <- ifelse(s$SURVEY_LC1_1 == "A",1,0)
sum(s$LC1_A*s$cal_wgt)

s2$LC1_A <- ifelse(s2$SURVEY_LC1_1 == "A",1,0)
sum(s2$LC1_A*s2$cal_wgt)
s2$LC1_A1 <- ifelse(s2$SURVEY_LC1_2 == "A1",1,0)
sum(s2$LC1_A1*s2$cal_wgt)
s2$LC1_A2 <- ifelse(s2$SURVEY_LC1_2 == "A2",1,0)
sum(s2$LC1_A2*s2$cal_wgt)
s2$LC1_A3 <- ifelse(s2$SURVEY_LC1_2 == "A3",1,0)
sum(s2$LC1_A3*s2$cal_wgt)
sum(s2$LC1_A1*s2$cal_wgt)+sum(s2$LC1_A2*s2$cal_wgt)+sum(s2$LC1_A3*s2$cal_wgt)

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


popfill2 <- pop.template(data=des2,
                         calmodel=   ~
                           LC1_A - 1,
)
popfill2[1,] <- est_LC1_LU1$Total[c(2)]
cal2 <- e.calibrate(design=des2,
                    df.population= popfill2,
                    calmodel=   ~
                      LC1_A - 1,
                    calfun= 'linear', bounds =   c(0.01,100))
check.cal(cal2)
