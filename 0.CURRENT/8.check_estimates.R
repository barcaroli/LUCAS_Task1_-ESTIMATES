##############################################################
# LUCAS - Check of estimates variations for Land Cover A
##############################################################


m <- read.csv2("master2019_with_weights_and_strata_2018.csv")
countries <- levels(m$NUTS0_16)
# countries <- countries[-1]
setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc/anomalies")
sink("check.txt")
for (i in (1:length(countries))) {
  setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc/allyears_estimates")
  cat("\n Country: ",countries[i],"\n")
  st <- paste("df <- read.csv2('",countries[i],"_est_all.csv',dec='.')",sep="")
  eval(parse(text=st))
  if (ncol(df) == 21) {
    if (df[df$Variable=="SURVEY_LC1_1A",c("Area_2018")] < df[df$Variable=="SURVEY_LC1_1A",c("Area_2015")]  ) {
      cat("\n LC1_1A 2015: ",df[df$Variable=="SURVEY_LC1_1A",c("Area_2015")], df[df$Variable=="SURVEY_LC1_1A",c("CI_lower.2")],df[df$Variable=="SURVEY_LC1_1A",c("CI_upper.2")],"\n")
      cat("\n LC1_1A 2018: ",df[df$Variable=="SURVEY_LC1_1A",c("Area_2018")], df[df$Variable=="SURVEY_LC1_1A",c("CI_lower.3")],df[df$Variable=="SURVEY_LC1_1A",c("CI_upper.3")],"\n")
    }
    if (df[df$Variable=="settlement1",c("Area_2018")] < df[df$Variable=="settlement1",c("Area_2015")] ) {
      cat("\n Settlement 2015: ",df[df$Variable=="settlement1",c("Area_2015")], df[df$Variable=="settlement1",c("CI_lower.2")],df[df$Variable=="settlement1",c("CI_upper.2")],"\n")
      cat("\n Settlement 2018: ",df[df$Variable=="settlement1",c("Area_2018")], df[df$Variable=="settlement1",c("CI_lower.3")],df[df$Variable=="settlement1",c("CI_upper.3")],"\n")
    }   
  }
  if (ncol(df) == 16) {
    if (df[df$Variable=="SURVEY_LC1_1A",c("Area_2018")] < df[df$Variable=="SURVEY_LC1_1A",c("Area_2015")]  ) {
      cat("\n LC1_1A 2015: ",df[df$Variable=="SURVEY_LC1_1A",c("Area_2015")], df[df$Variable=="SURVEY_LC1_1A",c("CI_lower.1")],df[df$Variable=="SURVEY_LC1_1A",c("CI_upper.1")],"\n")
      cat("\n LC1_1A 2018: ",df[df$Variable=="SURVEY_LC1_1A",c("Area_2018")], df[df$Variable=="SURVEY_LC1_1A",c("CI_lower.2")],df[df$Variable=="SURVEY_LC1_1A",c("CI_upper.2")],"\n")
    }
    if (df[df$Variable=="settlement1",c("Area_2018")] < df[df$Variable=="settlement1",c("Area_2015")] ) {
      cat("\n Settlement 2015: ",df[df$Variable=="settlement1",c("Area_2015")], df[df$Variable=="settlement1",c("CI_lower.1")],df[df$Variable=="settlement1",c("CI_upper.1")],"\n")
      cat("\n Settlement 2018: ",df[df$Variable=="settlement1",c("Area_2018")], df[df$Variable=="settlement1",c("CI_lower.2")],df[df$Variable=="settlement1",c("CI_upper.2")],"\n")
    }   
  }
}
sink()
