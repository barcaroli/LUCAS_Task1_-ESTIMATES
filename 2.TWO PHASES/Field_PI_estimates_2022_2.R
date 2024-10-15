#----------------------------------------------
# Script to analyze the impact of a two-phase
# estimation procedure on land cover two digits
# estimates
#----------------------------------------------
# setwd("D:/Google Drive/LUCAS 2024/3.Field_PI_share")
setwd("C:/Users/UTENTE/Google Drive/LUCAS 2024/3.Field_PI_share")
options(scipen=999)
library(data.table)

library(ReGenesees)

# dati <- fread("D:\\Google Drive\\LUCAS 2024\\data\\EU_LUCAS_2022.csv")
# s <- fread("D:\\Google Drive\\LUCAS 2024\\data\\sample_LUCAS_2022.csv")
dati <- fread("C:/Users/UTENTE/Google Drive\\LUCAS 2024\\data\\EU_LUCAS_2022.csv")
s <- fread("C:/Users/UTENTE/Google Drive\\LUCAS 2024\\data\\sample_LUCAS_2022.csv")

s2022 <- merge(s,dati[,c("POINT_ID","SURVEY_LC1","SURVEY_OBS_TYPE")],by.x="ID",by.y="POINT_ID")
table(s2022$SURVEY_LC1)
s2022$LC1 <- as.factor(substr(s2022$SURVEY_LC1,1,1))
table(s2022$LC1)
s2022$LC2 <- as.factor(substr(s2022$SURVEY_LC1,1,2))
table(s2022$LC2)

s2022 <- s2022[!is.na(s2022$LC1) & s2022$LC1 != "8",]
s2022$STRATUM <- as.factor(s2022$STRATUM)

s2022 <- s2022[order(s2022$NUTS2_16,s2022$STRATUM,s2022$LC2),]

table(s2022$SURVEY_OBS_TYPE)
des <- e.svydesign(data=s2022, ids= ~ ID, strata= ~ STRATUM, weights= ~ WEIGHTS, self.rep.str= NULL, check.data= TRUE)
ls <- find.lon.strata(des)
if (!is.null(ls)) des <- collapse.strata(des)


# First phase estimation
phase1LC1 <- svystatTM(des, y = ~LC1, estimator="Mean", conf.int=TRUE)
round(phase1LC1,3)
phase1LC2 <- svystatTM(des, y = ~LC2, estimator="Mean", conf.int=TRUE)
round(phase1LC2,3)
totals <- svystatTM(des, y = ~LC1, estimator="Total")
totals

# Second phase estimation
# subset: only Field
s2 <- s2022[s2022$SURVEY_OBS_TYPE==1,]
des2 <- e.svydesign(data=s2, ids= ~ ID, strata= ~ STRATUM, weights= ~ WEIGHTS, self.rep.str= NULL, check.data= TRUE)
ls <- find.lon.strata(des2)
if (!is.null(ls)) des2 <- collapse.strata(des2)
# calibration
poptot <- pop.template(data=des2, calmodel= ~ LC1 - 1, partition= FALSE)
poptot[1,] <- totals$Total
cal <- e.calibrate(design=des2, 
                   df.population= poptot, 
                   calmodel= ~ LC1 - 1, 
                   calfun= "linear") 
check.cal(cal)

# Estimation
phase2LC1 <- svystatTM(cal, y = ~LC1, estimator="Mean", conf.int=TRUE)
round(phase2LC1,3)
phase2LC1$LC2 <- row.names(phase2LC1)
phase2LC2 <- svystatTM(cal, y = ~LC2, estimator="Mean", conf.int=TRUE)
round(phase2LC2,3)
phase1LC2$LC2 <- row.names(phase1LC2)
colnames(phase1LC2)[1] <- "Phase1"
phase2LC2$LC2 <- row.names(phase2LC2)
colnames(phase2LC2)[1] <- "Phase2"
LC2 <- merge(phase1LC2[,c(1,5)],phase2LC2)
LC2$Diff <- (LC2$Phase1 - LC2$Phase2) / LC2$Phase1
LC2$FieldObs <- nrow(s2)
LC2$Iteration <- 0

# Loop: increasing field observations
LC2 <- NULL
set.seed(1234)
ind <- c(1:nrow(s2))
for (k in c(20:2)) {
  set.seed(1234)
  step <- k - 1
  indices <- seq.int(from=1, to=length(ind), by=step)
  s3 <- s2[indices,]
  cat(nrow(s3))
  table(s3$LC2)
  totals
  cat("\n Iteration ",k,"  Number of field observations: ", nrow(s3))
  des2 <- e.svydesign(data=s3, ids= ~ ID, strata= ~ STRATUM, weights= ~ WEIGHTS, self.rep.str= NULL, check.data= TRUE)
  ls <- find.lon.strata(des2)
  if (!is.null(ls)) des2 <- collapse.strata(des2)
  # calibration
  tots <- svystatTM(des2, y = ~LC1, estimator="Total", conf.int = TRUE)
  totals$LC1 <- row.names(totals)
  tots$LC1 <- row.names(tots)
  totals2 <- totals[totals$LC1 %in% tots$LC1,]
  poptot <- pop.template(data=des2, calmodel= ~ LC1 - 1, partition= FALSE)
  poptot[1,] <- totals2$Total
  cal <- e.calibrate(design=des2, 
                     df.population= poptot, 
                     calmodel= ~ LC1 - 1, 
                     calfun= "linear") 
  check.cal(cal)
  a <- as.data.frame(cal$variables)
  a$wgt_final <- weights(cal)
  tapply(a$wgt_final,a$LC1,FUN=sum)
  totals
  # Estimation
  phase2LC2 <- svystatTM(cal, y = ~LC2, estimator="Mean", conf.int = TRUE)
  phase2LC2
  phase2LC2$LC2 <- row.names(phase2LC2)
  colnames(phase1LC2)[1] <- "Phase1"
  colnames(phase2LC2)[1] <- "Phase2"
  phase1LC2$LC2 <- row.names(phase1LC2)
  LC2iter <- merge(phase1LC2[,c(1,5)],phase2LC2)
  LC2iter$Diff <- (LC2iter$Phase1 - LC2iter$Phase2) / LC2iter$Phase1
  LC2iter$FieldObs <- nrow(s2)
  LC2iter$Iteration <- k
  colnames(LC2iter)[5:6] <- c("CI.l","CI.u")
  LC2 <- rbind(LC2,LC2iter)
}
# colnames(LC2)[5:6] <- c("CI.l","CI.u")
source("plotCI.R")
par(mfrow=c(2,2))
old_LC2 <- ""
for (k in unique(LC2$LC2)[order(unique(LC2$LC2))]) {
  if (substr(old_LC2,4,4) != substr(k,4,4)) {
    old_LC2 <- k
    par(mfrow=c(1,1))
    par(mfrow=c(2,2))
  }
  ymin <- min(LC2$CI.l[LC2$LC2==k],phase1LC2$Phase1[phase1LC2$LC2==k])
  ymax <- max(LC2$CI.u[LC2$LC2==k],phase1LC2$Phase1[phase1LC2$LC2==k])
  # if (ymin < 0) ymin <- ymin * 1.2
  # if (ymin > 0) ymin <- ymin * 0.8
  # if (ymax < 0) ymax <- ymax * 0.8
  # if (ymax > 0) ymax <- ymax * 1.2
  df=LC2[LC2$LC2==k,]
  plotCI(df=df,
         y=colnames(df)[3],
         ci_l=colnames(df)[5],
         ci_u=colnames(df)[6],
         variable="Iteration",
         domain=k,
         ylim=c(ymin,ymax),
         specs=NULL)
  abline(a=phase1LC2$Phase1[phase1LC2$LC2==k],b=0,col="red")
  # lines(lowess(LC2$Phase2[LC2$LC2==k], f = 1), col = "darkgreen")
}
par(mfrow=c(1,1))
write.table(LC2,"LC2b.csv",sep=",",quote=F,row.names=F)
# save.image(file="runItaly.RData")

# # # Render an html of this script
# library(rmarkdown)
# options("knitr.duplicate.label" = "allow")
# rmarkdown::render("Field_PI_estimates_2022.R",
#                   # output_format=html_document(df_print="paged", theme="flatly", highlight="haddock", 
#                   output_format=word_document(df_print="paged", 
#                   fig_width = 8, fig_height = 6))

