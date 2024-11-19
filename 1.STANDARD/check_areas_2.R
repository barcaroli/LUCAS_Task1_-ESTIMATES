options(scipen=100)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD")
load("countries.Rdata")
path_data <- "D:/Google Drive/LUCAS 2025/2.DATA/"
areas <- read.csv(paste0(path_data,"areas_2015_2024.csv"),colClasses = c(rep('character',4), rep('numeric',10)))
aree <- as.data.frame(list(country=countries,
                           area_est=rep(NA,27),
                           known_tot=rep(NA,27)))
for (i in c(1:length(countries))) {
  a <- read.csv(paste0(".\\estimates2022\\",countries[i],"_est_LC1_LU1_2022.csv"))
  aree$area_est[i] <- a$Total[1]
  aree$known_tot[i] <- sum(areas$area2024[substr(areas$NUTS2,1,2)==countries[i]],na.rm=TRUE)
  aree$diff[i] <- round((aree$known_tot[i] - aree$area_est[i]),2)
}
tots <- colSums(aree[,c(2:ncol(aree))])
aree <- rbind(aree,c("Total",as.numeric(tots)))

write.table(aree,"check_areas_2.csv",sep=",",quote=F,row.names = F)
