setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD")
load("countries.Rdata")
path_data <- "D:/Google Drive/LUCAS 2025/2.DATA/"
areas <- read.csv(paste0(path_data,"areas_2015_2024.csv"),colClasses = c(rep('character',4), rep('numeric',10)))
aree <- as.data.frame(list(country=countries,
                           area_a=rep(NA,27),
                           area_b=rep(NA,27),
                           area_c=rep(NA,27),
                           known_tot=rep(NA,27)))
for (i in c(1:length(countries))) {
  a <- read.csv(paste0(".\\estimates2022a\\",countries[i],"_est_LC1_LU1_2022.csv"))
  b <- read.csv(paste0(".\\estimates2022b\\",countries[i],"_est_LC1_LU1_2022.csv"))
  c <- read.csv(paste0(".\\estimates2022c\\",countries[i],"_est_LC1_LU1_2022.csv"))
  aree$area_a[i] <- a$Total[i]
  aree$area_b[i] <- b$Total[i]
  aree$area_c[i] <- c$Total[i]
  aree$known_tot <- sum(areas$area2024[substr(areas$NUTS2,1,2)==countries[i]],na.rm=TRUE)
}




a <- read.csv(".\\estimates2022a\\AT_est_LC1_LU1_2022.csv")
b <- read.csv(".\\estimates2022b\\AT_est_LC1_LU1_2022.csv")
c <- read.csv(".\\estimates2022c\\AT_est_LC1_LU1_2022.csv")
aree$area_a[1] <- a$Total[1]
aree$area_b[1] <- b$Total[1]
aree$area_c[1] <- c$Total[1]
aree$known_tot <- sum(areas$area2024[substr(areas$NUTS2,1,2)=="AT"],na.rm=TRUE)


length(unique(substr(areas$NUTS2,1,2)))
27
sum(areas$area2024,na.rm=TRUE)
# [1] 4125085

sum(areas$area2021,na.rm=TRUE)
# [1] 4125114
# Dalle stime
4125114-4116276.95
# [1] 8837.05

path_input <- "D:\\Google Drive\\LUCAS 2025\\Task 1 - ESTIMATES\\1.STANDARD\\"
for (i in c())

sum(areas$area2024[substr(areas$NUTS2,1,2)=="IT"],na.rm=TRUE)
# [1] 302073
302079-301654.7367
# [1] 424.2633

sum(popfill2[substr(colnames(popfill2),1,4)=="NUTS"])

# Totale 27 paesi
4125131.995-4125085
# [1] 46.995