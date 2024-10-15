plotCI <- function(df,
                   y,
                   ci_l,
                   ci_u,
                   variable,
                   domain=NULL,
                   ylim=NULL,
                   specs=NULL) {
  eval(parse(text=paste0("y_vals <- df$",y)))
  eval(parse(text=paste0("ci_l <- df$",ci_l)))
  eval(parse(text=paste0("ci_u <- df$",ci_u)))
  plot(y_vals,
       ylim = c(min(ci_l),max(ci_u)),
       type="b",
       ylab = "Estimate",
       xlab = variable,
       xaxt = 'n',
       col="blue")
  title(paste0(y," by ",variable," - ",domain," ",specs),cex.main = 1,font.main= 1,col.main="blue")
  axis(side = 1, at = c(1:nrow(df)), 
       labels = c("2009","2012","2015","2018","2022"), las=1, cex.axis=0.7)
  x_points <- 1:nrow(df)
  polygon(
    x = c(x_points,rev(x_points)),
    y = c(ci_l[1:nrow(df)],rev(ci_u[1:nrow(df)])), 
    col = rgb(0, 0, 1, 0.2), border = NA)
}


library(xlsx)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/0.CURRENT/EU_estimates")
eu <- read.xlsx("Europe_LC_LU_1digit.xlsx",sheetIndex = 1,colClasses = rep("numeric",5))

variables <- eu$variable[2:13]
variables <- gsub("Total\\.", "", variables)
var <- variables[1]
par(mfrow=c(2,2))
for (var in variables) {
  df <- as.data.frame(t(eu[grep(var,eu$variable),]))
  df <- df[,c(1,3,4)]
  df[] <- lapply(df, as.numeric)
  df <- df[-nrow(df),]
  colnames(df) <- c(var,"ci_l","ci_u")
  df$Year <- gsub("X","",row.names(df))
  df
  plotCI(df=df,
         y=var,
         ci_l="ci_l",
         ci_u="ci_u",
         variable="year",
         domain=NULL,
         ylim=NULL,
         specs=NULL)
}
par(mfrow=c(1,1))  
  
# eu <- read.xlsx("Europe_LC_LU_2digit.xlsx",sheetIndex = 1,colClasses = rep("numeric",5))
# 
# variables <- eu$variable[2:57]
# variables <- gsub("Total\\.", "", variables)
# var <- variables[2]
# par(mfrow=c(2,2))
# for (var in variables) {
#   if (nchar(var) > 13) {
#     df <- eu[grep(var,eu$variable),]
#     df <- as.data.frame(t(eu[grep(var,eu$variable),]))
#     df <- df[,c(1,3,4)]
#     df[] <- lapply(df, as.numeric)
#     df <- df[-nrow(df),]
#     colnames(df) <- c(var,"ci_l","ci_u")
#     df$Year <- gsub("X","",row.names(df))
#     df
#     plotCI(df=df,
#            y=var,
#            ci_l="ci_l",
#            ci_u="ci_u",
#            variable="year",
#            domain=NULL,
#            ylim=NULL,
#            specs=NULL)
#     }
#   }
# par(mfrow=c(1,1))      
#   
# eu <- read.xlsx("Europe_LC_LU_3digit.xlsx",sheetIndex = 1,colClasses = rep("numeric",5))
# 
# variables <- eu$variable[2:57]
# variables <- gsub("Total\\.", "", variables)
# var <- variables[2]
# par(mfrow=c(2,2))
# for (var in variables) {
#   if (nchar(var) > 13) {
#     df <- eu[grep(var,eu$variable),]
#     df <- as.data.frame(t(eu[grep(var,eu$variable),]))
#     df <- df[,c(1,3,4)]
#     df[] <- lapply(df, as.numeric)
#     df <- df[-nrow(df),]
#     colnames(df) <- c(var,"ci_l","ci_u")
#     df$Year <- gsub("X","",row.names(df))
#     df
#     plotCI(df=df,
#            y=var,
#            ci_l="ci_l",
#            ci_u="ci_u",
#            variable="year",
#            domain=NULL,
#            ylim=NULL,
#            specs=NULL)
#   }
# }
# par(mfrow=c(1,1))
