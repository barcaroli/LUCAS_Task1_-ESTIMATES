##############################################################
# LUCAS - Check of estimates variations for all countries
##############################################################
options(stringsAsFactors = TRUE)
library(openxlsx)
library(data.table)
setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/")
filename <- "D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/tables_country/tables_all.xlsx"
load("countries.RData")
wb <- createWorkbook()

for (i in (1:length(countries))) {
  setwd("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/1.STANDARD/allyears_estimates")
  cat("\n Country: ",countries[i],"\n")
  eval(parse(text=paste("df <- fread('",countries[i],"_est_all.csv',dec='.')",sep="")))
  addWorksheet(wb, sheetName = countries[i])
  writeData(wb, sheet = countries[i], x = df, colNames = TRUE, rowNames = FALSE)
}
saveWorkbook(wb, filename, overwrite = TRUE)
