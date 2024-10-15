##############################################################
# LUCAS - Check of estimates variations for all countries
##############################################################
options(stringsAsFactors = TRUE)
library(xlsx)
setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc - with LUE LUD")
filename <- "C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc - with LUE LUD/tables_country/tables_all.xlsx"
unlink(filename)

m <- read.csv2("master2019_with_weights_and_strata_2018.csv")
countries <- levels(m$NUTS0_16)

i <- which(countries == "MT")

for (i in (1:length(countries))) {
  df2 <- NULL
  setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc - with LUE LUD/allyears_estimates")
  cat("\n Country: ",countries[i],"\n")
  st <- paste("df <- read.csv2('",countries[i],"_est_all.csv',dec='.')",sep="")
  eval(parse(text=st))
  values <- c("fao_class_name1", "fao_class_name2" , "fao_class_name3", 
              "lud0", "lud1", "lue0", "lue1",
              "settl_pc","settlement1",
              "SURVEY_LC1_1A" , "SURVEY_LC1_1B" , "SURVEY_LC1_1C" , "SURVEY_LC1_1D" , "SURVEY_LC1_1E" , "SURVEY_LC1_1F",
              "SURVEY_LC1_1G" , "SURVEY_LC1_1H",
              "SURVEY_LU1_1U1" , "SURVEY_LU1_1U2",
              "SURVEY_LU1_1U3" , "SURVEY_LU1_1U4")
  fiancata <- c("fao_class = 1", "fao_class = 2" , "fao_class = 3", 
                "LUD = 0", "LUD = 1", "LUE = 0", "LUE = 1",  
                "settlement pro-capite","settlement = 1",
                "Land cover = A" , "Land cover = B" , "Land cover = C" , "Land cover = D" , "Land cover = E" , "Land cover = F",
                "Land cover = G" , "Land cover = H",
                "Land use = U1" , "Land use = U2",
                "Land use = U3" , "Land use = U4"
                )
  if (countries[i] == "LU") {
      values <- c("fao_class_name1", "fao_class_name2" , "fao_class_name3", 
                  "lud0", "lud1", "lue0", "lue1",
                  "settl_pc","settlement1",
                    "SURVEY_LC1_1A" , "SURVEY_LC1_1B" , "SURVEY_LC1_1C" , "SURVEY_LC1_1D" , "SURVEY_LC1_1E" , "SURVEY_LC1_1F",
                    "SURVEY_LC1_1G" , 
                    "SURVEY_LU1_1U1" , "SURVEY_LU1_1U2",
                    "SURVEY_LU1_1U3" , "SURVEY_LU1_1U4")
      fiancata <- c("fao_class = 1", "fao_class = 2" , "fao_class = 3", 
                    "LUD = 0", "LUD = 1", "LUE = 0", "LUE = 1",  
                    "settlement pro-capite","settlement = 1",
                    "Land cover = A" , "Land cover = B" , "Land cover = C" , "Land cover = D" , "Land cover = E" , "Land cover = F",
                    "Land cover = G" , 
                    "Land use = U1" , "Land use = U2",
                    "Land use = U3" , "Land use = U4"
                    )
  }
  if (countries[i] == "MT") {
    values <- c("fao_class_name1", "fao_class_name2" , "fao_class_name3", 
                # "LUD = 0", "LUD = 1", "LUE = 0", "LUE = 1",  
                "settl_pc","settlement1",
                "SURVEY_LC1_1A" , "SURVEY_LC1_1B" , "SURVEY_LC1_1C" , "SURVEY_LC1_1D" , "SURVEY_LC1_1E" , "SURVEY_LC1_1F",
                "SURVEY_LC1_1G" , 
                "SURVEY_LU1_1U1" , 
                "SURVEY_LU1_1U3" , "SURVEY_LU1_1U4")
    fiancata <- c("fao_class = 1", "fao_class = 2" , "fao_class = 3", 
                  # "LUD = 0", "LUD = 1", "LUE = 0", "LUE = 1",  
                  "settlement pro-capite","settlement = 1",
                  "Land cover = A" , "Land cover = B" , "Land cover = C" , "Land cover = D" , "Land cover = E" , "Land cover = F",
                  "Land cover = G" , 
                  "Land use = U1" , 
                  "Land use = U3" , "Land use = U4"
    )
  }
  if (ncol(df) == 21) {
     df2 <- df[df$Variable %in% values,
               c("Variable","Area_2009","Std_error","CV",
                 "Area_2012","Std_error.1","CV.1",
                 "Area_2015","Std_error.2","CV.2",
                 "Area_2018","Std_error.3","CV.3")]
     df2$Variable <- fiancata
     # setwd("C:/Users/Giulio/Google Drive/LUCAS/task 4/estimates_hrl_clc/check_tables")
     write.xlsx(df2, filename, sheetName = countries[i], 
                col.names = TRUE, row.names = FALSE, append = TRUE)
     # write.table(df2,filename,row.names=FALSE,sep=";")
  }
  if (ncol(df) == 16) {
    df2 <- df[df$Variable %in% values,c("Variable",
                                       "Area_2012","Std_error","CV",
                                       "Area_2015","Std_error.1","CV.1",
                                       "Area_2018","Std_error.2","CV.2")]
    df2$Variable <- fiancata
    write.xlsx(df2, filename, sheetName = countries[i], 
               col.names = TRUE, row.names = FALSE, append = TRUE)
  }
  if (ncol(df) == 11) {
    df2 <- df[df$Variable %in% values,c("Variable",
                                        "Area_2015","Std_error","CV",
                                        "Area_2018","Std_error.1","CV.1")]
    df2$Variable <- fiancata
    write.xlsx(df2, filename, sheetName = countries[i], 
               col.names = TRUE, row.names = FALSE, append = TRUE)
  }
}

