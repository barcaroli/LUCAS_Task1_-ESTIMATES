# # Render an html of this script
library(rmarkdown)
options("knitr.duplicate.label" = "allow")
knitr::opts_chunk$set(warning = FALSE, echo=FALSE)
rmarkdown::render("C:\\Users\\UTENTE\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES/10.plot_confidence_EU27.R",
                  # output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))

