install.packages("readxl")
install.packages("readr")
install.packages("Writexl")
install.packages("WriteXLS")
install.packages("haven")

library(readr)
library(haven)
library(readxl)
library(writexl)
library(WriteXLS)

# Import Kenya_agri_disease_spatial.csv data and export in xls, sas, spss, and stata 
Kenya_agri_disease_spatial <- read_csv("Kenya_agri_disease_spatial.csv")

# Export data
write_xlsx(Kenya_agri_disease_spatial,"Kenya_agri_disease_spatial.xlsx")
WriteXLS(Kenya_agri_disease_spatial, "Kenya_agri_disease_spatial.xls")
write_dta(Kenya_agri_disease_spatial,"Kenya_agri_disease_spatial.dta")
write_sav(Kenya_agri_disease_spatial,"Kenya_agri_disease_spatial.sav")

# The fuction write_sas is deprecated
write_sas(Kenya_agri_disease_spatial, "Kenya_agri_disease_spatial.sas")
#Warning message:
#  `write_sas()` was deprecated in haven 2.5.2.
#ℹ Please use `write_xpt()` instead.
#This warning is displayed once every 8 hours.
#Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated. 

# Export using write_xpt function
write_xpt(Kenya_agri_disease_spatial, "Kenya_agri_disease_spatial.sas")

#Import using read_xpt()
read_xpt("Kenya_agri_disease_spatial.sas")




