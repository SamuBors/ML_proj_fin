library(tidyverse)


# Change directory of loading data in data_cleaning_P
# C:/Users/pablo/OneDrive/:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/current.csv
wd <- "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Final estimations"
source(file = paste0(wd, "/Data_cleaning_P.R"))
wd <- "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Final estimations"
source(file = paste0(wd, "/rolling_estimates_lasso_4.0.R"))
wd <- "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Final estimations"
source(file = paste0(wd, "/rolling_estimates_ridge_4.0.R"))
wd <- "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Final estimations"
source(file = paste0(wd, "/rolling_estimates_pcr_4.0.R"))
wd <- "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Final estimations"
source(file = paste0(wd, "/rolling_estimates_pls_4.0.R"))
wd <- "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Final estimations"
source(file = paste0(wd, "/Comparison.R"))


MSFE_all <- data.frame(MSFE_relative_ridge,
                       MSFE_relative_lasso,
                       MSFE_relative_pcr,
                       MSFE_relative_pls)
comparison <- data.frame(cor_lasso = cor_lasso$correlation,
                         cor_ridge = cor_ridge$correlation,
                         cor_pls = cor_pls$correlation)


write.csv(MSFE_all, "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Tables/MSFE_all.csv",
          row.names = FALSE)
write.csv(comparison, "C:/Users/pablo/OneDrive/Escritorio/Master/ML/ML_assignment/Tables/comparison.csv",
          row.names = FALSE)


