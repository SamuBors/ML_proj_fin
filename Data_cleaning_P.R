library(tidyverse)
remove(list=ls())
data <- read.csv("/Users/samueleborsini/Library/Mobile Documents/com~apple~CloudDocs/Università/Economics and econometrics/II anno/Machine Learning/Project/Dati/current.csv")

# Drop some variables
data <- data %>% 
  # drop because many missing values 
  select(-c("ACOGNO","VIXCLSx","ANDENOx","TWEXAFEGSMTHx","UMCSENTx","NONBORRES")) %>% 
  mutate(sasdate=as.Date(sasdate, "%m/%d/%Y"))
    # ACOGNO we dont have naything until 1992
    # VIXCLSx: first obs july 1962
    # ANDENOx: first obs Feb 1968 
    # TWEXAFEGSMTHx: Dec 1972
    # UMCSENTx: trimestral data until Jan 1978
    # "NONBORRES" structural break in 2010

transf <- data[1, ]

# Drop covid ' this also removes the tranfs observation
data_pre <- data %>% filter(sasdate<"2020-01-01")
#data_pre <- data %>% filter(sasdate<"2008-01-01")
# Keep separate data for covid
covid_data <- data %>% filter(sasdate>"2019-12-01")


# Separate outcomes from regressors
# outcomes <- data_pre_2020 %>% 
#   select(CPIAUCSL, INDPRO)
# data_pre_2020 <- data_pre_2020 %>% 
#   select(-CPIAUCSL, -INDPRO)


# Transformation regressors
transf_rp <- transf %>% 
  select(-sasdate) %>% 
  pivot_longer(cols = everything(), names_to = "vars", values_to = "transf")

a <- transf_rp %>% filter(transf==2) 
tranfs_2 <- unique(a$vars)
a <- transf_rp %>% filter(transf==3) 
tranfs_3 <- unique(a$vars)
a <- transf_rp %>% filter(transf==4) 
tranfs_4 <- unique(a$vars)
a <- transf_rp %>% filter(transf==5) 
tranfs_5 <- unique(a$vars)
a <- transf_rp %>% filter(transf==6) 
tranfs_6 <- unique(a$vars)
a <- transf_rp %>% filter(transf==7) 
tranfs_7 <- unique(a$vars)

# apply transformations to regressors (annual)
data_transformed <- data_pre %>%
  mutate_at(tranfs_2, ~(.-lag(.,12))) %>%
  mutate_at(tranfs_3, ~((.-lag(.,12) - (lag(.,12)-lag(.,24))))) %>%
  mutate_at(tranfs_4, ~(log(.))) %>%
  #mutate_at(tranfs_4, ~(log(.)-log(lag(.,12)))) %>%
  mutate_at(tranfs_5, ~(log(.)-log(lag(.,12)))) %>%
  mutate_at(tranfs_6, ~((log(.)-log(lag(.,12))) - (log(lag(.,12))- log(lag(.,24))))) %>%
  mutate_at(tranfs_7, ~(((./lag(.,12))-1)-((lag(.)/lag(.,24))-1)))



# apply transformations to regressors (semestral)
# data_transformed <- data_pre %>%
#   mutate_at(tranfs_2, ~(.-lag(.,6))) %>%
#   mutate_at(tranfs_3, ~((.-lag(.,6) - (lag(.,6)-lag(.,12))))) %>%
#   # mutate_at(tranfs_4, ~(log(.))) %>%
#   mutate_at(tranfs_4, ~(log(.)-log(lag(.,6)))) %>%
#   mutate_at(tranfs_5, ~(log(.)-log(lag(.,6)))) %>%
#   mutate_at(tranfs_6, ~((log(.)-log(lag(.,6))) - (log(lag(.,6))- log(lag(.,12))))) %>%
#   mutate_at(tranfs_7, ~(((./lag(.,6))-1)-((lag(.,6)/lag(.,12))-1)))


# #apply transformations to regressors (monthly)
# data_transformed <- data_pre %>%
#   mutate_at(tranfs_2, ~(.-lag(.,1))) %>%
#   mutate_at(tranfs_3, ~((.-lag(.,1) - (lag(.,1)-lag(.,2))))) %>%
#   mutate_at(tranfs_4, ~(log(.))) %>%
#   mutate_at(tranfs_5, ~(log(.)-log(lag(.,1)))) %>%
#   mutate_at(tranfs_6, ~((log(.)-log(lag(.,1))) - (log(lag(.,1))- log(lag(.,2))))) %>%
#   mutate_at(tranfs_7, ~(((./lag(.,1))-1)-((lag(.)/lag(.,2))-1)))

# Transformation outcomes
# outcomes_transformed <- outcomes %>% 
#   # mutate(INDPRO = log(INDPRO)*100,
#          # CPIAUCSL = log(CPIAUCSL/lag(CPIAUCSL,12))*100)
#   mutate(INDPRO =  log(INDPRO)*100 -log(lag(INDPRO,12))*100,
#          CPIAUCSL = log(CPIAUCSL/lag(CPIAUCSL,12))*100 - log(lag(CPIAUCSL,12)/lag(CPIAUCSL,24))*100)


# lose first 24 obs because we lose it with lags
data_transformed <- data_transformed %>% slice(25:(nrow(.)))
# outcomes_transformed <- outcomes_transformed %>% slice(25:(nrow(.)))
    #now we have from jan 1961 to dec 2019 - for the sevent-th transformation we lose 2 years of data

 #standardize all variables
 dates <- data_transformed[,1]
 data_standardized <- data_transformed %>% 
   select(-sasdate) %>% 
   mutate_all( ~(scale(.) %>% as.vector)) %>% 
   cbind(data.frame(sasdate=dates))

 # outcomes_standardized <- outcomes_transformed %>% 
 #   mutate_all( ~(scale(.) %>% as.vector)) 



# Set final dataset -------------------------------------------------------
  #Put regressors and outcomes together
final <-  data_transformed 

#plot NONBORRES
# gg <- ggplot(final, aes(x = sasdate, y = NONBORRES)) +
#  geom_line() +
#  labs(x = "Time", y = "")
# plot_directory <- "/Users/samueleborsini/Library/CloudStorage/GoogleDrive-sam.sb74@gmail.com/Il mio Drive/ML project/R/plots"
# plot_filename <- "NONBORRES.png"
# ggsave(file.path(plot_directory, plot_filename), plot = gg, width = 9, height = 4, dpi = 1000)
 



#RUN UNTIL HERE TO GET DATA READY
# The vairbales we want to predict is INDPRO and CPIAUCSL



# Testing stationarity ----------------------------------------------------
aa <- final %>% 
  mutate(rowid=row_number())

library(tseries)
adf_results <- data.frame()
for (i in colnames(aa)) {
  reg_model <- lm(data=aa, 
                  formula = paste0(i, "~rowid"))
  res <- reg_model$residuals
  # a <- tseries::adf.test(data_standardized[,paste0(i)], k=1)
  a <- tseries::adf.test(res, k=1)
  adf_results <- rbind(adf_results,
                       data.frame(variable=i,
                                  p_value=a$p.value))
}










