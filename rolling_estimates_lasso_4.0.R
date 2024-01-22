library(tidyverse)
library(glmnet)

#rm(list=setdiff(ls(),"final")) #drop everything, except the final dataset

#data
final_d <- final %>% select(-c(sasdate)) #whole cleaned dataset, without the dates
y_final <- final %>% select(CPIAUCSL) #variable to forecast
dates <- final %>% select(sasdate) #dates

#whole series reasoning
lambda <- 10 #lambda
lambda_f <- lambda/(nrow(final_d)*ncol(final_d)) #lambda for the function
x <- cbind(1,scale(as.matrix(final_d)[1:(nrow(final_d)-12),])) #whole dataset as matrix, but scaled and we drop the last year because we cannot compare our predictions
y <- as.matrix(lead(y_final,12))[1:(nrow(final_d)-12)] #variable to forecast
y_random_walk <- as.matrix(y_final)[1:(nrow(final_d)-12),] #random walk predictions
lasso.fit <- glmnet(as.matrix(x), as.matrix(y), lambda=lambda_f, intercept=T, standardize = F, alpha = 1) #LASSO through the command
y_forecast <- predict(lasso.fit, as.matrix(x), standardize=F, alpha = 1) #prediction through the command
date <- lead(dates,12)[1:(nrow(final_d)-12),]

#prediction plots
y_lim <- max(max(abs(y_forecast)),max(abs(y_random_walk)),max(abs(y)))
plot(y_forecast,type = "l", col = "red", ylim=c(-y_lim,y_lim), lty = 2,xaxt = "n",yaxt = "n",xlab = "",ylab = "") #LASSO prediction
par(new = T)
plot(y_random_walk,type = "l", col = "blue", ylim=c(-y_lim,y_lim), lty = 3,xaxt = "n",yaxt = "n",xlab = "",ylab = "") #rw prediction
par(new = T)
plot(date,y,type = "l",col = "black", ylim=c(-y_lim,y_lim),xlab = "Time",ylab = "") #true series
legend("bottomright", 
       legend = c("LASSO forecasts", "Random walk forecast", "True series"), 
       col = c("red", "blue", "black"),
       lty=c(2,3,1),
       bg = "white",
       cex = 0.65) #legende size
title(main = paste("LASSO forecasts with lambda equal to",lambda,", random walk forecasts and true series"))
abline(h=0,col="grey") #zero line
par(new = F)


#rolling estimates
lambda <- 1 #lambda
lambda_f <- lambda/(nrow(final_d)*119) #lambda for the function
y_pred <- matrix(NA,nrow=(nrow(final)-132),ncol=length(lambda_f))
y_ts <- matrix(NA,nrow=0,ncol=1)
y_rw <- matrix(NA,nrow=0,ncol=1)
y_dates <- matrix(NA,nrow=0,ncol=1)
all_coeffs <- data.frame()
for (i in 1:(nrow(final)-132)){ #the number of repetition is T(=nrow(final)=564) - 120(=estimation period) - 12(=preidciton skip) +1(=from 11 to 20 there are 20-11+1 numbers)
  x_train <- scale(as.matrix(final_d[i:(119+i),])) #training set (10 years, from 1 to 10)
  x_means <- colMeans(as.matrix(final_d[i:(119+i),])) #means of all the columns
  x_sds <- sqrt(diag(cov(as.matrix(final_d[i:(119+i),])))) #sds of all the columns
  x_test_ns <- as.matrix(final_d[(120+i),]) #test set (first month of the 11-th year)
  x_test <- (x_test_ns-matrix(1,nrow=nrow(x_test_ns),ncol=1)%*%x_means)%*%solve(diag(x_sds)) #test set standardized
  #by subtracting to each "column" the mean of the relative column of the test set
  #by dividing each "column" by the sd of the reltive column
  y_train <-as.matrix(y_final[(12+i):(131+i),]) #variable to forecast (10 years, from 2 to 11)
  lasso.fit <- glmnet(as.matrix(x_train), as.matrix(y_train), lambda=lambda_f, intercept=T, standardize = F, alpha = 1) #LASSO through the command
  y_pred[i,1:length(lambda_f)] <- predict(lasso.fit, as.matrix(x_test), standardize=F, alpha = 1) #prediction through the command
  y_ts[i] <- y_final[132+i,] #storing the true series, the test dependent variable
  y_rw[i] <- y_final[120+i,] #storing the true series lagged 12 month, for comparison
  y_dates[i] <- as.Date(dates[132+i,]) #storing the prediction dates
  all_coeffs=rbind(all_coeffs, as.vector(coefficients(lasso.fit)))
  print(i)
}
y_dates <- as.Date("1971-01-01") + y_dates - min(y_dates)

#prediction plots
y_lim <- max(max(abs(y_pred)),max(abs(y_rw)),max(abs(y_ts)))
plot(y_pred,type = "l", col = "red", ylim=c(-y_lim,y_lim), lty = 2,xaxt = "n",yaxt = "n",xlab = "",ylab = "") #LASSO prediction
par(new = T)
plot(y_rw,type = "l", col = "blue", ylim=c(-y_lim,y_lim), lty = 3,xaxt = "n",yaxt = "n",xlab = "",ylab = "") #rw prediction
par(new = T)
plot(y_dates,y_ts,type = "l",col = "black", ylim=c(-y_lim,y_lim),xlab = "Time",ylab = "") #true series
legend("bottomright", 
       legend = c("LASSO forecasts", "Random walk forecast", "True series"), 
       col = c("red", "blue", "black"),
       lty=c(2,3,1),
       bg = "white",
       cex = 0.65) #legende size
title(main = paste("LASSO forecasts with lambda equal to",lambda,", random walk forecasts and true series"))
abline(h=0,col="grey") #zero line
par(new = F)

#prediction plots
data1 <- data.frame(y_pred = y_pred, y_rw = y_rw, y_dates = y_dates, y_ts = y_ts)
gg<-data1 %>% 
  pivot_longer(cols = c(y_ts, y_rw, y_pred) , values_to = "values", names_to = "series") %>% 
  ggplot(aes(x=y_dates, y=values, color=series, linetype=series))+
  geom_line(size = 0.3)+
  labs(x = "Time", y = "", title = paste("LASSO forecasts with lambda equal to", lambda, ", random walk forecasts and true series"), col=NULL, linetype=NULL) +
  scale_y_continuous(limits = c(-y_lim, y_lim)) +
  scale_color_manual(values = c("red", "blue", "black"), labels=c("LASSO forecasts", "Random walk forecasts", "True series"))+
  scale_linetype_manual(values = c("dashed", "dotted", "solid"), labels=c("LASSO forecasts", "Random walk forecasts", "True series"))+
  geom_hline(yintercept = 0, color = "grey")
plot_directory <- "/Users/samueleborsini/Library/CloudStorage/GoogleDrive-sam.sb74@gmail.com/Il mio Drive/ML project/R/plots"
plot_filename <- "forecast_lasso.png"
ggsave(file.path(plot_directory, plot_filename), plot = gg, width = 9, height = 4, dpi= 1000)

#SFE and SFE_rw
SFE <- (y_pred-y_ts)^2 #squared forecasting errors of the LASSO
SFE_rw <- (y_rw-y_ts)^2 #squared forecasting errors of the rw
MSFE <- mean(SFE) #MFSE of the LASSO
MSFE_rw <- mean(SFE_rw) #MFSE of the rw
MSFE_relative <- MSFE/MSFE_rw #relative MFSE

#SFE plots
y_lim <- 0.005
plot(y_dates,SFE,type = "l", col = "red", ylim=c(0,y_lim),xaxt = "n",yaxt = "n",xlab = "",ylab = "") #LASSO squared errors
par(new = T)
plot(y_dates,SFE_rw,type = "l",col = "black", ylim=c(0,y_lim),xlab = "Time",ylab = "Squared errors") #random walk squared errors
par(new = F)

#big loop
#rolling estimates
lambda <- 10^seq(-3,3,1) #lambda
lambda_f <- lambda/(nrow(final_d)*119) #lambda for the function
MSFE_relative <- matrix(NA,nrow=0,ncol=1)
y_pred <- matrix(NA,nrow=(nrow(final)-120-12),ncol=length(lambda_f))
y_ts <- matrix(NA,nrow=0,ncol=1)
y_rw <- matrix(NA,nrow=0,ncol=1)
y_dates <- matrix(NA,nrow=0,ncol=1)
for (i in 1:(nrow(final)-120-12)){ #the number of repetition is T(=nrow(final)=564) - 120(=rolling window timeframe) - 12(=preidciton skip) +1(=from 11 to 20 there are 20-11+1 numbers)
  x_train <- scale(as.matrix(final_d[i:(119+i),])) #training set (10 years, from 1 to 10)
  x_means <- colMeans(as.matrix(final_d[i:(119+i),])) #means of all the columns
  x_sds <- sqrt(diag(cov(as.matrix(final_d[i:(119+i),])))) #sds of all the columns
  x_test_ns <- as.matrix(final_d[(120+i),]) #test set (first month of the 11-th year)
  x_test <- (x_test_ns-matrix(1,nrow=nrow(x_test_ns),ncol=1)%*%x_means)%*%solve(diag(x_sds)) #test set standardized
  #by subtracting to each "column" the mean of the relative column of the test set
  #by dividing each "column" by the sd of the reltive column
  y_train <-as.matrix(y_final[(12+i):(131+i),]) #variable to forecast (10 years, from 2 to 11)
  lasso.fit <- glmnet(as.matrix(x_train), as.matrix(y_train), lambda=lambda_f, intercept=T, standardize = F, alpha = 1) #LASSO through the command
  y_pred[i,1:length(lambda_f)] <- predict(lasso.fit, as.matrix(x_test), standardize=F, alpha = 1) #prediction through the command
  y_ts[i] <- y_final[132+i,] #storing the true series, the test dependent variable
  y_rw[i] <- y_final[120+i,] #storing the true series lagged 12 month, for comparison
  y_dates[i] <- as.Date(dates[132+i,]) #storing the prediction dates
  print(i)
}

#SFE and SFE_rw
SFE <- (y_pred-y_ts)^2 #squared forecasting errors of the LASSO
MSFE <- colMeans(SFE) #MFSE of the LASSO
SFE_rw <- (y_rw-y_ts)^2 #squared forecasting errors of the rw
MSFE_rw <- mean(SFE_rw) #MFSE of the rw
MSFE_relative <- MSFE/MSFE_rw #relative MFSE

#plot
data1 <- data.frame(l = lambda, MSFE_relative = MSFE_relative)
gg <- ggplot(data1, aes(x = log10(l), y = MSFE_relative)) +
  geom_line() +
  labs(x = "Log Lambda", y = "MSFE relative", title = "MFSE for LASSO")

# Save the plot
plot_directory <- "/Users/samueleborsini/Library/CloudStorage/GoogleDrive-sam.sb74@gmail.com/Il mio Drive/ML project/R/plots"
plot_filename <- "MSFE_lasso.png"
ggsave(file.path(plot_directory, plot_filename), plot = gg, width = 6, height = 4, dpi = 1000)

#storing results
y_pred_lasso <- y_pred
MSFE_relative_lasso <- MSFE_relative
lambda_lasso <- lambda











# Lasso coefficients shrunk to 0  -----------------------------------------
all_coeffs %>% 
  mutate(window=row_number()) %>% 
  pivot_longer(cols=1:(ncol(.)-1), names_to = "betas", values_to = "coeffs") %>% 
  mutate(zeroes= as.factor(ifelse(coeffs==0, 1,0))) %>% 
  ggplot(aes(x=window, y=betas, fill=zeroes))+
  geom_tile()+
  scale_fill_manual(values=c("red", "light grey"),
                    labels = c("Zero", "Non-zero"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  # scale_x_continuous(breaks = c(108, 288, 468),
  #                    labels= c(1980, 1995, 2010))+
  labs(x="Rolling windows", y = "Coefficients", fill="Coefficients")+
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.size =  unit(0.4, 'cm'))
ggsave("zero coefficients in LASSO.png", width=9, height = 3.5, units = "in", dpi=1000)
