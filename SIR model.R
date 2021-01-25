library(tidyverse)
library(dplyr)
library(ggplot2)
update.packages('covdata')
library(covdata)
library(covidregionaldata)
library(ggplot2)
library(forecast)
################################################### Cleaning/Isolation
data_isolate <- function(df){
  final_df <- ungroup(df) %>%
    select(date, cases_total, deaths_total, recovered_total)
  final_df[is.na(final_df)] <- 0
  return(final_df)  
}
################################################### Generate SIR on data
data_SIR <- function(df, pop){
  S <- pop - select(df,cases_total)
  I <- select(df, cases_total) - select(df, recovered_total) -
    select(df, deaths_total)
  R <- select(df,recovered_total) + select(df,deaths_total)
  colnames(S) <- 'S'
  colnames(I) <- 'I'
  colnames(R) <- 'R'
  return(list(S = as.data.frame(cbind(select(df,days_elapsed),S)),
              I = as.data.frame(cbind(select(df,days_elapsed),I)),
              R = as.data.frame(cbind(select(df,days_elapsed),R))))  
}
################################################### Calculate dS, dI, dR
data_dSIR <- function(S,I,R){
  dS <- numeric(nrow(S) -1)
  dI <- numeric(nrow(I) -1)
  dR <- numeric(nrow(R) -1)
  for(i in 1:nrow(S) -1) {
    dS[i] <- S[i+1,2] - S[i,2]
    dI[i] <- I[i+1,2] - I[i,2]
    dR[i] <- R[i+1,2] - R[i,2]
  }
  S <- S[-c(1),]
  I <- I[-c(1),]
  R <- R[-c(1),]
  return(list(S = as.data.frame(S),I = as.data.frame(I),R = as.data.frame(R),
              dS = as.data.frame(cbind(select(S,days_elapsed),dS)), 
              dI = as.data.frame(cbind(select(S,days_elapsed),dI)),
              dR = as.data.frame(cbind(select(S,days_elapsed),dR))))
}
################################################### Find implied B
find_B <- function(S,I,dS,pop){
  B <- -(dS[,2]*pop) / (I[,2]*S[,2])
  B[is.na(B)] <- 0
  B <- as.data.frame(B)
  colnames(B) <- 'B'
  return(as.data.frame(cbind(select(I,days_elapsed),B)))
}
################################################### Find implied Y
find_Y <- function(dR,I){
  Y <- dR[,2] / I[,2]
  Y[is.na(Y)] <- 0
  Y <- as.data.frame(Y)
  colnames(Y) <- 'Y'
  return(as.data.frame(cbind(select(I,days_elapsed),Y)))
}
################################################### Forecast using values
forecast_SIR <- function(SIR, avg_B, avg_Y, sd_B, sd_Y, pop, pl){
  data <- data.frame(days_elapsed = integer(pl), pred_S=double(pl), pred_S_u=double(pl), pred_S_l=double(pl),
                          pred_I=double(pl), pred_I_u=double(pl), pred_I_l=double(pl),
                          pred_R=double(pl), pred_R_u=double(pl), pred_R_l=double(pl),
                          pred_dS=double(pl), pred_dS_u=double(pl), pred_dS_l=double(pl),
                          pred_dI=double(pl), pred_dI_u=double(pl), pred_dI_l=double(pl),
                          pred_dR=double(pl), pred_dR_u=double(pl), pred_dR_l=double(pl))
  
  data$days_elapsed = seq.int(from = SIR$S[nrow(SIR$S),1]+1, by = 1, length.out = pl)
  pl <- pl-1

  data$pred_S[1] <- SIR$S[nrow(SIR$S),2] + SIR$dS[nrow(SIR$dS),2]
  data$pred_I[1] <- SIR$I[nrow(SIR$I),2] + SIR$dI[nrow(SIR$dI),2]
  data$pred_R[1] <- SIR$R[nrow(SIR$R),2] + SIR$dR[nrow(SIR$dR),2]
  ##
  data$pred_S_u[1] <- SIR$S[nrow(SIR$S),2] + SIR$dS[nrow(SIR$dS),2]
  data$pred_I_u[1] <- SIR$I[nrow(SIR$I),2] + SIR$dI[nrow(SIR$dI),2]
  data$pred_R_u[1] <- SIR$R[nrow(SIR$R),2] + SIR$dR[nrow(SIR$dR),2]
  ##
  data$pred_S_l[1] <- SIR$S[nrow(SIR$S),2] + SIR$dS[nrow(SIR$dS),2]
  data$pred_I_l[1] <- SIR$I[nrow(SIR$I),2] + SIR$dI[nrow(SIR$dI),2]
  data$pred_R_l[1] <- SIR$R[nrow(SIR$R),2] + SIR$dR[nrow(SIR$dR),2]
  for(i in 1:pl) {
    data$pred_dS[i] <- -(data$pred_S[i]*data$pred_I[i]*avg_B)/pop
    data$pred_dI[i] <- ((data$pred_S[i]*data$pred_I[i]*avg_B)/pop) - avg_Y*data$pred_I[i]
    data$pred_dR[i] <- avg_Y*data$pred_I[i]
    ##
    data$pred_dS_u[i] <- -(data$pred_S_u[i]*data$pred_I_u[i]*(avg_B-2.45*sd_B))/pop
    data$pred_dI_u[i] <- ((data$pred_S_u[i]*data$pred_I_u[i]*(avg_B+2.45*sd_B))/pop) - (avg_Y-2.45*sd_Y)*data$pred_I_u[i]
    data$pred_dR_u[i] <- (avg_Y+2.45*sd_Y)*data$pred_I_u[i]
    ##
    data$pred_dS_l[i] <- -(data$pred_S_l[i]*data$pred_I_l[i]*(avg_B+2.45*sd_B))/pop
    data$pred_dI_l[i] <- ((data$pred_S_l[i]*data$pred_I_l[i]*(avg_B-2.45*sd_B))/pop) - (avg_Y+.45*sd_Y)*data$pred_I_l[i]
    data$pred_dR_l[i] <- (avg_Y-2.45*sd_Y)*data$pred_I_l[i]
    ##
    data$pred_S[i+1] <- data$pred_S[i] + data$pred_dS[i]
    data$pred_I[i+1] <- data$pred_I[i] + data$pred_dI[i]
    data$pred_R[i+1] <- data$pred_R[i] + data$pred_dR[i]
    ##
    data$pred_S_u[i+1] <- data$pred_S_u[i] + data$pred_dS_u[i]
    data$pred_I_u[i+1] <- data$pred_I_u[i] + data$pred_dI_u[i]
    data$pred_R_u[i+1] <- data$pred_R_u[i] + data$pred_dR_u[i]
    ##
    data$pred_S_l[i+1] <- data$pred_S_l[i] + data$pred_dS_l[i]
    data$pred_I_l[i+1] <- data$pred_I_l[i] + data$pred_dI_l[i]
    data$pred_R_l[i+1] <- data$pred_R_l[i] + data$pred_dR_l[i]}
  return(data)
}
################################################### Visualization
plot_S <- function(SIR, SIR_predictions,S) {
  plot_data <- as.data.frame(cbind(SIR$S[,1],SIR$S[,2],SIR$S[,2],SIR$S[,2]))
  colnames(plot_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  bind_data <- as.data.frame(cbind(SIR_predictions$days_elapsed,
                                   SIR_predictions$pred_S_l, SIR_predictions$pred_S,
                                   SIR_predictions$pred_S_u))
  colnames(bind_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  plot_data <- rbind(plot_data, bind_data)
  ggplot(plot_data, aes(x=Days_Elapsed)) +
    geom_line(aes(y=lower),color = "black", size = 0.5) + 
    geom_line(aes(y=upper),color = "black", size = 0.5) + 
    geom_line(aes(y=trend),color = "cornflowerblue", size = 1) +
    labs(title = "S Population + SIR Projection", x = "Days since 1st Measurement",
         y = "S Count") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "White", colour = "Black",
                                          size = 1, linetype = "solid"))
}
plot_I <- function(SIR, SIR_predictions,S) {
  plot_data <- as.data.frame(cbind(SIR$I[,1],SIR$I[,2],SIR$I[,2],SIR$I[,2]))
  colnames(plot_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  bind_data <- as.data.frame(cbind(SIR_predictions$days_elapsed,
                                   SIR_predictions$pred_I_l, SIR_predictions$pred_I,
                                   SIR_predictions$pred_I_u))
  colnames(bind_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  plot_data <- rbind(plot_data, bind_data)
  ggplot(plot_data, aes(x=Days_Elapsed)) +
    geom_line(aes(y=lower),color = "black", size = 0.5) + 
    geom_line(aes(y=upper),color = "black", size = 0.5) + 
    geom_line(aes(y=trend),color = "cornflowerblue", size = 1) + 
    labs(title = "I Population + SIR Projection", x = "Days since 1st Measurement",
         y = "I Count") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "White", colour = "Black",
                                          size = 1, linetype = "solid"))
}
plot_R <- function(SIR, SIR_predictions,S) {
  plot_data <- as.data.frame(cbind(SIR$R[,1],SIR$R[,2],SIR$R[,2],SIR$R[,2]))
  colnames(plot_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  bind_data <- as.data.frame(cbind(SIR_predictions$days_elapsed,
                                   SIR_predictions$pred_R_l, SIR_predictions$pred_R,
                                   SIR_predictions$pred_R_u))
  colnames(bind_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  plot_data <- rbind(plot_data, bind_data)
  ggplot(plot_data, aes(x=Days_Elapsed)) +
    geom_line(aes(y=lower),color = "black", size = 0.5) + 
    geom_line(aes(y=upper),color = "black", size = 0.5) + 
    geom_line(aes(y=trend),color = "cornflowerblue", size = 1) + 
    labs(title = "R Population + SIR Projection", x = "Days since 1st Measurement",
         y = "R Count") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "White", colour = "Black",
                                          size = 1, linetype = "solid"))
}
plot_Case <- function(SIR, SIR_predictions,S) {
  plot_data <- as.data.frame(cbind(SIR$S[,1],SIR$S[,2],SIR$S[,2],SIR$S[,2]))
  colnames(plot_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  bind_data <- as.data.frame(cbind(SIR_predictions$days_elapsed,
                                   SIR_predictions$pred_S_l, SIR_predictions$pred_S,
                                   SIR_predictions$pred_S_u))
  colnames(bind_data) <- c("Days_Elapsed", "lower", "trend", "upper")
  plot_data <- rbind(plot_data, bind_data)
  plot_data$lower <- S - plot_data$lower
  plot_data$upper <- S - plot_data$upper
  plot_data$trend <- S - plot_data$trend
  ggplot(plot_data, aes(x=Days_Elapsed)) +
    geom_line(aes(y=lower),color = "black", size = 0.5) + 
    geom_line(aes(y=upper),color = "black", size = 0.5) + 
    geom_line(aes(y=trend),color = "cornflowerblue", size = 1) + 
    labs(title = "Case Count + SIR Projection", x = "Days since 1st Measurement",
         y = "Case Count") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "White", colour = "Black",
                                          size = 1, linetype = "solid"))
}
################################################### Main
region <- "Maharashtra"
data <- get_regional_data(country = "India", include_level_2_regions = FALSE)
data <- data %>%
  filter(state == region)
main_data <- data[40:(nrow(data)-171),]
trim_data <- data_isolate(main_data) %>%
  mutate(days_elapsed = as.numeric(date - min(date), units="days"))
SIR <- data_SIR(trim_data, (112870000))
SIR <- data_dSIR(SIR$S, SIR$I, SIR$R)
B <- find_B(SIR$S, SIR$I, SIR$dS, 112870000)
Y <- find_Y(SIR$dR, SIR$I)
avg_B <- tail(B$B,7) %>%
  mean()
sd_B <- tail(B$B,7) %>%
  sd()
avg_Y <- tail(Y$Y,7) %>%
  mean()
sd_Y <- tail(Y$Y,7) %>%
  sd()
SIR_predictions <- forecast_SIR(SIR,avg_B,avg_Y,sd_B,sd_Y,112870000,7)
plot_I(SIR, SIR_predictions, 112870000)
plot(B$B)
