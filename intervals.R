rm(list = ls())
library(magrittr)
library(purrr)
library(forecast)
library(dplyr)
library(AdaptiveConformal)

df <- readRDS("data_with_predictions.rds")

intervals_df <- 
df%>%
  filter(!is.na(predicted_log_return))%>%
  group_by(id)%>%
  summarise(
            ###################################################################################################################
            # alpha 5%
            intervals_AgACI5 = aci(method = "AgACI")%>%
                                update(newY = log_return, newpredictions = predicted_log_return)%$%
                                intervals,
            
            # no FACI in the package :(
            intervals_dtACI5 = aci(method = "DtACI")%>%
                                update(newY = log_return, newpredictions = predicted_log_return)%$%
                                intervals,
            
            intervals_SF_OGD5 = aci(method = "SF-OGD")%>%
                                update(newY = log_return, 
                                       newpredictions = predicted_log_return,
                                       parameters=list(gamma = max(abs(log_return-predicted_log_return))/sqrt(3))
                                )%$%
                                intervals,
            intervals_SAOCP5 = aci(method = "SAOCP")%>%
                                update(newY = log_return, 
                                       newpredictions = predicted_log_return,
                                       parameters=list(D = max(abs(log_return-predicted_log_return))/sqrt(3))
                                       )%$%
                                intervals,
            
            ###################################################################################################################
            # alpha 1%
            intervals_AgACI1 = aci(method = "AgACI", alpha = 0.99)%>%
                                    update(newY = log_return, newpredictions = predicted_log_return)%$%
                                    intervals,
            
            # no FACI in the package :(
            intervals_dtACI1 = aci(method = "DtACI", alpha = 0.99)%>%
                                    update(newY = log_return, newpredictions = predicted_log_return)%$%
                                    intervals,
                                  
            intervals_SF_OGD1 = aci(method = "SF-OGD", alpha = 0.99)%>%
                                  update(newY = log_return, 
                                         newpredictions = predicted_log_return,
                                         parameters=list(gamma = max(abs(log_return-predicted_log_return))/sqrt(3))
                                  )%$%
                                  intervals,
            intervals_SAOCP1 = aci(method = "SAOCP", alpha = 0.99)%>%
                                update(newY = log_return, 
                                       newpredictions = predicted_log_return,
                                       parameters=list(D = max(abs(log_return-predicted_log_return))/sqrt(3))
                                )%$%
                                intervals
  )
  
# we can do that since the order of days is preserved
intervals_df$day_num <- df%>%filter(!is.na(predicted_log_return))%$%day_num
intervals_df$log_return <- df%>%filter(!is.na(predicted_log_return))%$%log_return
saveRDS(intervals_df, "VaR_data.rds")


