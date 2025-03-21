rm(list = ls())
library(rugarch)
library(purrr)
library(dplyr)
library(magrittr)

intervals_df <- readRDS("VaR_data.rds")
get_test_results <- function(id_df, alpha, VaR, col){
  
  res_df <- tibble(.rows = 1)
  
  below_var <- sum(id_df$log_return < VaR)
  above_var <- sum(id_df$log_return >= VaR)
  
  check <- above_var == length(VaR)
  check2 <- below_var == 1 & (below_var[1] | below_var[length(below_var)])
  
  if(check | check2){
    res_df[col] <- "NaN"
  }
  
  else{
    
    res <- 
      VaRTest(alpha = alpha, 
              actual=id_df$log_return, 
              VaR = VaR,
              conf.level = 1 - alpha
      )
    
    uc_reject <- res$uc.Decision == "Reject H0"
    cc_reject <- res$cc.Decision == "Reject H0"
    
    test_res <- 
      case_when(uc_reject & cc_reject ~ "+",
                uc_reject ~ "uc",
                cc_reject ~ "cc",
                TRUE ~ "-"
      )
    
    res_df[col] <- test_res
    
  }
  
  res_df
}

get_full_report_for_id <- function(id_df){
  res_df <- tibble(.rows = 1)
  res_df$id <- unique(id_df$id)
  
  res_df%>%
    cbind(get_test_results(id_df, 0.01, id_df$intervals_AgACI1[,1], "0.5_AgACI"))%>%
    cbind(get_test_results(id_df, 0.05, id_df$intervals_AgACI5[,1], "2.5_AgACI"))%>%
    cbind(get_test_results(id_df, 0.01, id_df$intervals_dtACI1[,1], "0.5_DtACI"))%>%
    cbind(get_test_results(id_df, 0.05, id_df$intervals_dtACI5[,1], "2.5_DtACI"))%>%
    cbind(get_test_results(id_df, 0.01, id_df$intervals_SF_OGD1[,1], "0.5_SF-OGD"))%>%
    cbind(get_test_results(id_df, 0.05, id_df$intervals_SF_OGD5[,1], "2.5_SF-OGD"))%>%
    cbind(get_test_results(id_df, 0.01, id_df$intervals_SAOCP1[,1], "0.5_SAOCP"))%>%
    cbind(get_test_results(id_df, 0.05, id_df$intervals_SAOCP5[,1], "2.5_SAOCP"))
  
}

get_full_report <- function(intervals_df){
  intervals_df%>%
    pull(id)%>%
    unique()%>%
    map_dfr(~intervals_df%>%
              filter(id == .x)%>%
              get_full_report_for_id()
    )
}

get_full_report(intervals_df)%>%
  saveRDS("report_df.rds")
