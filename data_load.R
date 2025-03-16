rm(list = ls())
gc()
library(dplyr)
library(crypto2)
library(purrr)

crypto_df <- 
  crypto_list()%>%
    filter(id <= 10000)

walk(unique(crypto_df$id),
    ~ crypto_df%>%
       filter(id == .x)%>%
       crypto_history(convert = "USD")%>%
       saveRDS(paste0("loaded_data\\", .x, ".rds"))
)
