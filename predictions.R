library(magrittr)
library(purrr)
library(forecast)
library(dplyr)

# loaded data concatenation + log_returns + some helpful vars
df <- 
  list.files("loaded_data")%>%
    map_dfr(~.x%>%
              paste0("loaded_data\\", .)%>%
              readRDS()
    )%>%
  
  select(id, time_open, close)%>%
  group_by(id)%>%
  filter(time_open%>%
          length()%>%
          unique()%>%
          between(60, 730)
  )%>%
  arrange(id, time_open)%>%
  mutate(day_num = row_number())%>%
  mutate(last_day_num = max(day_num))%>%
  mutate(log_return = close%>%
                        `/`(lag(close, 1))%>% 
                        log(),
         last_prediction_day = last_day_num/2
  )%>%
  filter(!is.na(log_return))%>%
  ungroup()%>%
  select(-close)


# expanding window, auto.arima on every step
predictions_df <- data.frame(id = numeric(), day_num = numeric(), predicted_log_return = numeric())
while(TRUE){
  
    rows_left <- df%>%filter(day_num > last_prediction_day)%>%nrow()
    if(rows_left == 0){break}
    print(paste0("rows left: ", rows_left))
      
    current_predictions <- 
      df%>%
        filter(day_num < last_prediction_day + 1)%>%
        group_by(id)%>%
        summarise(day_num = max(day_num) + 1,
                  predicted_log_return = log_return%>%
                                          auto.arima()%>%
                                          forecast(h = 1)%$%
                                          mean
        )%>%
        ungroup()
      
      predictions_df%<>%
        rbind(current_predictions)
    
    df%<>%
      mutate(last_prediction_day = ifelse(id %in% current_predictions$id, last_prediction_day + 1, last_prediction_day))
}
  
# add predicted values and save df for further usage
df%<>%
  left_join(predictions_df, by = c("day_num"="day_num", "id"="id"))

saveRDS(df, "data_with_predictions.rds")
