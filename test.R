batch_table <- Batch_hospital_table("data/",new_cols = new_cols)
month_data_list <- list.files("data/") %>% str_remove(".xlsx")

weekday_summary <- week_summary(batch_table,name_id = "焦宇",mod ="weekday" ,
                                month_data_list = month_data_list,types = weekday_type_commm)

weekend_summary <- week_summary(batch_table,name_id = "焦宇",mod ="weekend" ,
                                month_data_list = month_data_list ,types = weekend_type_commm)

weeks_universe <- week_summary(batch_table,name_id = "焦宇",mod ="universe" ,
                               month_data_list = month_data_list ,types = universe_type)

df_templist <- list(weeks_universe,
                    weekend_summary,
                    weekday_summary)
df <- data.table::rbindlist(df_templist,fill = TRUE) %>% 
  gather(key = "month", value = "value", -types)
weekdaysscore <- list()
for (months_item in month_data_list) {
  temp <- rep(0,length(weekday_type_commm))
  for (i in 1:length(weekday_type_commm)) {
    temp[i] <- get_name_score(batch_table,name = "焦宇",mod = "weekday",
                              type = weekday_type_commm[i],month_data = months_item)
  }
  weekdaysscore[[months_item]] <- data.frame(types = weekday_type_commm,
                                             temp = temp) %>% 
    rename(months_item = temp)
}
weekdaysscore <- do.call("cbind",weekdaysscore) %>% 
  select(paste0(month_data_list[1],".types"),ends_with("months_item"))
names(weekdaysscore) <- c("types",month_data_list)

temps <- get_name_score(batch_table,name = "王子怡"，mod = 'weekday',type = weekday_type_commm[1],
                        month_data = "agu")


df <- batch_table %>% 
  filter(month == month_data_list)
mod = "weekday"
type = weekday_type_commm[1]
name = "焦宇"
if (mod == "weekday"){
  df <- df %>% 
    select(-all_of(c("sat","sun","weekend_type"))) %>% 
    filter(weekday_type == type)
  #row_level <- levels(factor(df$weekday_type))
  
}else if(mod == "weekend"){
  df <- df %>% 
    select(all_of(c("weekend_type","sat","sun"))) %>% 
    filter(weekend_type == type)
} else {
  df <- df %>% 
    select(-weekend_type) %>% 
    filter(weekday_type == type)
}
df <- as.data.frame(df)
score <- 0
for (i in 1:ncol(df)){
  for (a in 1:nrow(df)) {
    print(df[a,i])
    if (is.na(df[a,i])){
      score_temp = score + 0
      score = score_temp
    }else if(str_detect(df[a,i],name) == TRUE){
      score_temp = score + 1
      score = score_temp
    } else {
      score_temp = score + 0
      score = score_temp
    }
  }
}

return(score)
}
