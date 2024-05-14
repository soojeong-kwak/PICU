##### EtCO2
##### file load
fp = "C:/Users/user/Desktop/PICU/PCO2/0 Data/"
sp = "C:/Users/user/Desktop/PICU/PCO2/2 Works/"

source(paste0(sp, "99_packageList.R"))

pco2_join   = read_csv(paste0(sp, "pco2_join.csv"), show_col_types = FALSE)
fn_rows0415 = read_csv(paste0(fp, "rid_filename_rows0415.csv"), show_col_types = FALSE)
search0415  = fn_rows0415 %>% 
  dplyr::filter(hn %in% unique(fin_file_info$hn))


##### extract point data by hospital number
etco2 = tibble()
id = unique(pco2_join$hn)

for(f in id){
  
  tmp = search0415 %>% dplyr::filter(hn == f)
  
  if(nrow(tmp) == 0) 
    next
  
  hn_etco2 = tibble()
  
  for (i in 1:nrow(tmp)){
    
    df = read_csv(paste0(fp, "0415/samples_exclude_0_row/",tmp$csvnames[i]), show_col_types = FALSE)
    df %<>% select(hn, pco2_dttm, pco2, Intellivue.CO2, Time, t2pco2)
    
    bf_pco2 = df[df$t2pco2 < 0,] %>% arrange(desc(t2pco2))
    
    # pco2_dttm과 가장 가까운 시점의 데이터 추출
    if (nrow(bf_pco2) == 0){
      point = df[df$t2pco2 == min(df$t2pco2),]
    }else {
      point = bf_pco2[bf_pco2$t2pco2 == max(bf_pco2$t2pco2),] 
    }
    
    hn_etco2 <- rbind(hn_etco2, point)
  }
  etco2 <- rbind(etco2, hn_etco2)
}

# 과거 자료만 사용
etco2 %<>% dplyr::filter(t2pco2 < 0)
write.csv(etco2, file = "etco2_data.csv", row.names = FALSE)


##### data join
join_df = left_join(pco2_join, etco2, by = c("hn","pco2_dttm"))
View(join_df)

# remove missing data
join_df %<>% na.omit() %>% dplyr::select(-pco2.y,-Time,-t2pco2)
names(join_df)[3] <- "pco2"
names(join_df)[ncol(join_df)] <- "etco2"

write.csv(join_df, file = "pco2_etco2.csv", row.names = FALSE)








