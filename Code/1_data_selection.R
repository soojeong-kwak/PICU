##### file load
fp = "C:/Users/user/Desktop/PICU/PCO2/0 Data/"
sp = "C:/Users/user/Desktop/PICU/PCO2/2 Works/"

source(paste0(sp, "99_packageList.R"))
source(paste0(sp, "99_EDA_func.R"))

fin_file_info = read_csv(paste0(fp, "samples_exclude_0_row/samples_df_info.csv"), show_col_types = FALSE)


##### extract point data by hospital number
hn = unique(fin_file_info$hn)
pco2_data = tibble()

for (i in hn){
  
  subj = subset(fin_file_info, hn == i)
  
  for (j in 1:nrow(subj)){
    
    tmp = read_csv(paste0(fp, "samples_exclude_0_row/", subj$csvnames[j]), show_col_types = FALSE)
    
    bf_pco2 = tmp[tmp$t2pco2 < 0,] %>% arrange(desc(t2pco2))
    
    # pco2_dttm과 가장 가까운 시점의 데이터 추출
    if (nrow(bf_pco2) == 0){
      point = tmp[tmp$t2pco2 == min(tmp$t2pco2),]
    }else {
      point = bf_pco2[bf_pco2$t2pco2 == max(bf_pco2$t2pco2),] 
    }
    
    pco2_data = rbind(pco2_data, point)
  }
}

colnames(pco2_data) <- gsub("Intellivue.","", colnames(pco2_data))

# 과거 자료만 사용
pco2_data <- pco2_data[pco2_data$t2pco2 < 0,] %>% select(-filname) 
View(pco2_data)

write.csv(pco2_data, file = "pco2_data.csv", row.names = F)


# pco2_data %<>%
#   mutate(
#     t2pco2_mins = pco2_data$t2pco2 / 60,
#     t2pco2_hrs = pco2_data$t2pco2_mins / 60
#   ) 
# 
# pco2_data$t2pco2_mins <- ifelse(abs(pco2_data$t2pco2_mins) < 1, 0, pco2_data$t2pco2_mins)
# pco2_data$t2pco2_hrs <- ifelse(abs(pco2_data$t2pco2_mins) < 1, 0, pco2_data$t2pco2_hrs)
