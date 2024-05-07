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


##### data join
join_1 = tibble()

for (i in 1:nrow(pco2_data)){
  
  htwt_sub = subset(na.omit(htwt), rid == pco2_data$hn[i])
  pco2_sub = subset(pco2_data, hn == pco2_data$hn[i])
  pco2_sub %<>%
    mutate(wt = NA, ht = NA)
  
  for (j in 1:nrow(pco2_sub)){
    
    time_diff = abs(htwt_sub$measure_dt - pco2_sub$pco2_dttm[j])
    idx = which(min(time_diff) == time_diff)
    
    pco2_sub[j, c('wt', 'ht')] <- htwt_sub[idx, c("bwt", "ht")]
  }
  
  join_1 = rbind(join_1, pco2_sub) 
}
join_1 = unique(join_1)


join_2 = tibble()

for (i in 1:nrow(join_1)){
  
  adm_sub = subset(na.omit(admission), rid == join_1$hn[i])
  pco2_sub = subset(join_1, hn == join_1$hn[i])
  
  pco2_sub %<>%
    mutate(age_month = NA, sex = NA)
  
  for (j in 1:nrow(pco2_sub)){
    
    idx = which(pco2_sub$pco2_dttm[j] >= adm_sub$adm_dt & pco2_sub$pco2_dttm[j] <= adm_sub$dc_dt)
    
    if(is_empty(idx)){
      
      time_diff = abs(adm_sub$adm_dt - pco2_sub$pco2_dttm[j])
      idx = which(min(time_diff) == time_diff)
      
      pco2_sub$age_month[j] <- round(adm_sub$adm_age_day[idx]/30, 1)
      pco2_sub$sex[j] <- adm_sub$sex[idx]
      
    }else {
      
      pco2_sub$age_month[j] <- round(adm_sub$adm_age_day[idx]/30, 1)
      pco2_sub$sex[j] <- adm_sub$sex[idx]
    }
  }
  join_2 = rbind(join_2, pco2_sub) 
}
join_2 = unique(join_2)

# Base 분석 데이터셋
pco2_join = join_2[,c("hn", "pco2_dttm", "pco2", "PLETH_SAT_O2", "ECG_HR", "RR", "NIBP_SYS", "NIBP_DIA", "NIBP_MEAN",
                      "t2pco2", "wt", "ht", "age_month", "sex")]
write.csv(pco2_join, file = "pco2_join.csv", row.names = F)






