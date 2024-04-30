##### file load
fp = "C:/Users/user/Desktop/PICU/PCO2/0 Data/"
sp = "C:/Users/user/Desktop/PICU/PCO2/2 Works/"

source(paste0(sp, "99_packageList.R"))

fn_rows = read_csv(paste0(fp, "rid_filename_rows.csv"), show_col_types = FALSE)
fn_overlap = tibble()

for (f in unique(fn_rows$hn)) {
  
  tmp = fn_rows %>% filter(hn == f)
  
  for (i in 1:nrow(tmp)) {
    
    nm = tmp[i,]
    df = read_csv(paste0(fp, "samples_exclude_0_row/",nm$csvnames), show_col_types = FALSE)
    hn = unique(df$pco2_dttm)
    
    for (j in seq_along(hn)) {
      
      ex = df[which(df$pco2_dttm == hn[j]),]
      
      nm$pco2_dttm = hn[j]
      nm$pco2 = unique(ex$pco2)
      nm$overlap = ifelse(length(hn) > 1, T, F)
      
      fn_overlap = rbind(fn_overlap, nm)
    }
  }
}
save(fn_overlap, file = "sample_df_overlap_TF.RData")

ex = fn_overlap %>% filter(hn %in% fn_anl$hn)
ex_T = ex %>% filter(overlap == T)


##### 1) 혈액검사 간격이 좁은 경우, 전후 2시간(총 4시간)에 겹치는 구간 발생 => 각각의 독립적인 값으로 사용

for (f in unique(ex_T$hn)) {
  
  tmp = ex_T %>% filter(hn == f)
  
  for (i in 1:nrow(tmp)) {
    
    nm = tmp[i,]
    df = read_csv(paste0(fp, "samples_exclude_0_row/",nm$csvnames), show_col_types = FALSE)
    hn = unique(df$pco2_dttm)
    
    for (j in seq_along(hn)) {
      
      ex = df[which(df$pco2_dttm == hn[j]),]
      write.csv(ex, file = paste0(gsub(".csv", "", nm$csvnames),"(",j,").csv"), row.names = F)
    }
  }
}

rm_file <- ex_T$csvnames
file.remove(rm_file)


##### 2)  key(hn, pco2_dttm)이 여러 개의 file로 나뉘어져 있거나 하나의 file에 2개 이상의 key 존재 
#####     => pco2_dttm이 같은 파일끼리 merging

uniq_file = list.files(path = paste0(fp, "samples_exclude_0_row/"), pattern = "\\(")
ex_F = ex %>% filter(overlap == F)

for (i in seq_along(uniq_file)) {
  
  df = read_csv(paste0(fp, "samples_exclude_0_row/", uniq_file[i]), show_col_types = FALSE)
  
  df.summary = tibble(
    hn = df$hn,
    filename = df$filename,
    rows = nrow(df),
    csvnames = uniq_file[i],
    pco2_dttm = df$pco2_dttm,
    pco2 = df$pco2,
    overlap = FALSE
  )
  
  df.summary %<>% unique
  ex_F = rbind(ex_F, df.summary) 
}

merge_base = unique(ex_F[c("hn", "pco2_dttm","pco2")]) # key value

for (i in 1:nrow(merge_base)) {
  
  tmp = ex_F %>% filter(
      hn == merge_base$hn[i] &
      pco2_dttm == merge_base$pco2_dttm[i] &
      pco2 == merge_base$pco2[i]
  )
  
  j = 1; d = tibble()
  while (j <= nrow(tmp)) {
    df = read_csv(paste0(fp, "samples_exclude_0_row/", tmp$csvnames[j]), show_col_types = FALSE)
    df = df[,which(str_detect(names(df), "[a-z]+") == T)]
    d = rbind(d, df)
    
    j = j + 1
  }
  d %<>% arrange(t2pco2)
  print(nrow(d) == sum(tmp$rows))
  write.csv(d, file = paste0(gsub(".csv", "", tmp$csvnames[1]),"_merging.csv"), row.names = F)
}


##### 3) 최종 분석 대상이 되는 data file만 폴더에 저장

merge_file = list.files(path = paste0(fp, "samples_exclude_0_row/"), pattern = "merging")
rm_file = list.files(path = paste0(fp, "samples_exclude_0_row/"))

rm_file <- rm_file[which(!rm_file %in% merge_file)]
file.remove(rm_file)

# summary info
merge_file_re = gsub("_merging", "", merge_file)
fin_file_info = ex_F %>% filter(csvnames %in% merge_file_re) %>% select(-filename,-rows,-overlap)

fin_file_info$csvnames = paste0(gsub(".csv", "", fin_file_info$csvnames),"_merging.csv")
fin_file_info$rows = 0
for(i in 1:nrow(fin_file_info)){
  tmp = read_csv(paste0(fp, "samples_exclude_0_row/", fin_file_info$csvnames[i]), show_col_types = FALSE)
  fin_file_info$rows[i] = nrow(tmp)
}

write.csv(fin_file_info, file = "samples_df_info.csv", row.names = F)



