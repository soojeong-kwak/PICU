##### file load
fp = "C:/Users/user/Desktop/PICU/PCO2/0 Data/"
sp = "C:/Users/user/Desktop/PICU/PCO2/2 Works/"

source(paste0(sp, "99_packageList.R"))

##### 1) exclude 0 row
picu_files_set =
  list.files('C:/Users/user/Desktop/PICU/PCO2/0 Data/samples_exclude_0_row',
             full.names = T) %>%
  str_subset('csv$')

# rows number each file
fn_rows <- tibble()
for (i in seq_along(picu_files_set)) {
  df <- read_csv(picu_files_set[i], show_col_types = FALSE)
  n  <- nrow(df)
  
  sub <-
    fn %>% filter(hn == unique(df$hn), filename == unique(df$filename))
  sub$rows <- n
  sub$csvnames <- basename(picu_files_set[i])
  
  fn_rows <- rbind(fn_rows, sub)
}

write.csv(fn_rows, file = "rid_filename_rows.csv", row.names = FALSE)


##### 2) data filtering
# duplicates / missing values
clinic <-
  unique(na.omit(htwt)) %>% group_by(rid) %>% summarise(wt = round(mean(bwt), 2), ht = round(mean(ht), 2))

# Age(month) < 18
demo <- admission %>%
  mutate(adm_age = adm_age_day / 365, adm_age_month = adm_age_day / 365 *
           12) %>%
  filter(adm_age < 18)
demo <- na.omit(demo)

# PaO2 >= 40
abga <- lab %>% filter(po2 >= 40)
abga <- abga[, colSums(is.na(abga)) < nrow(abga)]


##### 3) data join
db <- left_join(abga, clinic)
db <- na.omit(db)

age_list <-
  demo %>% group_by(rid) %>% summarise(adm_age_day = min(adm_age_day)) %>% left_join(demo[, c(1, 4)])
db2 <- tibble()

for (i in 1:nrow(age_list)) {
  db_sub <- db %>% filter(rid == age_list$rid[i])
  demo_sub <- demo %>% filter(rid == age_list$rid[i])
  
  # age days+n
  start_dt <- ymd(as_date(min(db_sub$lab_dt)))
  diff_dt <- ymd(as_date(db_sub$lab_dt)) - start_dt
  db_sub$adm_age_day <-
    min(demo_sub$adm_age_day) + as.numeric(diff_dt)
  db_sub$adm_age_month <- round(db_sub$adm_age_day / 365 * 12, 2)
  
  # merging
  db2 <- rbind(db2, subset(db_sub, select = -c(adm_age_day)))
}

anl_db <- unique(db2)
save(anl_db, file = "patient_lab_info.RData")


# collected data (sample_df_xxxx.csv)
fn_anl <- fn_rows %>% filter(hn %in% anl_db$rid)
View(fn_anl)

n = length(unique(fn_anl$hn)) # number of patients
measurement = nrow(fn_anl)    # total measurements from patients

print(c(n, measurement))

# train/test split 7:3 ratio
# hn_list = unique(fn_anl$hn)
# train_hn = sample(hn_list, size = 40)
# test_hn  = hn_list[which(!hn_list %in% train_hn)]
#
# train_file = fn_anl %>% filter(hn %in% train_hn)
# test_file  = fn_anl %>% filter(hn %in% test_hn)
#
# print(c(length(train_hn),nrow(train_file)))
# print(c(length(test_hn),nrow(test_file)))
