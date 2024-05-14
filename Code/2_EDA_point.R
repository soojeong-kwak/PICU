##### file load
fp = "C:/Users/user/Desktop/PICU/PCO2/0 Data/"
sp = "C:/Users/user/Desktop/PICU/PCO2/2 Works/"

source(paste0(sp, "99_packageList.R"))

base_df = read_csv(paste0(sp, "pco2_etco2.csv"), show_col_types = FALSE)
base_df %<>% arrange(pco2_dttm)


##### create derived variables
base_df$sex <- factor(base_df$sex, levels = c("F", "M"))
base_df$pco2_status <- case_when(
  base_df$pco2 >= 35 & base_df$pco2 <= 45 ~ "Normal",
  base_df$pco2 < 35 ~ "R.Alkalosis",
  base_df$pco2 > 45 ~ "R.Acidosis"
)
base_df$pco2_status <- factor(base_df$pco2_status, levels = c("R.Alkalosis", "Normal", "R.Acidosis"))
write.csv(base_df, file = "base_dataset.csv", row.names = FALSE)

# palete
my_colors = brewer.pal(8, "Set2") 

# line plot by pco2_status
at = seq(1,nrow(base_df),by=100)
label = str_wrap(base_df$pco2_dttm, 10)[at]
clplot(1:nrow(base_df), base_df$etco2, main="", lwd=1, levels=c(35, 45), col=my_colors,
       showcuts=T , bty="n", ylim = range(base_df$etco2), xlab = "PaCO2_dttm", ylab = "EtCO2", xaxt="n")
axis(1, at=at, labels = label, cex.axis=.7)
legend("topleft", legend = c("Respiratory Alkalosis", "Normal", "Respiratory Acidosis"), col=my_colors, lty=1, cex=.8, bty = "n")

# box plot by pco2_status
boxplot(base_df$pco2)
boxplot(pco2 ~ pco2_status, data = base_df, col=my_colors, pch=20, xlab = "PaCO2 status", ylab = "PaCO2", main = "boxplot")

# density plot
plot(density(base_df$pco2), main = "PaCO2")

# 이상치 제거
outlier = boxplot(base_df$pco2)$out
base_df %<>% dplyr::filter(!pco2 %in% outlier)

par(mfrow = c(1,3))
plot(density(subset(base_df, pco2_status == "R.Alkalosis")$pco2), main = "Respiratory Alkalosis")
plot(density(subset(base_df, pco2_status == "Normal")$pco2), main = "Normal")
plot(density(subset(base_df, pco2_status == "R.Acidosis")$pco2), main = "Respiratory Acidosis")
par(mfrow = c(1,1))

##### baseline characteristics
ztable(mytable(base_df))


##### test for non-normally distribution
# Kruskal-Wallis test
kruskal.test(pco2 ~ pco2_status, data = base_df)


##### Correlation Analysis
features = names(base_df)[3:ncol(base_df)]

# plot
ggpairs(base_df[,features], 
        upper = list(continuous = wrap(ggally_cor, method = "spearman")))

# Spearman's correlation
base_df$sex <- ifelse(base_df$sex == "F", 1, 0)
base_df$pco2_status <- ifelse(base_df$pco2_status == "Normal", 0, 
                              ifelse(base_df$pco2_status == "R.Alkalosis", -1, 1))

cr = cor(base_df[,features], method = "spearman")
with(base_df[,features], cor.test(pco2, NIBP_MEAN))

# GLM (all features)
l = lm(pco2 ~ . , data = base_df[,features])
summary(l)
ztable(l)

# Multicollinearity (VIF)
ztable(vif(l))

f = features[-7]
l = glm(pco2 ~. , data = base_df[,f])  # remove NIBP_MEAN
ztable(vif(l))

f = f[-8]
l = glm(pco2 ~. , data = base_df[,f])  # remove ht
ztable(vif(l))

# GLM (R-squared)
l = glm(pco2 ~., data = base_df[,f])
summary(l)
ztable(l)

g_list = list()
for (i in f[-c(1,9,11)]) { # except categorical variables features
  
  g <- base_df %>% 
    ggplot(aes(x = .data[[i]], y = pco2)) +
    geom_smooth(method = "glm", level = 0.95, se = TRUE) +
    geom_point(size = .8) +
    stat_poly_eq(use_label(c("eq", "R2", "p"))) + # "R2.confint"
    theme_bw() +
    ylim(0,160) +
    ylab("PaCO2")
  
  g_list[[i]] <- g
}
grid.arrange(grobs = g_list, ncol = 3)





