load("~/Downloads/sampleInfo.RData")

sample_info <- data.frame(sample_info)

variables <- colnames(sample_info)[-1]

continuousVariables <- c("Age_hr", "BW", "GA", "Year","Mom_age")

categoricalVariables <- c("Age_group", "BW_group", "GA_group", "sex", "TPN", "race_major", "race_detail", "flag_race_multi", "RBC_transfusion")




