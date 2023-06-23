
source("global.R")

#sample_info <- data.frame(sample_info)

#variables <- colnames(sample_info)[-1]

#continuousVariables <- c("Age_hr", "BW", "GA", "Year","Mom_age")

categoricalVariables <- c("Age_group", "BW_group", "GA_group", "sex", "TPN", "race_major", "race_detail", "flag_race_multi", "RBC_transfusion")



variable_info <- data.frame(
  variables = c(
    "Age_hr", "Age_group", "BW", "BW_group",
    "GA", "GA_group", "sex", "TPN",
    "race_major", "race_detail", "RBC_transfusion", "Mom_age"),
  varShow = c(
    "Age at blood collection (Hour)", "Age at blood collection (Group)", "Birth Weight (g)", "Birth Weight (Group)",
    "Gestational age (weeks)", "Gestational age (Group)", "Sex", "Total Parenteral Nutrition",
    "Ethnicity (Major)", "Ethnicity (Detailed)", "Blood transfusion", "Maternal age"
  ),
  varType = c(
    "continuous", "categorical",  "continuous", "categorical",
    "continuous", "categorical", "categorical", "categorical",
    "categorical", "categorical", "categorical", "continuous"
  )
)


