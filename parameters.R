#R script to store information about data


#Data frame contaning information about the data
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

#List of categorical variables
categoricalVariables <- variable_info$varShow[as.numeric(rownames(variable_info[variable_info$varType == "categorical", ]))]


