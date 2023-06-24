library(ggplot2)
library(patchwork)

plotBothContinuousNoStr <- function(var_1, var_2){
  
  # Create data vectors
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "pairwise.complete.obs")
  
  # Plot the correlation using ggplot2
  cplot <- ggplot(sample_info, aes(get(var_1),get(var_2))) +
    geom_point() +
    geom_smooth() +
    theme_light() +
    labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  
  return(cplot)
}

plotBothContinuousOneStrColor <- function(var_1,var_2,str_1){
  
  # Create data vectors
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "pairwise.complete.obs")
  
  cplot <- ggplot(sample_info, aes(get(var_1),get(var_2),color = get(str_1))) +
    geom_point() +
    geom_smooth() +
    theme_light() +
    labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1])
  
  return(cplot)
}



plotBothContinuousOneStrFacet <- function(var_1,var_2,str_1){
  
  # Create data vectors
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "pairwise.complete.obs")
  
  cplot <- ggplot(sample_info,aes(get(var_1), get(var_2))) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~get(str_1)) +
    theme_light() +
    labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(cplot)
}

#ggplot(sample_info, aes(GA,BW,colour = race_major)) +
#geom_smooth(method = lm) +
#facet_wrap(~sex) +
#theme_light() +
#labs(title = paste("Correlation:", cor(sample_info$GA,sample_info$BW, use = "complete.obs")))


#ggplot(sample_info, aes(Age_group,BW_group,color = race_major)) +
# geom_jitter(height = 2, width = 2) +
#facet_wrap(~sex)
#theme_light()

#ggplot(sample_info, aes(Age_group,BW_group,color = race_major)) +
# geom_count() +
#facet_wrap(~sex) +
#theme_light()


#ggplot(sample_info, aes(Age_hr,BW_group,color = race_major)) +
#geom_col() +
#facet_wrap(~sex) +
#theme_light()


#ggplot(sample_info, aes(BW_group,Year)) +
# geom_boxplot() +
#geom_point() +
#facet_wrap(~sex) +
#theme_light()

#ggplot(sample_info, aes(BW_group,GA,color = race_major)) +
#geom_dotplot() +
#facet_wrap(~sex) +
#theme_light()

#ggplot(sample_info, aes(BW_group,Year,color = race_major)) +
#geom_violin(scale = "area") +
#facet_wrap(~sex) +
#theme_light()
