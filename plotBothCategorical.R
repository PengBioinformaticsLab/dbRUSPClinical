library(ggplot2)
library(patchwork)

plotBothCategoricalNoStr <- function(var_1, var_2){
  

  # bar plot using ggplot2
  caplotNoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  

  
  return(caplotNoStr)
}

plotBothCategoricalCount <- function(var_1,var_2){
  
  # bar plot using ggplot2
  caplotCount <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
    geom_count() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  
  
  
  return(caplotCount)

}

plotBothCategoricalJitter <- function(var_1,var_2){
  
  # bar plot using ggplot2
  caplotJitter <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
    geom_jitter() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  
  
  
  return(caplotJitter)
  
}


plotBothCategoricalMosaic <- function(var_1, var_2){
  
  
  # bar plot using ggplot2
  caplotmosaic <- mosaicplot(get(var_2)~get(var_1),data = sample_info, 
                             main = "Mosaic plot",
                             xlab = variable_info$varShow[variable_info$variables==var_2], 
                             ylab = variable_info$varShow[variable_info$variables==var_1],
                             color = TRUE)
  
  
  
  
  return(caplotmosaic)
}



plotBothCategoricalOneStrfacet <- function(var_1,var_2,str_1){
  
  # bar plot using ggplot2
  caplotOneStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    facet_wrap(~get(str_1)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  
  
  
  return(caplotOneStr)
}


plotBothCategoricalTwoStrfacet <- function(var_1,var_2,str_1,str_2){
  
  # bar plot using ggplot2
  caplotTwoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    facet_wrap(~get(str_1)+get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  
  
  
  return(caplotTwoStr)
}


# plotBothContinuousOneStrColor <- function(var_1,var_2,str_1){
#   
#   # Create data vectors
#   x <- sample_info[[var_1]]  # First variable
#   y <- sample_info[[var_2]]  # Second variable
#   
#   # Calculate the correlation coefficient, ignoring missing values
#   cor_coef <- cor(x, y, use = "pairwise.complete.obs")
#   
#   cplot <- ggplot(sample_info, aes(get(var_1),get(var_2),color = get(str_1))) +
#     geom_point() +
#     geom_smooth() +
#     theme_light() +
#     labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
#     scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1])
#   
#   return(cplot)
# }
# 
# 
# 
# plotBothContinuousOneStrFacet <- function(var_1,var_2,str_1){
#   
#   # Create data vectors
#   x <- sample_info[[var_1]]  # First variable
#   y <- sample_info[[var_2]]  # Second variable
#   
#   # Calculate the correlation coefficient, ignoring missing values
#   cor_coef <- cor(x, y, use = "pairwise.complete.obs")
#   
#   cplot <- ggplot(sample_info,aes(get(var_1), get(var_2))) +
#     geom_point() +
#     geom_smooth() +
#     facet_wrap(~get(str_1)) +
#     theme_light() +
#     labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
#   
#   return(cplot)
# }
# 
# plotBothContinuoustwostr <- function(var_1,var_2,str_1,str_2){
#   
#   # Create data vectors
#   x <- sample_info[[var_1]]  # First variable
#   y <- sample_info[[var_2]]  # Second variable
#   
#   # Calculate the correlation coefficient, ignoring missing values
#   cor_coef <- cor(x, y, use = "pairwise.complete.obs")
#   
#   cplot <- ggplot(sample_info,aes(get(var_1), get(var_2),color = get(str_1))) +
#     geom_point() +
#     geom_smooth() +
#     facet_wrap(~get(str_2)) +
#     theme_light() +
#     labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
#     scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1])
#   
#   return(cplot)
#   
# }
# 
# 
# plotBothContinuoustwostralt <- function(var_1,var_2,str_1,str_2){
#   
#   # Create data vectors
#   x <- sample_info[[var_1]]  # First variable
#   y <- sample_info[[var_2]]  # Second variable
#   
#   # Calculate the correlation coefficient, ignoring missing values
#   cor_coef <- cor(x, y, use = "pairwise.complete.obs")
#   
#   cplot <- ggplot(sample_info,aes(get(var_1), get(var_2),color = get(str_2))) +
#     geom_point() +
#     geom_smooth() +
#     facet_wrap(~get(str_1)) +
#     theme_light() +
#     labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
#     scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_2])
#   
#   return(cplot)
#   
# }
# 
# plotBothContinuoustwostrfacet <- function(var_1,var_2,str_1,str_2){
#   
#   # Create data vectors
#   x <- sample_info[[var_1]]  # First variable
#   y <- sample_info[[var_2]]  # Second variable
#   
#   # Calculate the correlation coefficient, ignoring missing values
#   cor_coef <- cor(x, y, use = "pairwise.complete.obs")
#   
#   cplot <- ggplot(sample_info,aes(get(var_1), get(var_2))) +
#     geom_point() +
#     geom_smooth() +
#     facet_wrap(~get(str_1) + get(str_2)) +
#     theme_light() +
#     labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
#   
#   return(cplot)
#   
# }
# 
# #ggplot(sample_info, aes(GA,BW,colour = race_major)) +
# #geom_smooth(method = lm) +
# #facet_wrap(~sex) +
# #theme_light() +
# #labs(title = paste("Correlation:", cor(sample_info$GA,sample_info$BW, use = "complete.obs")))
# 
# 
# #ggplot(sample_info, aes(Age_group,BW_group,color = race_major)) +
# # geom_jitter(height = 2, width = 2) +
# #facet_wrap(~sex)
# #theme_light()
# 
# #ggplot(sample_info, aes(Age_group,BW_group,color = race_major)) +
# # geom_count() +
# #facet_wrap(~sex) +
# #theme_light()
# 
# 
# #ggplot(sample_info, aes(Age_hr,BW_group,color = race_major)) +
# #geom_col() +
# #facet_wrap(~sex) +
# #theme_light()
# 
# 
# #ggplot(sample_info, aes(BW_group,Year)) +
# # geom_boxplot() +
# #geom_point() +
# #facet_wrap(~sex) +
# #theme_light()
# 
# #ggplot(sample_info, aes(BW_group,GA,color = race_major)) +
# #geom_dotplot() +
# #facet_wrap(~sex) +
# #theme_light()
# 
# #ggplot(sample_info, aes(BW_group,Year,color = race_major)) +
# #geom_violin(scale = "area") +
# #facet_wrap(~sex) +
# #theme_light()
