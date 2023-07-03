#R script to generate the plots when both the variables are categorical

#Load the required packages
library(ggplot2)
library(patchwork)

#Function to generate a bar plot with two categorical variables and no stratification variables
plotBothCategoricalNoStr <- function(var_1, var_2){
  


  caplotNoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Count") +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  

  
  return(caplotNoStr)
}

#Function to generate a count plot with two categorical variables and no stratification variables
plotBothCategoricalCount <- function(var_1,var_2){
  

  caplotCount <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
    geom_count() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  
  
  
  return(caplotCount)

}


#Function to generate a jitter plot with two categorical variables and no stratification variables
plotBothCategoricalJitter <- function(var_1,var_2){
  
  caplotJitter <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
    geom_jitter() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  
  
  
  return(caplotJitter)
  
}


#Function to generate a mosaic plot with two categorical variables and no stratification variables
plotBothCategoricalMosaic <- function(var_1, var_2){
  
  

  caplotmosaic <- mosaicplot(get(var_2)~get(var_1),data = sample_info, 
                             main = "Mosaic plot",
                             xlab = variable_info$varShow[variable_info$variables==var_2], 
                             ylab = variable_info$varShow[variable_info$variables==var_1],
                             color = TRUE)
  
  
  
  
  return(caplotmosaic)
}


#Function to generate a bar plot with two categorical variables and one stratification variable
plotBothCategoricalOneStrfacet <- function(var_1,var_2,str_1){
  
  if(str_1 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # bar plot using ggplot2
  caplotOneStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    facet_wrap(~get(str_1)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Count") +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  
  
  
  return(caplotOneStr)
}


#Function to generate a bar plot with two categorical variables and two stratification variables
plotBothCategoricalTwoStrfacet <- function(var_1,var_2,str_1,str_2){
  
  # bar plot using ggplot2
  caplotTwoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    facet_wrap(~get(str_1)+get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Count") +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  
  
  
  return(caplotTwoStr)
}


