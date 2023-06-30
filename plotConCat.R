#R script to generate plots when one variable is continuous and the other is categorical


#Load the required packages
library(ggplot2)

#Function to generate a column plot with one continuous variable and one categorical variable and no stratification variable
plotConCategoricalNoStrCol <- function(var_1, var_2){


  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_col() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


#Function to generate a box plot with one continuous variable and one categorical variable and no stratification variable
plotConCategoricalNoStrBox <- function(var_1, var_2){
  

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_boxplot() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


#Function to generate a violin plot with one continuous variable and one categorical variable and no stratification variable
plotConCategoricalNoStrViolin <- function(var_1, var_2){
  

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_violin(scale = "area") +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


#Function to generate a dpt plot with one continuous variable and one categorical variable and no stratification variables
plotConCategoricalNoStrDot <- function(var_1, var_2){
  
  if(variable_info$varType[variable_info$variables == var_1] == "categorical"){
    

    ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
      geom_dotplot(binwidth = 1.5, stackdir = "center",binaxis = "y") +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
    
  }else{
    

    ccaplot <- ggplot(sample_info,aes(get(var_2),get(var_1)))+
      geom_dotplot(binwidth = 1.5, stackdir = "center",binaxis = "y") +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_2], y = variable_info$varShow[variable_info$variables == var_1])
    
  }
  
  
  return(ccaplot)
}


#Function to generate a box plot with one continuous variable and one categorical variable and one stratification variable
plotConCategoricalOneStrBox <- function(var_1, var_2,str){
  

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_boxplot() +
    facet_wrap(~get(str)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


#Function to generate a box plot with one continuous variable and one categorical variable and two stratification variables
plotConCategoricalTwoStrBox <- function(var_1, var_2,str_1,str_2){
  

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_boxplot() +
    facet_wrap(~get(str_1)+get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}
