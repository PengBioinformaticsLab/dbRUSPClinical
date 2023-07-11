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
plotConCategoricalNoStrBoxAndViolin <- function(var_1, var_2){
  

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_violin(scale = "area",adjust = 4) +
    geom_boxplot() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


#Function to generate a violin plot with one continuous variable and one categorical variable and no stratification variable
plotConCategoricalNoStrViolin <- function(var_1, var_2){
  

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_violin(scale = "area",adjust = 2) +
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
plotConCategoricalOneStrBoxAndViolin <- function(var_1, var_2,str){
  
  if(str == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(str == "TPN"){
    sample_info <- subset(sample_info, TPN != "Unknown")
  }

  
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2),color = get(str)))+
    geom_violin(scale = "area",adjust = 2) +
    geom_boxplot() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    scale_color_discrete(name = variable_info$varShow[variable_info$variables==str])
  
  return(ccaplot)
}


#Function to generate a box plot with one continuous variable and one categorical variable and two stratification variables
plotConCategoricalTwoStrBoxAndViolin <- function(var_1, var_2,str_1,str_2){
  
  if(str_1 == "race_major" || str_2 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(str_1 == "TPN" || str_2 == "TPN" ){
    sample_info <- subset(sample_info, TPN != "Unknown")
  }

  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2),color = get(str_1)))+
    geom_violin(scale = "area",adjust = 2) + 
    geom_boxplot() +
    facet_wrap(~get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1])
  
  return(ccaplot)
}


#Function to generate a box plot with one continuous variable and one categorical variable and two stratification variables
plotConCategoricalTwoStrBoxAndViolinAlt <- function(var_1, var_2,str_1,str_2){
  
  if(str_1 == "race_major" || str_2 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(str_1 == "TPN" || str_2 == "TPN" ){
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2),color = get(str_2)))+
    geom_violin(scale = "area",adjust = 2) + 
    geom_boxplot() +
    facet_wrap(~get(str_1)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
    scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_2])
  
  return(ccaplot)
}
