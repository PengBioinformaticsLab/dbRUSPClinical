library(ggplot2)

plotConCategoricalNoStrCol <- function(var_1, var_2){

  # Plot the correlation using ggplot2
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_col() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}

plotConCategoricalNoStrBox <- function(var_1, var_2){
  
  # Plot the correlation using ggplot2
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_boxplot() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}

plotConCategoricalNoStrViolin <- function(var_1, var_2){
  
  # Plot the correlation using ggplot2
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_violin(scale = "area") +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


plotConCategoricalNoStrDot <- function(var_1, var_2){
  
  if(variable_info$varType[variable_info$variables == var_1] == "categorical"){
    
    # Plot the correlation using ggplot2
    ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
      geom_dotplot(binwidth = 1.5, stackdir = "center",binaxis = "y") +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
    
  }else{
    
    # Plot the correlation using ggplot2
    ccaplot <- ggplot(sample_info,aes(get(var_2),get(var_1)))+
      geom_dotplot(binwidth = 1.5, stackdir = "center",binaxis = "y") +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_2], y = variable_info$varShow[variable_info$variables == var_1])
    
  }
  
  
  return(ccaplot)
}


plotConCategoricalOneStrBox <- function(var_1, var_2,str){
  
  # Plot the correlation using ggplot2
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_boxplot() +
    facet_wrap(~get(str)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}


plotConCategoricalTwoStrBox <- function(var_1, var_2,str_1,str_2){
  
  # Plot the correlation using ggplot2
  ccaplot <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
    geom_boxplot() +
    facet_wrap(~get(str_1)+get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  return(ccaplot)
}
