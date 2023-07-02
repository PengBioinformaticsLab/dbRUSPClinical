#R script to generate plots when both the variables are continuous

#load the required packages
library(ggplot2)
library(patchwork)

#Function to generate a correlation plot with two continuous variables and no stratification variables
plotBothContinuousNoStr <- function(var_1, var_2,dots,xscale,yscale){
  
  if(dots){
    
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
      labs(title = paste("Correlation:", cor_coef), 
           x = variable_info$varShow[variable_info$variables == var_1], 
           y = variable_info$varShow[variable_info$variables == var_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }else{
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Plot the correlation using ggplot2
    cplot <- ggplot(sample_info, aes(get(var_1),get(var_2))) +
      geom_smooth() +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), 
           x = variable_info$varShow[variable_info$variables == var_1], 
           y = variable_info$varShow[variable_info$variables == var_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    
    return(cplot)
  }
}


#Function to generate a correlation plot with two continuous variables and no stratification variables and no dots
plotBothContinuousNoStrNoDots <- function(var_1, var_2){
  
  # Create data vectors
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "pairwise.complete.obs")
  
  # Plot the correlation using ggplot2
  cplot <- ggplot(sample_info, aes(get(var_1),get(var_2))) +
    geom_smooth() +
    theme_light() +
    labs(title = paste("Correlation:", cor_coef), 
         x = variable_info$varShow[variable_info$variables == var_1], 
         y = variable_info$varShow[variable_info$variables == var_2]) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  return(cplot)
}


#Function to generate a correlation plot with two continuous variables and one stratification variable
plotBothContinuousOneStrColor <- function(var_1,var_2,str_1,dots,xscale,yscale){
  
  if(str_1 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(dots){
    
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
      scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }else{
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info, aes(get(var_1),get(var_2),color = get(str_1))) +
      geom_smooth() +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }
}


#Function to generate a correlation plot with two continuous variables and no stratification variable
plotBothContinuousOneStrFacet <- function(var_1,var_2,str_1,dots,xscale,yscale){
  
  if(str_1 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(dots){
    
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
      labs(title = paste("Correlation:", cor_coef), 
           x = variable_info$varShow[variable_info$variables == var_1], 
           y = variable_info$varShow[variable_info$variables == var_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }else{
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2))) +
      geom_smooth() +
      facet_wrap(~get(str_1)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), 
           x = variable_info$varShow[variable_info$variables == var_1], 
           y = variable_info$varShow[variable_info$variables == var_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
  }
}


#Function to generate a correlation plot with two continuous variables and two stratification variables
plotBothContinuoustwostr <- function(var_1,var_2,str_1,str_2,dots,xscale,yscale){
  
  if(str_1 == "race_major" || str_2 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(dots){
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2),color = get(str_1))) +
      geom_point() +
      geom_smooth() +
      facet_wrap(~get(str_2)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }else{
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2),color = get(str_1))) +
      geom_smooth() +
      facet_wrap(~get(str_2)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_1]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }
  
}


#Function to generate a correlation plot with two continuous variables and two stratification variables
plotBothContinuoustwostralt <- function(var_1,var_2,str_1,str_2,dots,xscale,yscale){
  
  if(str_1 == "race_major" || str_2 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(dots){
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2),color = get(str_2))) +
      geom_point() +
      geom_smooth() +
      facet_wrap(~get(str_1)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }else{
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2),color = get(str_2))) +
      geom_smooth() +
      facet_wrap(~get(str_1)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_color_discrete(name = variable_info$varShow[variable_info$variables==str_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }
  
  
  
}


#Function to generate a correlation plot with two continuous variables and two stratification variables
plotBothContinuoustwostrfacet <- function(var_1,var_2,str_1,str_2,dots,xscale,yscale){
  
  if(str_1 == "race_major" || str_2 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(dots){
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2))) +
      geom_point() +
      geom_smooth() +
      facet_wrap(~get(str_1) + get(str_2)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), 
           x = variable_info$varShow[variable_info$variables == var_1], 
           y = variable_info$varShow[variable_info$variables == var_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }else{
    
    # Create data vectors
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    cplot <- ggplot(sample_info,aes(get(var_1), get(var_2))) +
      geom_smooth() +
      facet_wrap(~get(str_1) + get(str_2)) +
      theme_light() +
      labs(title = paste("Correlation:", cor_coef), 
           x = variable_info$varShow[variable_info$variables == var_1], 
           y = variable_info$varShow[variable_info$variables == var_2]) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if(xscale && !yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10")
      
    }else if (!xscale && yscale){
      
      cplot <- cplot + scale_y_continuous(trans = "log10")
      
    }else if (xscale && yscale){
      
      cplot <- cplot + scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      
    }else{
      
      cplot <- cplot
    }
    
    return(cplot)
    
  }
  
}
