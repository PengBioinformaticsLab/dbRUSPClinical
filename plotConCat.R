#R script to generate plots when one variable is continuous and the other is categorical


#Load the required packages
library(ggplot2)

# Function to generate a column plot with one continuous variable and one categorical variable and no stratification variable
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
plotConCategoricalNoStrCol <- function(var_1, var_2) {
  
  # Check if the variable is "race_detail" and remove rows with commas from the data
  if (var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Create the column plot using ggplot2
  ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
    geom_col() +                            # Use columns to represent the data
    theme_light() +                         # Use a light theme
    labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
         y = variable_info$varShow[variable_info$variables == var_2])   # Label for the y-axis
  
  return(ccaplot)  # Return the column plot
}



# Function to generate a box plot with one continuous variable and one categorical variable and no stratification variable
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
#v1_type - Type of variable one as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable one
#v2_type - Type of variable two as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable two
plotConCategoricalNoStrBoxAndViolin <- function(var_1, var_2,v1_type,v2_type) {
  
  # Check if the variable is "race_major" and remove rows with "OtherUnknown" value from the data
  if (var_1 == "race_major" || var_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if the variable is "race_detail" and remove rows with commas from the data
  if (var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Create the combined plot using ggplot2
  ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
    geom_violin(adjust = 10) +                  # Add violin plots with some width adjustment
    geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Add box plots with specified width and appearance
    theme_light() +                             # Use a light theme
    stat_summary(fun.y = mean, geom = "point",color="Blue") + #Add a data point to represent mean
    labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
         y = variable_info$varShow[variable_info$variables == var_2]) # Label for the y-axis
  
  #If-else block to add mean data points
  if(v2_type == "continuous"){
    ccaplot <- ccaplot + scale_x_discrete(labels = function(x) {
      group_means <- tapply(sample_info[[var_2]], sample_info[[var_1]], mean)
      sprintf("%s\nMean: %.2f", x, group_means[x])
    })
  }else{
    ccaplot <- ccaplot + scale_y_discrete(labels = function(x) {
      group_means <- tapply(sample_info[[var_1]], sample_info[[var_2]], mean)
      sprintf("%s\nMean: %.2f", x, group_means[x])
  })}
  
  
  return(ccaplot)  # Return the combined plot
}



# Function to generate a violin plot with one continuous variable and one categorical variable and no stratification variable
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
plotConCategoricalNoStrViolin <- function(var_1, var_2) {
  
  # Check if the variable is "race_detail" and remove rows with commas from the data
  if (var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Create the violin plot using ggplot2
  ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
    geom_violin(scale = "area", adjust = 2) +    # Add violin plots with area scaling and some width adjustment
    theme_light() +                             # Use a light theme
    labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
         y = variable_info$varShow[variable_info$variables == var_2])   # Label for the y-axis
  
  return(ccaplot)  # Return the violin plot
}



# Function to generate a dot plot with one continuous variable and one categorical variable and no stratification variables
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
plotConCategoricalNoStrDot <- function(var_1, var_2) {
  
  # Check if the variable is "race_detail" and remove rows with commas from the data
  if (var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Check if the variable is categorical or continuous, and create the dot plot accordingly
  if (variable_info$varType[variable_info$variables == var_1] == "categorical") {
    # If var_1 is categorical, use it on the x-axis and var_2 on the y-axis
    ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
      geom_dotplot(binwidth = 1.5, stackdir = "center", binaxis = "y") +  # Add dot plots with specified binwidth and stack direction
      theme_light() +                           # Use a light theme
      labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
           y = variable_info$varShow[variable_info$variables == var_2])   # Label for the y-axis
    
  } else {
    # If var_2 is categorical, use it on the x-axis and var_1 on the y-axis
    ccaplot <- ggplot(sample_info, aes(get(var_2), get(var_1))) +
      geom_dotplot(binwidth = 1.5, stackdir = "center", binaxis = "y") +  # Add dot plots with specified binwidth and stack direction
      theme_light() +                           # Use a light theme
      labs(x = variable_info$varShow[variable_info$variables == var_2],   # Label for the x-axis
           y = variable_info$varShow[variable_info$variables == var_1])   # Label for the y-axis
  }
  
  return(ccaplot)  # Return the dot plot
}



# Function to generate a box plot with one continuous variable, one categorical variable, and one stratification variable
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
#str - Any one stratification variable as selected by the user
#v1_type - Type of variable one as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable one
#v2_type - Type of variable two as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable two
plotConCategoricalOneStrBoxAndViolin <- function(var_1, var_2, str,v1_type,v2_type) {
  
  # Check if any of the variables or the stratification variable is "race_major" and remove rows with "OtherUnknown" from the data
  if (str == "race_major" || var_1 == "race_major" || var_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if any of the variables or the stratification variable is "race_detail" and remove rows with commas from the data
  if (str == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Check if the stratification variable is "TPN" and remove rows with "Unknown" from the data
  if (str == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  dodge <- position_dodge(width = 0.8)  # Set the dodge width for positioning the violin and box plots
  
  ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2), fill = get(str))) +
    geom_violin(adjust = 10, position = dodge) +  # Add violin plots with specified adjustment and dodge position
    geom_boxplot(width = 0.1, position = dodge) +  # Add box plots with specified width and dodge position
    theme_light() +                            # Use a light theme
    labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
         y = variable_info$varShow[variable_info$variables == var_2]) +  # Label for the y-axis
    stat_summary(fun.y = mean, geom = "point", color = "Blue", shape = 19,position=position_dodge(width=0.80)) + #Add a data point to represent mean
    scale_fill_discrete(name = variable_info$varShow[variable_info$variables == str])  # Legend title for the fill color
  
  
  #If-else block to add mean data points
  if(v2_type == "continuous"){
    
    means <- aggregate(sample_info[[var_2]] ~  sample_info[[var_1]]+sample_info[[str]], sample_info, mean)
    colnames(means) <- c(variable_info$variables[variable_info$variables == var_1],
                        variable_info$variables[variable_info$variables == str],
                        "m")
    # Round up every value in column m to the next 2 decimals
    means$m <- round(means$m, 2)
    ccaplot <- ccaplot + geom_text(data = means, aes(label = m, y = m + 0.08),position=position_dodge(width=0.80),hjust=1.35)
    
  }else{
    
    means <- aggregate(sample_info[[var_1]] ~  sample_info[[var_2]]+sample_info[[str]], sample_info, mean)
    colnames(means) <- c(variable_info$variables[variable_info$variables == var_2],
                         variable_info$variables[variable_info$variables == str],
                         "m")
    # Round up every value in column m to the next 2 decimals
    means$m <- round(means$m, 2)
    ccaplot <- ccaplot + geom_text(data = means, aes(label = m, x = m + 0.08),position=position_dodge(width=0.80),vjust=1.50)
    
  }
  
  return(ccaplot)  # Return the combined box plot and violin plot
}



# Function to generate a box plot with one continuous variable, one categorical variable, and two stratification variables
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
#str_1 - Stratification variable One as selected by the user
#str_2 - Stratification variable Two as selected by the user
#v1_type - Type of variable one as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable one
#v2_type - Type of variable two as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable two
plotConCategoricalTwoStrBoxAndViolin <- function(var_1, var_2, str_1, str_2,v1_type,v2_type) {
  
  # Check if any of the variables or the stratification variables are "race_major" and remove rows with "OtherUnknown" from the data
  if (str_1 == "race_major" || str_2 == "race_major" || var_1 == "race_major" || var_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if any of the variables or the stratification variables are "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || str_2 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Check if any of the stratification variables are "TPN" and remove rows with "Unknown" from the data
  if (str_1 == "TPN" || str_2 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  dodge <- position_dodge(width = 0.8)  # Set the dodge width for positioning the violin and box plots
  ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2), fill = get(str_1))) +
    geom_violin(adjust = 10, position = dodge) +  # Add violin plots with specified adjustment and dodge position
    geom_boxplot(width = 0.1, position = dodge) +  # Add box plots with specified width and dodge position
    facet_wrap(~get(str_2), ncol = 1) +           # Facet the plots by the second stratification variable
    theme_light() +                            # Use a light theme
    labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
         y = variable_info$varShow[variable_info$variables == var_2]) +  # Label for the y-axis
    stat_summary(fun.y = mean, geom = "point", color = "Blue", shape = 19,position=position_dodge(width=0.80)) + #Add a data point to represent mean
    scale_fill_discrete(name = variable_info$varShow[variable_info$variables == str_1])  # Legend title for the fill color
  
  
  #If-else block to add mean data points
  if(v2_type == "continuous"){
    
    means <- aggregate(sample_info[[var_2]] ~  sample_info[[var_1]]+sample_info[[str_1]]+sample_info[[str_2]], sample_info, mean)
    colnames(means) <- c(variable_info$variables[variable_info$variables == var_1],
                         variable_info$variables[variable_info$variables == str_1],
                         variable_info$variables[variable_info$variables == str_2],
                         "m")
    # Round up every value in column m to the next 2 decimals
    means$m <- round(means$m, 2)
    ccaplot <- ccaplot + geom_text(data = means, aes(label = m, y = m + 0.08),position=position_dodge(width=0.80),hjust=1.35)
    
  }else{
    
    means <- aggregate(sample_info[[var_1]] ~  sample_info[[var_2]]+sample_info[[str_1]]+sample_info[[str_2]], sample_info, mean)
    colnames(means) <- c(variable_info$variables[variable_info$variables == var_2],
                         variable_info$variables[variable_info$variables == str_1],
                         variable_info$variables[variable_info$variables == str_2],
                         "m")
    # Round up every value in column m to the next 2 decimals
    means$m <- round(means$m, 2)
    ccaplot <- ccaplot + geom_text(data = means, aes(label = m, x = m + 0.08),position=position_dodge(width=0.80),vjust=1.50)
    
  }
  
  return(ccaplot)  # Return the combined box plot and violin plot
}



# Function to generate a box plot with one continuous variable, one categorical variable, and two stratification variables
#var_1 - Variable One as selected by the user
#var_2 - Variable Two as selected by the user
#str_1 - Stratification variable One as selected by the user
#str_2 - Stratification variable Two as selected by the user
#v1_type - Type of variable one as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable one
#v2_type - Type of variable two as retrieved from the variable_info data frame in parameters.R based on the variable selected for variable two
plotConCategoricalTwoStrBoxAndViolinAlt <- function(var_1, var_2, str_1, str_2,v1_type,v2_type) {
  
  # Check if any of the variables or the stratification variables are "race_major" and remove rows with "OtherUnknown" from the data
  if (str_1 == "race_major" || str_2 == "race_major" || var_1 == "race_major" || var_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if any of the variables or the stratification variables are "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || str_2 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Check if any of the stratification variables are "TPN" and remove rows with "Unknown" from the data
  if (str_1 == "TPN" || str_2 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  dodge <- position_dodge(width = 0.8)  # Set the dodge width for positioning the violin and box plots
  ccaplot <- ggplot(sample_info, aes(get(var_1), get(var_2), fill = get(str_2))) +
    geom_violin(adjust = 10, position = dodge) +  # Add violin plots with specified adjustment and dodge position
    geom_boxplot(width = 0.1, position = dodge) +  # Add box plots with specified width and dodge position
    facet_wrap(~get(str_1), ncol = 1) +           # Facet the plots by the first stratification variable
    theme_light() +                            # Use a light theme
    labs(x = variable_info$varShow[variable_info$variables == var_1],   # Label for the x-axis
         y = variable_info$varShow[variable_info$variables == var_2]) +  # Label for the y-axis
    stat_summary(fun.y = mean, geom = "point", color = "Blue", shape = 19,position=position_dodge(width=0.80)) + #Add a data point to represent mean
    scale_fill_discrete(name = variable_info$varShow[variable_info$variables == str_2])  # Legend title for the fill color
  
  
  #If-else block to add mean data points
  if(v2_type == "continuous"){
    
    means <- aggregate(sample_info[[var_2]] ~  sample_info[[var_1]]+sample_info[[str_1]]+sample_info[[str_2]], sample_info, mean)
    colnames(means) <- c(variable_info$variables[variable_info$variables == var_1],
                         variable_info$variables[variable_info$variables == str_1],
                         variable_info$variables[variable_info$variables == str_2],
                         "m")
    # Round up every value in column m to the next 2 decimals
    means$m <- round(means$m, 2)
    ccaplot <- ccaplot + geom_text(data = means, aes(label = m, y = m + 0.08),position=position_dodge(width=0.80),hjust=1.35)
    
  }else{
    
    means <- aggregate(sample_info[[var_1]] ~  sample_info[[var_2]]+sample_info[[str_1]]+sample_info[[str_2]], sample_info, mean)
    colnames(means) <- c(variable_info$variables[variable_info$variables == var_2],
                         variable_info$variables[variable_info$variables == str_1],
                         variable_info$variables[variable_info$variables == str_2],
                         "m")
    # Round up every value in column m to the next 2 decimals
    means$m <- round(means$m, 2)
    ccaplot <- ccaplot + geom_text(data = means, aes(label = m, x = m + 0.08),position=position_dodge(width=0.80),vjust=1.50)
    
  }
  
  return(ccaplot)  # Return the combined box plot and violin plot
}


