#R script to generate plots when both the variables are continuous

#load the required packages
library(ggplot2)
library(patchwork)


# Function to generate a correlation plot with two continuous variables and no stratification variables
plotBothContinuousNoStr <- function(var_1, var_2, dots, xscale, yscale, ci) {
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Check if 'dots' parameter is TRUE (to show individual data points as dots) or FALSE (do not show data points)
  if (dots) {
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with data points and a confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_point() +                  # Add data points as dots
        geom_smooth() +                 # Add a smoothed line
        theme_light() +                 # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else {
      # Plot the correlation using ggplot2 with data points but no confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_point() +                  # Add data points as dots
        geom_smooth(se = FALSE) +       # Add a smoothed line without a confidence interval
        theme_light() +                 # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot
    
  } else {
    # If 'dots' parameter is FALSE, skip adding data points and proceed to plot only the correlation line
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with only a confidence interval (no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_smooth() +                 # Add a smoothed line with confidence interval
        theme_light() +                 # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with only the correlation line (no data points or confidence interval)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_smooth(se = FALSE) +       # Add a smoothed line without a confidence interval
        theme_light() +                 # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot
  }
}



# Function to generate a correlation plot with two continuous variables and no stratification variables and no dots
plotBothContinuousNoStrNoDots <- function(var_1, var_2) {
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  # Create data vectors for the two continuous variables
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "pairwise.complete.obs")
  
  # Plot the correlation using ggplot2 with only the correlation line (no data points)
  cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
    geom_smooth() +                 # Add a smoothed line without a confidence interval
    theme_light() +                 # Use a light theme
    labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
         x = variable_info$varShow[variable_info$variables == var_1], 
         y = variable_info$varShow[variable_info$variables == var_2]) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(cplot)  # Return the correlation plot
}


# Function to generate a correlation plot with two continuous variables and one stratification variable
plotBothContinuousOneStrColor <- function(var_1, var_2, str_1, dots, xscale, yscale, ci) {
  
  # Check if the stratification variable is "race_major" and remove rows with "OtherUnknown"
  if (str_1 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if the stratification variable is "TPN" and remove rows with "Unknown"
  if (str_1 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  if (dots) {
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with data points, colored by the stratification variable, and a confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(method = mymethod) +                         # Add a smoothed line
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with data points, colored by the stratification variable, but no confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot with data points and colors
    
  } else {
    # If 'dots' parameter is FALSE, skip adding data points and proceed to plot only the correlation line
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with only the correlation line and a confidence interval (no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_smooth(method = mymethod) +                         # Add a smoothed line with confidence interval
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with only the correlation line (no data points or confidence interval)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot without data points but with colors
  }
}



# Function to generate a correlation plot with two continuous variables and one stratification variable using facet_wrap
plotBothContinuousOneStrFacet <- function(var_1, var_2, str_1, dots, xscale, yscale, ci) {
  
  # Check if the stratification variable is "race_major" and remove rows with "OtherUnknown"
  if (str_1 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if the stratification variable is "TPN" and remove rows with "Unknown"
  if (str_1 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  if (dots) {
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with data points, colored by the stratification variable, and a confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(method = mymethod) +                         # Add a smoothed line
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with data points, colored by the stratification variable, but no confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot with data points, colors, and facets
    
  } else {
    # If 'dots' parameter is FALSE, skip adding data points and proceed to plot only the correlation line
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with only the correlation line and a confidence interval (no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_smooth(method = mymethod) +                         # Add a smoothed line with confidence interval
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with only the correlation line (no data points or confidence interval)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2))) +
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot without data points but with facets
  }
}



# Function to generate a correlation plot with two continuous variables and two stratification variables using facet_wrap
plotBothContinuoustwostr <- function(var_1, var_2, str_1, str_2, dots, xscale, yscale, ci) {
  
  # Check if either of the stratification variables is "race_major" and remove rows with "OtherUnknown"
  if (str_1 == "race_major" || str_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if either of the stratification variables is "TPN" and remove rows with "Unknown"
  if (str_1 == "TPN" || str_2 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || str_2 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  if (dots) {
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with data points, colored by the first stratification variable, and a confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(method = mymethod) +                         # Add a smoothed line
        facet_wrap(~get(str_2)) +               # Separate plots for each level of the second stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with data points, colored by the first stratification variable, but no confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_wrap(~get(str_2)) +               # Separate plots for each level of the second stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot with data points, colors, and facets
    
  } else {
    # If 'dots' parameter is FALSE, skip adding data points and proceed to plot only the correlation line
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with only the correlation line and a confidence interval (no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_smooth(method = mymethod) +                         # Add a smoothed line with confidence interval
        facet_wrap(~get(str_2)) +               # Separate plots for each level of the second stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with only the correlation line (no data points or confidence interval)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_1))) +
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_wrap(~get(str_2)) +               # Separate plots for each level of the second stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_1]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot without data points but with facets
  }
}



# Function to generate a correlation plot with two continuous variables and two stratification variables using an alternative order of stratification
plotBothContinuoustwostralt <- function(var_1, var_2, str_1, str_2, dots, xscale, yscale, ci) {
  
  # Check if either of the stratification variables is "race_major" and remove rows with "OtherUnknown"
  if (str_1 == "race_major" || str_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if either of the stratification variables is "TPN" and remove rows with "Unknown"
  if (str_1 == "TPN" || str_2 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || str_2 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  if (dots) {
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with data points, colored by the second stratification variable, and a confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(method = mymethod) +                         # Add a smoothed line
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the first stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with data points, colored by the second stratification variable, but no confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the first stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot with data points, colors, and facets
    
  } else {
    # If 'dots' parameter is FALSE, skip adding data points and proceed to plot only the correlation line
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with only the correlation line and a confidence interval (no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_smooth(method = mymethod) +                         # Add a smoothed line with confidence interval
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the first stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with only the correlation line (no confidence interval and no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_wrap(~get(str_1)) +               # Separate plots for each level of the first stratification variable
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        scale_color_discrete(name = variable_info$varShow[variable_info$variables == str_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot without data points but with facets
  }
}



# Function to generate a correlation plot with two continuous variables and two stratification variables with facets for both stratification variables
plotBothContinuoustwostrfacet <- function(var_1, var_2, str_1, str_2, dots, xscale, yscale, ci) {
  
  # Check if either of the stratification variables is "race_major" and remove rows with "OtherUnknown"
  if (str_1 == "race_major" || str_2 == "race_major") {
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  # Check if either of the stratification variables is "TPN" and remove rows with "Unknown"
  if (str_1 == "TPN" || str_2 == "TPN") {
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  # Check if any of the variables is "race_detail" and remove rows with commas from the data
  if (str_1 == "race_detail" || str_2 == "race_detail" || var_1 == "race_detail" || var_2 == "race_detail") {
    sample_info <- sample_info[!grepl(",", sample_info$race_detail),]
  }
  
  if (dots) {
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with data points, colored by the second stratification variable, and a confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(method = mymethod) +                         # Add a smoothed line
        facet_grid(get(str_1) ~ get(str_2)) +    # Separate plots for each combination of levels from str_1 and str_2
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with data points, colored by the second stratification variable, but no confidence interval
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_point() +                          # Add data points as dots
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_grid(get(str_1) ~ get(str_2)) +    # Separate plots for each combination of levels from str_1 and str_2
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot with data points, colors, and facets
    
  } else {
    # If 'dots' parameter is FALSE, skip adding data points and proceed to plot only the correlation line
    
    # Create data vectors for the two continuous variables
    x <- sample_info[[var_1]]  # First variable
    y <- sample_info[[var_2]]  # Second variable
    
    # Calculate the correlation coefficient, ignoring missing values
    cor_coef <- cor(x, y, use = "pairwise.complete.obs")
    
    # Check if 'ci' parameter is TRUE (to add confidence interval) or FALSE (do not add confidence interval)
    if (ci) {
      # Plot the correlation using ggplot2 with only the correlation line and a confidence interval (no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_smooth(method = mymethod) +                         # Add a smoothed line with confidence interval
        facet_grid(get(str_1) ~ get(str_2)) +    # Separate plots for each combination of levels from str_1 and str_2
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      # Plot the correlation using ggplot2 with only the correlation line (no confidence interval and no data points)
      cplot <- ggplot(sample_info, aes(get(var_1), get(var_2), color = get(str_2))) +
        geom_smooth(se = FALSE,method = mymethod) +               # Add a smoothed line without a confidence interval
        facet_grid(get(str_1) ~ get(str_2)) +    # Separate plots for each combination of levels from str_1 and str_2
        theme_light() +                         # Use a light theme
        labs(title = paste("Correlation:", format(round(cor_coef, 3), nsmall = 3)), 
             x = variable_info$varShow[variable_info$variables == var_1], 
             y = variable_info$varShow[variable_info$variables == var_2]) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    # Check if the 'xscale' and/or 'yscale' parameters are TRUE (logarithmic scale) or FALSE (linear scale)
    if (xscale && !yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10")  # Apply logarithmic scale to the x-axis
    } else if (!xscale && yscale) {
      cplot <- cplot + scale_y_continuous(trans = "log10")  # Apply logarithmic scale to the y-axis
    } else if (xscale && yscale) {
      cplot <- cplot + scale_x_continuous(trans = "log10") +  # Apply logarithmic scale to both axes
        scale_y_continuous(trans = "log10")
    } else {
      cplot <- cplot  # Use linear scale for both axes
    }
    
    return(cplot)  # Return the correlation plot without data points but with facets
  }
}

