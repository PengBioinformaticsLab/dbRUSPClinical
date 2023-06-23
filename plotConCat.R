library(ggplot2)

plotConCat <- function(var_1, var_2, str_1, str_2){
  #cplot <- plot(sample_info[[var_1]],sample_info[[var_2]], xlab = var_1, ylab = var_2, main = "Correlation plot")
  
  # Create data vectors
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "complete.obs")
  
  # Plot the correlation using ggplot2
  cplot <- ggplot(sample_info, aes(var_1,var_2)) +
    geom_point() +
    geom_smooth() +
    theme_light() +
    labs(title = paste("Correlation:", cor_coef), x = var_1, y = var_2)
  
  return(cplot)
}