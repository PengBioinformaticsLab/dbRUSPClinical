library(ggplot2)

plotBothContinuous <- function(var_1, var_2, str_1, str_2){
  cplot <- plot(sample_info[[var_1]],sample_info[[var_2]], xlab = var_1, ylab = var_2, main = "Correlation plot")
  return(cplot)
}