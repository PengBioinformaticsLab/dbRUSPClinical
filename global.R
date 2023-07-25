#R script to load the dataset and all the functions

load("data/sampleinfo.RData")

sample_info <- data.frame(sample_info)


source("functions.R")
source("parameters.R")
source("plotBothContinuous.R")
source("plotBothCategorical.R")
source("plotConCat.R")

#Method to oscillate between lm and gam methods for correlation
mymethod <- function(formula, data, weights, ...) {
  if(nrow(data) < 5) return(lm(y ~ x, data = data, ...))
  else  return(mgcv::gam(y ~ s(x, bs = "cs", k = 3), data = data, ...))
}